mod word_and_operator;

pub use self::word_and_operator::*;

#[derive(Clone)]
pub struct Bind {
    map: std::collections::HashMap<String, Word>,
    funcmap: std::collections::HashMap<String, Word>,
}

impl Bind {
    pub fn new() -> Self {
        return Bind {
            map: std::collections::HashMap::new(),
            funcmap: std::collections::HashMap::new(),
        }
    }

    pub fn get(&self, key: &String) -> Option<Word> {
        if let Some(w) = self.map.get(key) {
            return Some(w.clone());
        } else if let Some(w) = self.funcmap.get(key) {
            return Some(w.clone());
        } else {
            return None;
        }
    }

    pub fn insert(self: &mut Self, key: String, val: Word) {
        self.map.insert(key, val);
    }

    pub fn insert_func(self: &mut Self, key: String, val: Word) {
        self.funcmap.insert(key, val);
    }

    pub fn func_only(&self) -> Self {
        return Self {
            map: std::collections::HashMap::new(),
            funcmap: self.funcmap.clone(),
        }
    }
}

fn substitute(word: &Word, bindm: Bind) -> Result<Option<Word>, String> {
    if let Word::Identifier(id) = word {
        if let Some(w) = bindm.get(id) {
            return Ok(Some(w.clone()));
        }
        // Identifierにも関わらずbindmになければError
        return Err(format!("Parse error: Undefined token: {}", word.to_string()))
    }
    return Ok(None);
}

fn setlist_from_frozen(fwl: FrozenWordList, bindm: &Bind) -> Result<SetList, String> {
    if !fwl.env_is(Env::Set) {
        panic!("setlist_from_frozen: Irregal env: {}", fwl.get_env());
    }

    if fwl.is_empty() {
        return Ok(SetList::new(Vec::new()))
    }

    let wv = fwl.get_contents();
    let mut contents: Vec<Set> = Vec::new();

    for wv1 in Word::Symbol(",").explode(&wv) {
        let fwl1 = FrozenWordList::from_wordv(wv1, Env::Line)?;
        match eval(fwl1, bindm)? {
            (Word::Set(set), _) => contents.push(set),
            _ => return Err("Type error: Non-set object found in {} symbol.".to_string()),
        }
    }

    return Ok(SetList::new(contents));
}

fn set_from_frozen(fwl: FrozenWordList, bindm: &Bind) -> Result<Set, String> {
    let sl = setlist_from_frozen(fwl, bindm)?;
    return Ok(sl.uniquify());
}

fn find_frozen(wv: &Vec<Word>) -> Option<usize> {
    for i in 0..wv.len() {
        if let Word::Frozen(_) = wv[i] {
            return Some(i);
        }
    }

    return None;
}

fn rewrite_error<T>(result: Result<T, String>, string: String) -> Result<T, String> {
    match result {
        Ok(val) => Ok(val),
        Err(_) => Err(string),
    }
}

fn apply_function(f: Function, fwl: FrozenWordList, bm: &Bind) -> Result<Word, String> {
    if !fwl.env_is(Env::Bracket) {
        panic!("Token '(' must follow just after a function.");
    }

    let mut wv = Vec::new();
    let contents = fwl.get_contents();
    for wv1 in Word::Symbol(",").explode(&contents) {
        let fwl1 = FrozenWordList::from_wordv(wv1, Env::Line)?;
        let (word1, _) = eval(fwl1, bm)?;
        wv.push(word1);
    }

    if wv.len() == 1 && wv[0] == Word::Null {
        wv = vec![];
    }

    // Type check
    f.type_check(wv.clone())?;

    // Now apply function
    match f {
        Function::Preset(pf) => return pf.apply(wv),
        Function::User(uf) => return apply_user(uf, wv, bm),
    }
}

fn apply_user(f: UserFunction, argv: Vec<Word>, bindm: &Bind) -> Result<Word, String> {
    let mut bm = bindm.func_only();
    for (x, arg) in f.get_xv().iter().zip(argv) {
        bm.insert(x.clone(), arg);
    }

    if let Some(name) = f.get_name() {
        bm.insert_func(name, Word::Function(Function::User(f.clone())));
    }

    let fwl = FrozenWordList::from_wordv(f.get_expr(), Env::Line)?;
    let (ret, _) = eval(fwl, &bm)?;
    return Ok(ret);
}

fn funcgen(identifier: Option<String>, wv: &Vec<Word>) -> Result<Word, String> {
    if let Some((wv_types, wv_other)) = Word::Symbol(";").split(wv) {
        if let Some((wv_argst, wv_rett)) = Word::Symbol("->").split(&wv_types) {
            // 返り値のSignatureをつくる
            let mut argst = Vec::new();
            for w in Word::Symbol(",").explode_each(&wv_argst, "Syntax error in the argument type part of function definition.")? {
                let t = w.to_type("Type error: Type name expected.")?;
                argst.push(t);
            }

            if wv_rett.len() != 1 {
                return Err("Syntax error in the return type part of function definition.".to_string());
            }
            let rett = wv_rett[0].to_type("Type error: Type name expected.")?;
            let sig = Signature::new(argst, rett);

            if let Some((wv_args, wv_ret)) = Word::Symbol("->").split(&wv_other) {
                // ここが中心部
                let mut args = Vec::new();
                for w in Word::Symbol(",").explode_each(&wv_args, "Syntax error in the arguments part of function definition.")? {
                    if let Word::Identifier(id) = w {
                        if Some(id.clone()) == identifier {
                            return Err(format!("Name error: PresetFunction name '{}' cannot used in arguments.",
                                                identifier.unwrap()));
                        }
                        args.push(id);
                    } else {
                        return Err(format!("Name error: Token '{}' cannot used in arguments.", w));
                    }
                }
                let uf = UserFunction::new(identifier, sig, args, wv_ret);
                return Ok(uf.to_word());
            } else {
                return Err("Syntax error: Token '->' not found in the main part of function definition.".to_string());
            }
        } else {
            return Err("Syntax error: Token '->' not found in the type declaration of function definition.".to_string());
        }
    } else {
        return Err("Syntax error: Token ';' not found in function definition.".to_string());
    }
}

fn return_type_check(wvv: &Vec<Vec<Word>>, bindm: &Bind) -> Result<(), String> {
    return Ok(()); //tofix
}

fn compile_defs(wvv: &Vec<Vec<Word>>, bindm: &Bind) -> Result<(Vec<Vec<Word>>, Bind), String> {
    let mut wvv_def = Vec::new();
    let mut wvv_other = Vec::new();
    for wv in wvv {
        if wv.get(0) == Some(&Word::Keyword("def")) {
            wvv_def.push(wv.clone());
        } else {
            wvv_other.push(wv.clone());
        }
    }

    return_type_check(&wvv_def, bindm)?;

    let mut bm = bindm.clone();
    for wv in wvv_def {
        let fwl1 = FrozenWordList::from_wordv(wv.clone(), Env::Line)?;
        let (_, bm1) = eval(fwl1, &bindm)?;
        bm = bm1;
    }

    return Ok((wvv_other, bm));
}

fn eval(fwl: FrozenWordList, bm: &Bind) -> Result<(Word, Bind), String> {
    if fwl.is_empty() {
        return Ok((Word::Null, bm.clone()));
    }

    let env = fwl.get_env().clone();
    let mut bindm = bm.clone();

    match env {
        Env::Line => (),
        Env::Scope | Env::Bracket => {
            let wvv = Word::Symbol("|").explode(&fwl.get_contents());
            let (wvv_other, bindm1) = compile_defs(&wvv, &bindm)?;
            bindm = bindm1;

            let mut word = Word::Null;
            for wv in wvv_other {
                let fwl1 = FrozenWordList::from_wordv(wv.clone(), Env::Line)?;
                let (w1, bm1) = eval(fwl1, &bindm)?;
                word = w1;
                bindm = bm1;
            }
            return Ok((word, bindm));
        },
        _ => panic!("Invalid Env type {} in eval() function.", env),
    }
    
    // 先頭がletキーワードだった場合の処理
    if let Some(Word::Keyword("let")) = fwl.get(0) {
        if fwl.len() < 4 {
            return Err("Parse error: 'let' statement is too short.".to_string());
        }

        if let Some(Word::Symbol("=")) = fwl.get(2) {
            let word1 = fwl.get(1).unwrap();
            if let Word::Identifier(identifier) = &word1 {
                if let Some(_) = bindm.get(identifier) {
                    return Err(format!("Name error: Token '{}' is already reserved as identifier.", identifier.clone()));
                }
                
                let mut wordv = fwl.get_contents();
                let (word, _) = eval(FrozenWordList::from_wordv(wordv.split_off(3), env)?, &bindm)?;
                bindm.insert(identifier.clone(), word.clone());
                return Ok((word, bindm));
            }

            // word1がIdentifierでなかった場合
            return Err(format!("Name error: Cannot use '{}' as identifier.", word1.to_string()));
        }

        // 2番目のトークンが'='でなかった場合
        return Err("Syntax error: Token '=' needed after 'let' keyword.".to_string());
    }

    // if文の処理
    if let Some(Word::Keyword("if")) = fwl.get(0) {
        // then節を探す(なければerror)
        let option_then = rewrite_error(Word::find_bracket(&fwl.get_contents(), Word::Keyword("if"), Word::Keyword("then")),
                                        "Syntax error: Keyword 'then' not found after 'if' keyword.".to_string())?;
        let (_, i_then) = option_then.unwrap();

        // else節を探す(なければerror)
        let option_else = rewrite_error(Word::find_bracket(&fwl.get_contents(), Word::Keyword("if"), Word::Keyword("else")),
                                        "Syntax error: Keyword 'else' not found after 'if' keyword.".to_string())?;
        let (_, i_else) = option_else.unwrap();

        // thenがelseより後ろならerror
        if i_then > i_else {
            return Err("Syntax error: Keyword 'else' found before 'then'.".to_string());
        }

        // if節、then節、else節に分解
        let wordv = fwl.get_contents();
        let (wordv_other, wordv_else) = split_drop(&wordv, i_else, i_else);
        let (mut wordv_if, wordv_then) = split_drop(&wordv_other, i_then, i_then);
        wordv_if.remove(0);
        
        // if節を評価
        let (word1, bindm1) = eval(FrozenWordList::from_wordv(wordv_if, env.clone())?, &bindm)?;

        match word1 {
            Word::Bool(b) => { // 評価結果がbool型だった場合
                if b { // 条件式==trueの場合
                    let (w, _) = eval(FrozenWordList::from_wordv(wordv_then, env)?, &bindm1)?;
                    return Ok((w, bindm));
                } else { // 条件式==falseの場合
                    let (w, _) = eval(FrozenWordList::from_wordv(wordv_else, env)?, &bindm1)?;
                    return Ok((w, bindm));
                }
            },
            // boolじゃなかったらError
            _ => return Err("Type error: Non-bool value returned by the 'if' expression.".to_string()),
        }
    }

    // def(関数定義)文の処理
    if let Some(Word::Keyword("def")) = fwl.get(0) {
        if fwl.get(2) != Some(Word::Symbol(":")) {
            return Err("Syntax error: Token ':' needed after 'def' token.".to_string());
        } else if fwl.get(2) == None {
            return Err("Parse error: 'def' statement is too short.".to_string());
        }

        if let Some(Word::Identifier(identifier)) = fwl.get(1) {
            // ここが中心部
            if let Some(_) = bindm.get(&identifier) {
                return Err(format!("Name error: Token '{}' is already reserved as identifier.", identifier.clone()));
            }

            let (_, wv1) = split_drop(&fwl.get_contents(), 2, 2);
            let f = funcgen(Some(identifier.clone()), &wv1)?;
            bindm.insert_func(identifier, f.clone());
            return Ok((f, bindm));
        } else {
            let w = fwl.get(1).unwrap();
            return Err(format!("Name error: Token '{}' cannot used as identifier.", w));
        }
    }

    // Identifierトークンの置き換え処理
    // 注：関数処理より前にやること！
    let mut contents: Vec<Word> = fwl.get_contents();
    for i in 0..contents.len() {
        let word = &contents[i];
        if let Some(t_ret) = substitute(&word, bindm.clone())? {
            contents[i] = t_ret;
        }
    }

    // 関数処理
    // 注: 括弧処理より前にやること！
    // 注: ループではなく再帰で処理すること！(オペレータや関数を返す関数があり得るため)
    // 先に全部PresetFunctionで置き換えてしまう(関数を引数にとる関数やオペレータがあり得るため)
    for i in 0..contents.len() {
        if let Word::Function(f) = &contents[i] {
            if let Some(Word::Frozen(fwl)) = contents.get(i+1) {
                if fwl.env_is(Env::Bracket) {
                    let word = apply_function(f.clone(), fwl.clone(), &bindm)?;
                    let contents_new = subst_range(&contents, i, i + 1, word);
                    let fwl_new = FrozenWordList::from_wordv(contents_new, env)?;
                    return eval(fwl_new, &bindm);
                }
            }
        }
    }

    // 括弧処理
    while let Some(i) = find_frozen(&contents) {
        match contents[i].clone() {
            Word::Frozen(fwl1) => {
                match fwl1.get_env() {
                    Env::Scope => {
                        let (word, _) = eval(fwl1, &bindm)?;
                        contents[i] = word;
                    },
                    Env::Bracket => {
                        let (word, bindm1) = eval(fwl1, &bindm)?;
                        contents[i] = word;
                        bindm = bindm1;
                    },
                    Env::Set => { // Setの場合
                        let set = set_from_frozen(fwl1, &bindm)?;
                        contents[i] = Word::Set(set);
                    },
                    _ => panic!("Env error. Env {} in {}.", fwl1.get_env(), fwl.get_env()),
                }
            },
            word => panic!("eval: PresetFunction find_frozen() brought index of non-FrozenWord: {}", word.to_string()),
        }
    }

    // Operator探し
    let mut index: Option<usize> = None;
    let mut priority = 11;

    for i in 0..contents.len() {
        let word: Word = contents[i].clone();

        // wordがOperatorかどうか調べる
        if let Ok(op) = word.to_operator(&"".to_string()) {
            // 優先順位を確認
            let priority1 = op.priority();

            // 優先順位が既存より高ければindexと優先順位を記憶
            if priority1 < priority {
                index = Some(i);
                priority = priority1;
            }
        }    
    }

    // Operator処理
    // 注: ループではなく再帰で処理すること！(オペレータや関数を返すOperatorがあり得るため)
    if let Some(i) = index {  // Operator見つかった
        let (mut wv1, mut wv2) = split_drop(&contents, i, i);

        match contents[i].to_operator(&"Oops! Something is wrong.".to_string())? {
            Operator::BinaryOp(binop) => {
                let t1:Word = wv1.pop().ok_or("Parse error: Nothing before binary operator.".to_string())?;
                if wv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let t2 = wv2.remove(0);
                let t_res = binop.apply(t1, t2)?;
                wv1.push(t_res);
                wv1.append(&mut wv2);
                return eval(FrozenWordList::from_wordv(wv1, env)?, &bindm);
            },
            Operator::UnaryOp(unop) => {
                if wv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let t = wv2.remove(0);
                let t_res = unop.apply(t)?;
                wv1.push(t_res);
                wv1.append(&mut wv2);
                return eval(FrozenWordList::from_wordv(wv1, env)?, &bindm);
            },
        }
    }

    // トークン列が長さ1ならそのまま返す
    if contents.len() == 1 {
        return Ok((contents[0].clone(), bindm));
    } else {
        return Err("Parse error.".to_string());
    }
}

pub fn eval_string(s: &String, bindm: &Bind) -> Result<(Word, Bind), String> {
    return eval(FrozenWordList::from_string(s)?, bindm);
}
