mod word_and_operator;

pub use self::word_and_operator::*;
use std::fmt;

#[derive(Clone)]
pub struct Bind<T: Clone> {
    map: std::collections::HashMap<String, T>,
    funcmap: std::collections::HashMap<String, T>,
}

impl Bind<Word> {
    pub fn to_typebind(&self) -> Bind<WordType> {
        let mut map_new = std::collections::HashMap::new();
        let mut funcmap_new = std::collections::HashMap::new();

        for (s, w) in self.map.clone() {
            map_new.insert(s, w.get_type());
        }

        for (s, w) in self.funcmap.clone() {
            funcmap_new.insert(s, w.get_type());
        }

        return Bind {map: map_new, funcmap: funcmap_new};
    }
}

impl<T: Clone> Bind<T> {
    pub fn new() -> Self {
        return Self {
            map: std::collections::HashMap::new(),
            funcmap: std::collections::HashMap::new(),
        }
    }

    pub fn get(&self, key: &String) -> Option<T> {
        if let Some(w) = self.map.get(key) {
            return Some(w.clone());
        } else if let Some(w) = self.funcmap.get(key) {
            return Some(w.clone());
        } else {
            return None;
        }
    }

    pub fn insert(self: &mut Self, key: String, val: T) {
        self.map.insert(key, val);
    }

    pub fn insert_func(self: &mut Self, key: String, val: T) {
        self.funcmap.insert(key, val);
    }

    pub fn func_only(&self) -> Self {
        return Self {
            map: std::collections::HashMap::new(),
            funcmap: self.funcmap.clone(),
        }
    }
}

fn substitute<T: Clone + WordKind<T> + fmt::Display + PartialEq>(word: &T, bindm: Bind<T>) -> Result<Option<T>, String> {
    if let Ok(id) = word.to_identifier("") {
        if let Some(w) = bindm.get(&id) {
            return Ok(Some(w.clone()));
        }
        // Identifierにも関わらずbindmになければError
        return Err(format!("Parse error: Undefined token: {}", word.to_string()))
    }
    return Ok(None);
}

fn setlist_from_frozen<T: Clone + WordKind<T> + PartialEq + fmt::Display>(fwl: FrozenWordList<T>, bindm: &Bind<T>) -> Result<SetList, String> {
    if !fwl.env_is(Env::Set) {
        panic!("setlist_from_frozen: Irregal env: {}", fwl.get_env());
    }

    if fwl.is_empty() {
        return Ok(SetList::new(Vec::new()))
    }

    let wv = fwl.get_contents();
    let mut contents: Vec<Set> = Vec::new();

    for wv1 in T::from_symbol(",").explode(&wv) {
        let fwl1 = T::vec_to_frozen(wv1, &Env::Line)?;
        let (word, _) = eval(fwl1, bindm)?;
        let set = word.to_set("Type error: Non-set object found in {} symbol.")?;
        contents.push(set);
    }

    return Ok(SetList::new(contents));
}

fn set_from_frozen<T: Clone + WordKind<T> + PartialEq + fmt::Display>(fwl: FrozenWordList<T>, bindm: &Bind<T>) -> Result<Set, String> {
    let sl = setlist_from_frozen(fwl, bindm)?;
    return Ok(sl.uniquify());
}

fn find_frozen<T: WordKind<T> + Clone + fmt::Display + PartialEq>(wv: &Vec<T>) -> Option<usize> {
    for i in 0..wv.len() {
        if let Ok(_) = wv[i].to_frozen("") {
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

fn apply_function<T: Clone + WordKind<T> + PartialEq + fmt::Display>(f: Function<T>, fwl: FrozenWordList<T>, bm: &Bind<T>) -> Result<T, String> {
    if !fwl.env_is(Env::Bracket) {
        panic!("Token '(' must follow just after a function.");
    }

    let mut wv = Vec::new();
    let contents = fwl.get_contents();
    for wv1 in T::from_symbol(",").explode(&contents) {
        let fwl1 = T::vec_to_frozen(wv1, &Env::Line)?;
        let (word1, _) = eval(fwl1, bm)?;
        wv.push(word1);
    }

    if fwl.is_empty() {
        wv = vec![];
    }

    // Type check
    f.type_check(wv.clone())?;

    // Now apply function
    if T::is_wordtype() {
        return Ok(T::from_wordtype(f.sig().ret.clone()));
    } else {
        match f {
            Function::Preset(pf) => return pf.apply(wv),
            Function::User(uf) => return apply_user(uf, wv, bm),
        }
    }
}

fn apply_user<T: Clone + WordKind<T> + PartialEq + fmt::Display>(f: UserFunction<T>, argv: Vec<T>, bindm: &Bind<T>) -> Result<T, String> {
    let mut bm = bindm.func_only();
    for (x, arg) in f.get_xv().iter().zip(argv) {
        bm.insert(x.clone(), arg);
    }

    let fwl = T::vec_to_frozen(f.get_expr(), &Env::Line)?;
    let (ret, _) = eval(fwl, &bm)?;
    return Ok(ret);
}

fn parse_funcdef(identifier: Option<String>, wv: &Vec<Word>) -> Result<FuncDef, String> {
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
                let fdef = FuncDef{
                    name: identifier,
                    argtv: argst,
                    rett: rett,
                    argv: args,
                    expr: wv_ret
                };
                return Ok(fdef);
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

fn return_type_check<T: Clone>(wvv: &Vec<Vec<T>>, bindm: &Bind<T>) -> Result<(), String> {
    return Ok(()); //tofix
}

fn compile_defs<T: Clone + WordKind<T> + PartialEq + fmt::Display>(wvv: &Vec<Vec<T>>, bindm: &Bind<T>) -> Result<(Vec<Vec<T>>, Bind<T>), String> {
    let mut wvv_def = Vec::new();
    let mut wvv_other = Vec::new();
    for wv in wvv {
        if wv.get(0) == Some(&T::from_keyword("def")) {
            wvv_def.push(wv.clone());
        } else {
            wvv_other.push(wv.clone());
        }
    }

    if T::is_wordtype() {
        return_type_check(&wvv_def, bindm)?;
    }

    let mut bm = bindm.clone();
    for wv in wvv_def {
        let fwl1 = T::vec_to_frozen(wv.clone(), &Env::Line)?;
        let (_, bm1) = eval(fwl1, &bm)?;
        bm = bm1;
    }

    return Ok((wvv_other, bm));
}

fn eval_scope<T: Clone + WordKind<T> + PartialEq + fmt::Display>(wvv: &Vec<Vec<T>>, bm: &Bind<T>) -> Result<(T, Bind<T>), String> {
    let (wvv_other, bindm1) = compile_defs(&wvv, bm)?;
    let mut bindm = bindm1;

    let mut word = T::null();
    for wv in wvv_other {
        let fwl1 = T::vec_to_frozen(wv.clone(), &Env::Line)?;
        let (w1, bm1) = eval(fwl1, &bindm)?;
        word = w1;
        bindm = bm1;
    }
    return Ok((word, bindm));
}

fn eval_special(fwl: FrozenWordList<Word>) -> Result<Option<Frozen<Word>>, String> {
    if fwl.is_empty() {
        return Ok(None);
    }

    let env = fwl.get_env().clone();

    match env {
        Env::Line => (),
        Env::Scope | Env::Bracket => {
            let wvv = Word::Symbol("|").explode(&fwl.get_contents());
            return Ok(Some(Frozen::Scope(wvv)))
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
                // ここが中心部
                let mut wordv = fwl.get_contents();
                let expr = wordv.split_off(3);
                return Ok(Some(Frozen::LetExpr(LetExpr {identifier: identifier.clone(), expr: expr})));
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
        let (wordv_other, wv_else) = split_drop(&wordv, i_else, i_else);
        let (mut wv_if, wv_then) = split_drop(&wordv_other, i_then, i_then);
        wv_if.remove(0);

        return Ok(Some(Frozen::IfExpr(IfExpr {wv_if: wv_if, wv_then: wv_then, wv_else: wv_else})));
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
            let (_, wv1) = split_drop(&fwl.get_contents(), 2, 2);
            let f = parse_funcdef(Some(identifier.clone()), &wv1)?;
            return Ok(Some(Frozen::FuncDef(f)));
        } else {
            let w = fwl.get(1).unwrap();
            return Err(format!("Name error: Token '{}' cannot used as identifier.", w));
        }
    }

    return Ok(None);
}

fn eval<T: Clone + WordKind<T> + PartialEq + std::fmt::Display>
    (fwl: FrozenWordList<T>, bm: &Bind<T>) -> Result<(T, Bind<T>), String> {
    let env = fwl.get_env();
    let mut bindm = bm.clone();

    // Identifierトークンの置き換え処理
    // 注：関数処理より前にやること！
    let mut contents: Vec<T> = Vec::new();
    for word in fwl.get_contents() {
        if let Some(w_ret) = substitute(&word, bindm.clone())? {
            contents.push(w_ret);
        } else {
            contents.push(word);
        }
    }

    // 関数処理
    // 注: 括弧処理より前にやること！
    // 注: ループではなく再帰で処理すること！(オペレータや関数を返す関数があり得るため)
    // 先に全部PresetFunctionで置き換えてしまう(関数を引数にとる関数やオペレータがあり得るため)
    for i in 0..contents.len() {
        if let Ok(f) = contents[i].to_func("") {
            if let Some(w) = contents.get(i+1) {
                if let Ok(fwl) = w.to_frozen("") {
                    if fwl.env_is(Env::Bracket) {
                        let word = apply_function(f.clone(), fwl.clone(), &bindm)?;
                        let contents_new = subst_range(&contents, i, i + 1, word);
                        let fwl_new = T::vec_to_frozen(contents_new, &env)?;
                        return eval(fwl_new, &bindm);
                    }
                }
            }
        }
    }

    // 括弧処理
    while let Some(i) = find_frozen(&contents) {
        match contents[i].to_frozen("") {
            Ok(fwl1) => {
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
                        contents[i] = T::from_set(set);
                    },
                    _ => panic!("Env error. Env {} in {}.", fwl1.get_env(), fwl.get_env()),
                }
            },
            Err(word) => panic!("eval: PresetFunction find_frozen() brought index of non-FrozenWord: {}", word.to_string()),
        }
    }

    // Operator探し
    let mut index: Option<usize> = None;
    let mut priority = 11;

    for i in 0..contents.len() {
        let word: T = contents[i].clone();

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

        match contents[i].to_operator("Oops! Something is wrong.")? {
            Operator::BinaryOp(binop) => {
                let w1 :T = wv1.pop().ok_or("Parse error: Nothing before binary operator.".to_string())?;
                if wv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let w2 = wv2.remove(0);
                let w_res = binop.apply(w1, w2)?;
                wv1.push(w_res);
                wv1.append(&mut wv2);
                return eval(T::vec_to_frozen(wv1, &env)?, &bindm);
            },
            Operator::UnaryOp(unop) => {
                if wv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let w = wv2.remove(0);
                let w_res = unop.apply(w)?;
                wv1.push(w_res);
                wv1.append(&mut wv2);
                return eval(T::vec_to_frozen(wv1, &env)?, &bindm);
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

pub fn eval_line(s: &String, bindm: &Bind<Word>) -> Result<(Word, Bind<Word>), String> {
    return eval(FrozenWordList::from_string(s)?, bindm);
}

pub fn is_in<T: PartialEq>(x: T, vec: Vec<T>) -> bool {
    for v in vec {
        if v == x {
            return true;
        }
    }
    
    return false;
}

pub fn eval_main(string: &String) -> Result<(), String> {
    let sv: Vec<String> = string.split('\n').map(|s| s.to_string()).collect();
    if sv.len() == 0 {
        return Ok(());
    } else if sv.len() == 1 {
        let (w, _) = eval_line(&sv[0], &Bind::new())?;
        if let Word::PrintSignal(s) = w {
            println!("{}", s);
        }
        return Ok(())
    }

    let mut tvv: Vec<Vec<Token>> = Vec::new();
    let mut prev_tv: Vec<Token> = Token::tokenize(&sv[0])?;
    let mut concat_prev = false;
    for i in 1..sv.len() {
        let tv = Token::tokenize(&sv[i])?;
        if tv.len() == 0 {
            continue;
        }

        // concat_prevがtrueもしくはLINE_BEGIN_TOKENSで始まる行なら
        // 現在の行を前の行にくっつける
        if concat_prev || is_in(tv[0].clone(), LINE_BEGIN_TOKENS.to_vec()) {
            prev_tv.append(&mut tv.clone());
        } else {
            // くっつけないなら前の行はそれで終わりなのでpushする
            tvv.push(prev_tv);
            prev_tv = tv.clone();
        }

        concat_prev = false;
        if let Some(t) = tv.last() {
            if is_in(t.clone(), LINE_END_TOKENS.to_vec()) {
                // LINE_END_TOKENSで終わる行の場合
                concat_prev = true;
            }
        }
    }

    let mut wvv: Vec<Vec<Word>> = Vec::new();
    for tv in tvv {
        let fwl = FrozenWordList::from_tokenv(tv, Env::Line)?;
        wvv.push(fwl.get_contents());
    }

    let (wvv_other, bindm) = compile_defs(&wvv, &Bind::new())?;
    let mut bm = bindm.clone();
    for wv in wvv_other {
        let fwl = Word::vec_to_frozen(wv, &Env::Line)?;
        let (w1, bm1) = eval(fwl, &bm)?;
        bm = bm1;
        match w1 {
            Word::PrintSignal(s) => println!("{}", s),
            Word::ExitSignal => break,
            _ => (),
        }
    }

    return Ok(());
}
