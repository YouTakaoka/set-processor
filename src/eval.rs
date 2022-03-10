mod word_and_operator;

pub use self::word_and_operator::*;

#[derive(Clone)]
pub struct Bind {
    pub identifier: String,
    pub value: Word,
}

fn substitute(word: &Word, bindv: &Vec<Bind>) -> Result<Option<Word>, String> {
    if let Word::Identifier(identifier) = word {
        for bind in bindv {
            if bind.identifier == identifier.clone() {
                return Ok(Some(bind.value.clone()));
            }
        }
        return Err(format!("Parse error: Undefined word: {}", word.to_string()))
    }
    return Ok(None);
}

fn setlist_from_frozen(fwl: FrozenWordList, bindv: &Vec<Bind>) -> Result<SetList, String> {
    if !fwl.bound_is("{", "}") {
        panic!("setlist_from_frozen: Irregal bound: {}", display_bound(fwl.get_bound()));
    }

    if fwl.is_empty() {
        return Ok(SetList::new(Vec::new()))
    }

    let mut wv = fwl.get_contents();
    let mut contents: Vec<Set> = Vec::new();

    while let Some(i) = Word::find_word(&wv, Word::Symbol(",")) {
        let (wv1, wv2) = split_drop(&wv, i, i);

        let fwl1 = FrozenWordList::from_wordv(wv1, None)?;
        match eval(fwl1, bindv)? {
            (Word::Set(set), _) => contents.push(set),
            _ => return Err("Type error: Non-set object found in {} symbol.".to_string()),
        }

        wv = wv2;
    }

    let fwl1 = FrozenWordList::from_wordv(wv, None)?;
    match eval(fwl1, bindv)? {
        (Word::Set(set), _) => contents.push(set),
        _ => return Err("Type error: Non-set object found in {} symbol.".to_string()),
    }

    return Ok(SetList::new(contents));
}

fn set_from_frozen(fwl: FrozenWordList, bindv: &Vec<Bind>) -> Result<Set, String> {
    let sl = setlist_from_frozen(fwl, bindv)?;
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

fn apply_function(f: Function, fwl: FrozenWordList, bv: &Vec<Bind>) -> Result<Word, String> {
    if !fwl.bound_is("(", ")") {
        panic!("Word '(' must follow just after a function.");
    }

    let mut wv = Vec::new();
    let mut contents = fwl.get_contents();
    while let Some(i) = Word::find_word(&contents, Word::Symbol(",")) {
        let (wv1, wv2) = split_drop(&contents, i, i);
        contents = wv2;
        let fwl1 = FrozenWordList::from_wordv(wv1, None)?;
        let (word1, _) = eval(fwl1, bv)?;
        wv.push(word1);
    }

    let fwl1 = FrozenWordList::from_wordv(contents, None)?;
    let (word1, _) = eval(fwl1, bv)?;
    wv.push(word1);

    if wv.len() == 1 && wv[0] == Word::Null {
        wv = vec![];
    }

    let word = f.apply(wv);
    return word;
}

fn funcgen(identifier: String, wv: &Vec<Word>) -> Result<Word, String> {
    return Ok(Word::Null); //tofix
}

fn parse_funcdef(identifier: String, wv: &Vec<Word>) -> Result<(Signature, Vec<String>, Vec<Word>), String> {
    if let Some((wv_types, wv_other)) = Word::Symbol(";").split(wv) {
        if let Some((wv_argst, wv_rett)) = Word::Symbol("->").split(&wv_types) {
            let mut argst = Vec::new();
            for w in Word::Symbol(",").explode_each(&wv_argst, "Syntax error.")? {
                let t = w.to_type("Type error: Type name expected.")?;
                argst.push(t);
            }

            if wv_rett.len() != 1 {
                return Err("Syntax error.".to_string());
            }
            let rett = wv_rett[0].to_type("Type error: Type name expected.")?;
            let sig = Signature::new(argst, rett);

            if let Some((wv_args, wv_ret)) = Word::Symbol("->").split(&wv_other) {
                return Ok(()); //tofix
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

fn eval(fwl: FrozenWordList, bv: &Vec<Bind>) -> Result<(Word, Vec<Bind>), String> {
    let bound = fwl.get_bound().clone();
    if fwl.is_empty() {
        return Ok((Word::Null, bv.clone()));
    }

    let mut bindv = bv.clone();
    
    // 先頭がletキーワードだった場合の処理
    if let Some(Word::Keyword("let")) = fwl.get(0) {
        if fwl.len() < 4 {
            return Err("Parse error: 'let' statement is too short.".to_string());
        }

        if let Some(Word::Symbol("=")) = fwl.get(2) {
            let word1 = fwl.get(1).unwrap();
            if let Word::Identifier(identifier) = &word1 {
                for bind in &bindv {
                    if bind.identifier == identifier.clone() {
                        return Err(format!("Name error: Token '{}' is already reserved as identifier.", identifier.clone()));
                    }
                }
                
                let mut wordv = fwl.get_contents();
                let (word, _) = eval(FrozenWordList::from_wordv(wordv.split_off(3), bound)?, &bindv)?;
                let mut bindv_new = bindv.clone();
                bindv_new.push(Bind {
                    identifier: identifier.clone(),
                    value: word.clone(),
                });
                return Ok((word, bindv_new));
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
        let (word1, bindv1) = eval(FrozenWordList::from_wordv(wordv_if, bound.clone())?, &bindv)?;

        match word1 {
            Word::Bool(b) => { // 評価結果がbool型だった場合
                if b { // 条件式==trueの場合
                    let (w, _) = eval(FrozenWordList::from_wordv(wordv_then, bound)?, &bindv1)?;
                    return Ok((w, bindv));
                } else { // 条件式==falseの場合
                    let (w, _) = eval(FrozenWordList::from_wordv(wordv_else, bound)?, &bindv1)?;
                    return Ok((w, bindv));
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
            for bind in &bindv {
                if bind.identifier == identifier.clone() {
                    return Err(format!("Name error: Token '{}' is already reserved as identifier.", identifier.clone()));
                }
            }

            let (_, wv1) = split_drop(&fwl.get_contents(), 2, 2);
            let f = funcgen(identifier.clone(), &wv1)?;
            bindv.push(Bind {identifier: identifier, value: f.clone()});
            return Ok((f, bindv));
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
        if let Some(t_ret) = substitute(&word, &bindv)? {
            contents[i] = t_ret;
        }
    }

    // 関数処理
    // 注: 括弧処理より前にやること！
    // 注: ループではなく再帰で処理すること！(オペレータや関数を返す関数があり得るため)
    // 先に全部Functionで置き換えてしまう(関数を引数にとる関数やオペレータがあり得るため)
    for i in 0..contents.len() {
        if let Word::Function(f) = &contents[i] {
            if let Some(Word::Frozen(fwl)) = contents.get(i+1) {
                if fwl.bound_is("(", ")") {
                    let word = apply_function(f.clone(), fwl.clone(), &bindv)?;
                    let contents_new = subst_range(&contents, i, i + 1, word);
                    let fwl_new = FrozenWordList::from_wordv(contents_new, bound)?;
                    return eval(fwl_new, &bindv);
                }
            }
        }
    }

    // 括弧処理
    while let Some(i) = find_frozen(&contents) {
        match contents[i].clone() {
            Word::Frozen(fwl1) => {
                if fwl1.bound_is("(", ")") {
                    let (word, bindv1) = eval(fwl1, &bindv)?;
                    contents[i] = word;
                    bindv = bindv1;
                } else if fwl1.bound_is("{", "}") { // Setの場合
                    let set = set_from_frozen(fwl1, &bindv)?;
                    contents[i] = Word::Set(set);
                }
            },
            word => panic!("eval: Function find_frozen() brought index of non-FrozenWord: {}", word.to_string()),
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
                return eval(FrozenWordList::from_wordv(wv1, bound)?, &bindv);
            },
            Operator::UnaryOp(unop) => {
                if wv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let t = wv2.remove(0);
                let t_res = unop.apply(t)?;
                wv1.push(t_res);
                wv1.append(&mut wv2);
                return eval(FrozenWordList::from_wordv(wv1, bound)?, &bindv);
            },
        }
    }

    // トークン列が長さ1ならそのまま返す
    if contents.len() == 1 {
        return Ok((contents[0].clone(), bindv));
    } else {
        return Err("Parse error.".to_string());
    }
}

pub fn eval_string(s: &String, bindv: &Vec<Bind>) -> Result<(Word, Vec<Bind>), String> {
    return eval(FrozenWordList::from_string(s)?, bindv);
}
