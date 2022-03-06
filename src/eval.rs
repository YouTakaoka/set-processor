mod token_and_operator;

pub use self::token_and_operator::*;

#[derive(Clone)]
pub struct Bind {
    pub identifier: String,
    pub value: Token,
}

fn substitute(token: &Token, bindv: &Vec<Bind>) -> Result<Option<Token>, String> {
    if let Token::IdentifierToken(identifier) = token {
        for bind in bindv {
            if bind.identifier == identifier.clone() {
                return Ok(Some(bind.value.clone()));
            }
        }
        return Err(format!("Undefined token: {}", token.to_string()))
    }
    return Ok(None);
}

fn setlist_from_frozen(ftl: FrozenTokenList, bindv: &Vec<Bind>) -> Result<SetList, String> {
    if !ftl.bound_is("{", "}") {
        panic!("setlist_from_frozen: Irregal bound: {}", display_bound(ftl.get_bound()));
    }

    if ftl.is_empty() {
        return Ok(SetList::new(Vec::new()))
    }

    let mut tv = ftl.get_contents();
    let mut contents: Vec<Set> = Vec::new();

    while let Some(i) = Token::find_token(&tv, Token::SymbolToken(",")) {
        let (tv1, tv2) = split_drop(&tv, i, i);

        let ftl1 = FrozenTokenList::from_tokenv(tv1, &None)?;
        match eval(ftl1, bindv)? {
            (Token::SetToken(set), _) => contents.push(set),
            _ => return Err("Type error: Non-set object found in {} symbol.".to_string()),
        }

        tv = tv2;
    }

    let ftl1 = FrozenTokenList::from_tokenv(tv, &None)?;
    match eval(ftl1, bindv)? {
        (Token::SetToken(set), _) => contents.push(set),
        _ => return Err("Type error: Non-set object found in {} symbol.".to_string()),
    }

    return Ok(SetList::new(contents));
}

fn set_from_frozen(ftl: FrozenTokenList, bindv: &Vec<Bind>) -> Result<Set, String> {
    let sl = setlist_from_frozen(ftl, bindv)?;
    return Ok(sl.uniquify());
}

fn find_frozen(tv: &Vec<Token>) -> Option<usize> {
    for i in 0..tv.len() {
        if let Token::FrozenToken(_) = tv[i] {
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

fn apply(f: Function, ftl: FrozenTokenList, bv: &Vec<Bind>) -> Result<Token, String> {
    if !ftl.bound_is("(", ")") {
        panic!("Token '(' must follow just after a function.");
    }

    let mut tv = Vec::new();
    let mut contents = ftl.get_contents();
    while let Some(i) = Token::find_token(&contents, Token::SymbolToken(",")) {
        let (tv1, tv2) = split_drop(&contents, i, i);
        contents = tv2;
        let ftl1 = FrozenTokenList::from_tokenv(tv1, &None)?;
        let (token1, _) = eval(ftl1, bv)?;
        tv.push(token1);
    }

    let ftl1 = FrozenTokenList::from_tokenv(contents, &None)?;
    let (token1, _) = eval(ftl1, bv)?;
    tv.push(token1);

    let token = f.apply(tv);
    return token;
}

fn eval(ftl: FrozenTokenList, bv: &Vec<Bind>) -> Result<(Token, Vec<Bind>), String> {
    let bound = &ftl.get_bound();
    if ftl.is_empty() {
        return Ok((Token::NullToken, bv.clone()));
    }

    let mut bindv = bv.clone();
    
    // 先頭がletキーワードだった場合の処理
    if let Some(Token::KeywordToken("let")) = ftl.get(0) {
        if ftl.len() < 4 {
            return Err("Parse error: 'let' statement is too short.".to_string());
        }

        if let Some(Token::SymbolToken("=")) = ftl.get(2) {
            let token1 = ftl.get(1).unwrap();
            if let Token::IdentifierToken(identifier) = &token1 {
                for bind in &bindv {
                    if bind.identifier == identifier.clone() {
                        return Err(format!("Token {} is already reserved as identifier.", identifier.clone()));
                    }
                }
                
                let mut tokenv = ftl.get_contents();
                let (token, _) = eval(FrozenTokenList::from_tokenv(tokenv.split_off(3), bound)?, &bindv)?;
                let mut bindv_new = bindv.clone();
                bindv_new.push(Bind {
                    identifier: identifier.clone(),
                    value: token.clone(),
                });
                return Ok((token, bindv_new));
            }

            // token1がIdentifierTokenでなかった場合
            return Err(format!("Cannot use '{}' as identifier.", token1.to_string()));
        }

        // 2番目のトークンが'='でなかった場合
        return Err("Parse error: Not found '=' token after 'let' keyword.".to_string());
    }

    // if文の処理
    if let Some(Token::KeywordToken("if")) = ftl.get(0) {
        // then節を探す(なければerror)
        let option_then = rewrite_error(Token::find_bracket(&ftl.get_contents(), Token::KeywordToken("if"), Token::KeywordToken("then")),
                                        "Keyword 'then' not found after 'if' keyword.".to_string())?;
        let (_, i_then) = option_then.unwrap();

        // else節を探す(なければerror)
        let option_else = rewrite_error(Token::find_bracket(&ftl.get_contents(), Token::KeywordToken("if"), Token::KeywordToken("else")),
                                        "Keyword 'else' not found after 'if' keyword.".to_string())?;
        let (_, i_else) = option_else.unwrap();

        // thenがelseより後ろならerror
        if i_then > i_else {
            return Err("Keyword 'else' found before 'then'.".to_string());
        }

        // if節、then節、else節に分解
        let tokenv = ftl.get_contents();
        let (tokenv_other, tokenv_else) = split_drop(&tokenv, i_else, i_else);
        let (mut tokenv_if, tokenv_then) = split_drop(&tokenv_other, i_then, i_then);
        tokenv_if.remove(0);
        
        // if節を評価
        let (token1, bindv1) = eval(FrozenTokenList::from_tokenv(tokenv_if, bound)?, &bindv)?;

        match token1 {
            Token::BoolToken(b) => { // 評価結果がbool型だった場合
                if b { // 条件式==trueの場合
                    return eval(FrozenTokenList::from_tokenv(tokenv_then, bound)?, &bindv1);
                } else { // 条件式==falseの場合
                    return eval(FrozenTokenList::from_tokenv(tokenv_else, bound)?, &bindv1);
                }
            },
            // boolじゃなかったらError
            _ => return Err("Type error: Non-bool value returned by the 'if' expression.".to_string()),
        }
    }

    // Identifierトークンの置き換え処理
    // 注：関数処理より前にやること！
    let mut contents: Vec<Token> = ftl.get_contents();
    for i in 0..contents.len() {
        let token = &contents[i];
        if let Some(t_ret) = substitute(&token, &bindv)? {
            contents[i] = t_ret;
        }
    }

    // 関数処理
    // 注: 括弧処理より前にやること！
    // 注: ループではなく再帰で処理すること！(オペレータや関数を返す関数があり得るため)
    // 先に全部FunctionTokenで置き換えてしまう(関数を引数にとる関数やオペレータがあり得るため)
    //todo

    // 括弧処理
    while let Some(i) = find_frozen(&contents) {
        match contents[i].clone() {
            Token::FrozenToken(ftl1) => {
                if ftl1.bound_is("(", ")") {
                    let (token, bindv1) = eval(ftl1, &bindv)?;
                    contents[i] = token;
                    bindv = bindv1;
                } else if ftl1.bound_is("{", "}") { // Setの場合
                    let set = set_from_frozen(ftl1, &bindv)?;
                    contents[i] = Token::SetToken(set);
                }
            },
            token => panic!("eval: Function find_frozen() brought index of non-FrozenToken: {}", token.to_string()),
        }
    }

    // Operator探し
    let mut index: Option<usize> = None;
    let mut priority = 11;

    for i in 0..contents.len() {
        let token: Token = contents[i].clone();

        // tokenがOperatorかどうか調べる
        if let Some(op) = Operator::from_token(token) {
            // OperatorだったらOperatorTokenに変換
            contents[i] = Token::OperatorToken(op.clone());

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
        let (mut tv1, mut tv2) = split_drop(&contents, i, i);

        match contents[i].to_operator(&"Oops! Something is wrong.".to_string())? {
            Operator::BinaryOp(binop) => {
                let t1:Token = tv1.pop().ok_or("Parse error: Nothing before binary operator.".to_string())?;
                if tv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let t2 = tv2.remove(0);
                let t_res = binop.apply(t1, t2)?;
                tv1.push(t_res);
                tv1.append(&mut tv2);
                return eval(FrozenTokenList::from_tokenv(tv1, bound)?, &bindv);
            },
            Operator::UnaryOp(unop) => {
                if tv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let t = tv2.remove(0);
                let t_res = unop.apply(t)?;
                tv1.push(t_res);
                tv1.append(&mut tv2);
                return eval(FrozenTokenList::from_tokenv(tv1, bound)?, &bindv);
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

pub fn eval_string(s: &String, bindv: &Vec<Bind>) -> Result<(Token, Vec<Bind>), String> {
    return eval(FrozenTokenList::from_string(s)?, bindv);
}
