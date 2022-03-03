mod token_and_operator;

pub use self::token_and_operator::*;

#[derive(Clone)]
pub struct Bind {
    pub identifier: String,
    pub value: Token,
}

fn substitute(token: &Token, bindv: Vec<Bind>) -> Result<Token, String> {
    if let Token::IdentifierToken(identifier) = token {
        for bind in bindv {
            if bind.identifier == identifier.clone() {
                return Ok(bind.value);
            }
        }
        return Err(format!("Undefined token: {}", token.to_string()))
    }
    return Ok(token.clone());
}

fn find_token(tokenv: &Vec<Token>, token: Token) -> Option<usize> {
    let mut i_ret: Option<usize> = None;
    for i in 0..tokenv.len() {
        if tokenv[i] == token {
            i_ret = Some(i);
        }
    }
    return i_ret;
}

fn setlist_from_frozen(ftl: FrozenTokenList, bindv: Vec<Bind>) -> Result<SetList, String> {
    if !ftl.bound_is("{", "}") {
        panic!("setlist_from_frozen: Irregal bound: {}", display_bound(ftl.get_bound()));
    }

    if ftl.is_empty() {
        return Ok(SetList::new(Vec::new()))
    }

    let mut tv = ftl.get_contents().to_vec();
    let mut contents: Vec<Set> = Vec::new();

    while let Some(i) = find_token(&tv, Token::SymbolToken(",")) {
        let tv1 = &tv[0..i].to_vec();
        let ftl1 = FrozenTokenList::from_tokenv(&tv1, &None)?;
        match eval(ftl1, bindv.clone())? {
            (Token::SetToken(set), _) => contents.push(set),
            _ => return Err("Type error: Non-set object found in {} symbol.".to_string()),
        }

        if tv.len() < i+2 {
            return Err("Parse error: Symbol ',' found at the end of curly brace.".to_string());
        }

        tv = tv[i+1..].to_vec();
    }

    let ftl1 = FrozenTokenList::from_tokenv(&tv, &None)?;
    match eval(ftl1, bindv)? {
        (Token::SetToken(set), _) => contents.push(set),
        _ => return Err("Type error: Non-set object found in {} symbol.".to_string()),
    }

    return Ok(SetList::new(contents));
}

fn set_from_frozen(ftl: FrozenTokenList, bindv: Vec<Bind>) -> Result<Set, String> {
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

fn eval(ftl: FrozenTokenList, bv: Vec<Bind>) -> Result<(Token, Vec<Bind>), String> {
    let bound = ftl.get_bound();
    if ftl.is_empty() {
        return Ok((Token::NullToken, bv));
    }

    let mut bindv = bv;
    
    // 先頭がletキーワードだった場合の処理
    if ftl.get(0).unwrap() == Token::KeywordToken("let") {
        if ftl.len() < 4 {
            return Err("Parse error: 'let' statement is too short.".to_string());
        }

        if ftl.get(2).unwrap() != Token::SymbolToken("=") {
            return Err("Parse error: Not found '=' token after 'let' keyword.".to_string())
        }

        let token1 = ftl.get(1).unwrap();
        if let Token::IdentifierToken(identifier) = &token1 {
            for bind in bindv.clone() {
                if bind.identifier == identifier.clone() {
                    return Err(format!("Token {} is already reserved as identifier.", identifier.clone()));
                }
            }
            let (token, _) = eval(FrozenTokenList::from_tokenv(&ftl.get_contents()[3..].to_vec(), bound)?, bindv.clone())?;
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

    // if文の処理
    if ftl.get(0).unwrap() == Token::KeywordToken("if") {
        // then節を探す(なければerror)
        let option_then = rewrite_error(Token::find_bracket(&ftl.get_contents(), Token::KeywordToken("if"), Token::KeywordToken("then")),
                                        "Keyword 'then' not found after 'if' token.".to_string())?;
        let (_, i_then) = option_then.unwrap();

        // else節を探す(なければerror)
        let option_else = rewrite_error(Token::find_bracket(&ftl.get_contents(), Token::KeywordToken("if"), Token::KeywordToken("else")),
                                        "Keyword 'else' not found after 'if' token.".to_string())?;
        let (_, i_else) = option_else.unwrap();

        // thenがelseより後ろならerror
        if i_then > i_else {
            return Err("Keyword 'then' found after 'else'.".to_string());
        }

        // if節を評価
        let (token1, bindv1) = eval(FrozenTokenList::from_tokenv(&ftl.get_contents()[1..i_then].to_vec(), bound)?, bindv.clone())?;
        let tokenv_then = &ftl.get_contents()[i_then+1..i_else]; // then節
        let tokenv_else = &ftl.get_contents()[i_else+1..]; // else節

        match token1 {
            Token::BoolToken(b) => { // 評価結果がbool型だった場合
                if b { // 条件式==trueの場合
                    return eval(FrozenTokenList::from_tokenv(&tokenv_then.to_vec(), bound)?, bindv1);
                } else { // 条件式==falseの場合
                    return eval(FrozenTokenList::from_tokenv(&tokenv_else.to_vec(), bound)?, bindv1);
                }
            },
            // boolじゃなかったらError
            _ => return Err("Type error: Non-bool value returned by the 'if' expression.".to_string()),
        }
    }

    // 括弧処理
    let mut contents: Vec<Token> = ftl.get_contents().to_vec();
    while let Some(i) = find_frozen(&contents) {
        match contents[i].clone() {
            Token::FrozenToken(ftl1) => {
                if ftl1.bound_is("(", ")") {
                    let (token, bindv1) = eval(ftl1, bindv)?;
                    contents[i] = token;
                    bindv = bindv1;
                } else if ftl1.bound_is("{", "}") { // Setの場合
                    let set = set_from_frozen(ftl1, bindv.clone())?;
                    contents[i] = Token::SetToken(set);
                }
            },
            token => panic!("eval: Function find_frozen() brought index of non-FrozenToken: {}", token.to_string()),
        }
    }

    // Identifierトークンの置き換え処理
    for i in 0..contents.len() {
        let token = &contents[i];
        contents[i] = substitute(&token, bindv.clone())?;
    }

    // Operator探し
    let preset_opmap = preset_operators();
    let mut index: Option<usize> = None;
    let mut priority = 11;
    let mut operator: &Operator = preset_opmap.get(&"in".to_string()).unwrap();        

    for i in 0..contents.len() {
        let token: Token = contents[i].clone();

        // tokenがOperatorかどうか調べる
        if let Some(opname) = Operator::from_token(token.clone()) {
            // Operatorだったら優先順位を確認
            let op: &Operator = preset_opmap.get(&opname.to_string()).unwrap();
            let priority1 = op.priority();

            // 優先順位が既存より高ければindexと優先順位，Operatorオブジェクトを記憶
            if priority1 < priority {
                index = Some(i);
                operator = preset_opmap.get(&opname).unwrap();
                priority = priority1;
            }
        }    
    }

    //何も見つかっていなかったらエラー
    if let Some(i) = index {
        let mut tv1: Vec<Token> = contents[0..i].to_vec();
        let mut tv2: Vec<Token> = contents[i+1..].to_vec();

        match operator {
            Operator::BinaryOp(binop) => {
                let t1:Token = tv1.pop().ok_or("Parse error: Nothing before binary operator.".to_string())?;
                if tv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let t2 = tv2.remove(0);
                let t_res = binop.apply(t1, t2)?;
                tv1.push(t_res);
                tv1.append(&mut tv2);
                return eval(FrozenTokenList::from_tokenv(&tv1, bound)?, bindv.clone());
            },
            Operator::UnaryOp(unop) => {
                if tv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let t = tv2.remove(0);
                let t_res = unop.apply(t)?;
                tv1.push(t_res);
                tv1.append(&mut tv2);
                return eval(FrozenTokenList::from_tokenv(&tv1, bound)?, bindv.clone());
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

pub fn eval_string(s: &String, bindv: Vec<Bind>) -> Result<(Token, Vec<Bind>), String> {
    return eval(FrozenTokenList::from_string(s)?, bindv);
}
