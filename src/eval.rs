mod operator;

pub use self::operator::token_and_setlike::*;
pub use self::operator::Operator;
pub use self::operator::preset_operators;

fn setlist_from_tokenv(tv: Vec<Token>, bindv: Vec<Bind>) -> Result<SetList, String> {
    //todo
}

fn set_from_tokenv(tv: Vec<Token>, bindv: Vec<Bind>) -> Result<Set, String> {
    let sl = setlist_from_tokenv(tv, bindv)?;
    return Ok(sl.uniquify());
}

fn eval(ftl: FrozenTokenList, bindv: Vec<Bind>) -> Result<(Token, Vec<Bind>), String> {
    let bound = ftl.get_bound();
    if ftl.is_empty() {
        return Ok((Token::NullToken, bindv));
    }
    
    // 先頭がletキーワードだった場合の処理
    if ftl.get(0).unwrap() == Token::KeywordToken("let") {
        if ftl.len() < 4 {
            return Err("Parse error: 'let' statement is too short.".to_string());
        }

        if ftl.get(2).unwrap() != Token::SymbolToken("=") {
            return Err("Parse error: Not found '=' token after 'let' keyword.".to_string())
        }

        match &ftl.get(1).unwrap() {
            Token::IdentifierToken(identifier) => {
                for bind in bindv.clone() {
                    if bind.identifier == identifier.clone() {
                        return Err(format!("Token {} is already reserved as identifier.", identifier.clone()));
                    }
                }
                let (token, _) = eval(FrozenTokenList::from_tokenv(&ftl.get_content()[3..].to_vec(), bound)?, bindv.clone())?;
                let mut bindv_new = bindv.clone();
                bindv_new.push(Bind {
                    identifier: identifier.clone(),
                    value: token.clone(),
                });
                return Ok((token, bindv_new));
            },
            _ => return Err(format!("Cannot use '{}' as identifier.", ftl.get(1).unwrap().to_string())),
        }
    }

    // if文の処理
    if ftl.get(0).unwrap() == Token::KeywordToken("if") {
        match Token::find_bracket(&ftl.get_content(), Token::KeywordToken("if"), Token::KeywordToken("then")) {
            Err(_) => return Err("Keyword 'then' not found after 'if' token.".to_string()),
            Ok(option_then) => { // then節が見つかった場合
                let (_, i_then) = option_then.unwrap();
                match Token::find_bracket(&ftl.get_content(), Token::KeywordToken("if"), Token::KeywordToken("else")) {
                    Err(_) => return Err("Keyword 'else' not found after 'if' token.".to_string()),
                    Ok(option_else) => { // else節が見つかった場合
                        let (_, i_else) = option_else.unwrap();
                        if i_then > i_else {
                            return Err("Keyword 'then' found after 'else'.".to_string());
                        }

                        // if節を評価
                        let (token1, bindv1) = eval(FrozenTokenList::from_tokenv(&ftl.get_content()[1..i_then].to_vec(), bound)?, bindv.clone())?;
                        let tokenv_then = &ftl.get_content()[i_then+1..i_else]; // then節
                        let tokenv_else = &ftl.get_content()[i_else+1..]; // else節

                        match token1 {
                            Token::BoolToken(b) => { // 評価結果がbool型だった場合
                                //todo
                                if b { // 条件式==trueの場合
                                    return eval(FrozenTokenList::from_tokenv(&tokenv_then.to_vec(), bound)?, bindv1);
                                } else { // 条件式==falseの場合
                                    return eval(FrozenTokenList::from_tokenv(&tokenv_else.to_vec(), bound)?, bindv1);
                                }
                            },
                            // boolじゃなかったらError
                            _ => return Err("Type error: Non-bool value returned by the 'if' expression.".to_string()),
                        }
                    },
                }
            },
        }
    }

    // 括弧処理
    while let Some((ib, ie)) = Token::find_bracket(&ftl.get_content(), Token::SymbolToken("("), Token::SymbolToken(")"))? {
        let mut tv1 = ftl.get_content()[0..ib].to_vec();
        let tv2 = ftl.get_content()[ib+1..ie].to_vec();
        let mut tv3 = ftl.get_content()[ie+1..].to_vec();
        let (token, bindv1) = eval(FrozenTokenList::from_tokenv(&tv2, bound)?, bindv.clone())?;
        tv1.push(token);
        tv1.append(&mut tv3);
        return eval(FrozenTokenList::from_tokenv(&tv1, bound)?, bindv1);
    }

    // Identifierトークンの置き換え処理
    let mut content: Vec<Token> = Vec::new();
    for token in ftl.get_content() {
        content.push(Token::substitute(&token, bindv.clone())?);
    }

    // トークン列が長さ1ならそのまま返す
    if content.len() == 1 {
        return Ok((content[0].clone(), bindv));
    }
    
    let preset_opmap = preset_operators();
    let mut index: Option<usize> = None;
    let mut priority = 11;
    let mut operator: &Operator = preset_opmap.get(&"in".to_string()).unwrap();        

    for i in 0..content.len() {
        let token: Token = content[i].clone();

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
    match index {
        None => return Err("Parse error.".to_string()),
        Some(i) => {
            let mut tv1: Vec<Token> = content[0..i].to_vec();
            let mut tv2: Vec<Token> = content[i+1..].to_vec();

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
    }
}

pub fn eval_string(s: &String, bindv: Vec<Bind>) -> Result<(Token, Vec<Bind>), String> {
    return eval(FrozenTokenList::from_string(s)?, bindv);
}
