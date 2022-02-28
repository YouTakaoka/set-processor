mod operator;

pub use self::operator::token_and_setlike::*;
pub use self::operator::Operator;
pub use self::operator::preset_operators;

#[derive(Clone)]
pub struct PurifiedTokenList {
    content: Vec<Token>,
}

impl<'a> PurifiedTokenList {
    fn from_tokenv(tv: Vec<Token>) -> Result<Self, String> {
        let mut tv1: Vec<Token> = Vec::new();

        let mut previous_is_not_symbol = false; // 連続したnon-symbol tokenを判定するためのフラグ
        let mut previous_token = "".to_string();
        for token in tv {
            // '}'があったらError
            if token == Token::SymbolToken("}") {
                return Err("Curly brace is not closed.".to_string());
            }

            // 連続したnon-symbol tokenがあったらError
            match token {
                Token::SymbolToken(_) => previous_is_not_symbol = false,  // フラグを更新
                _ => {
                    if previous_is_not_symbol {
                        return Err(format!("Parse error: Found two contiguous non-symbol tokens without any spaces between them: {}, {}", previous_token, token.to_string()));
                    } else {
                        previous_is_not_symbol = true; // フラグを更新
                    }
                },
            }
            previous_token = token.to_string();            
            
            // tokenがスペースでない限り追加
            if token != Token::SymbolToken(" ") {
                tv1.push(token);
            }
        }

        return Ok(PurifiedTokenList {content: tv1});
    }

    pub fn from_string(s: &String) -> Result<Self, String> {
        return Ok(Self::from_tokenv(Set::parse_all_sets(Token::tokenize(&s)?)?)?);
    }

    pub fn to_string(self: &Self) -> String {
        return Token::tokenv_to_string(&self.content);
    }

    fn is_empty(&self) -> bool {
        return self.content.is_empty();
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

    pub fn find_bracket(tokenv: &Vec<Token>, b: &str, e: &str) -> Result<Option<(usize, usize)>, String> {
        let mut ib: Option<usize> = None;
        let mut cnt = 0;

        for i in 0..tokenv.len() {
            if let Token::SymbolToken(symbol) = tokenv[i] {
                if symbol == e {
                    if cnt < 1 {
                        return Err("Bracket is not closed.".to_string());
                    } else if cnt == 1 {
                        return Ok(Some((ib.unwrap(), i)));
                    }
                    cnt -= 1;
                } else if symbol == b {
                    if cnt == 0 {
                        ib = Some(i);
                    }
                    cnt += 1;
                }
            }
        }

        if let None = ib {
            return Ok(None);
        }
        
        return Err("Bracket is not closed.".to_string());
    }

    pub fn eval(&self, bindv: Vec<Bind>) -> Result<(Token, Vec<Bind>), String> {
        if self.is_empty() {
            return Ok((Token::NullToken, bindv));
        }
        
        // 先頭がletキーワードだった場合の処理
        if self.content[0] == Token::KeywordToken("let") {
            if self.content.len() < 4 {
                return Err("Parse error: 'let' statement is too short.".to_string());
            }

            if self.content[2] != Token::SymbolToken("=") {
                return Err("Parse error: Not found '=' token after 'let' keyword.".to_string())
            }

            match &self.content[1] {
                Token::IdentifierToken(identifier) => {
                    for bind in bindv.clone() {
                        if bind.identifier == identifier.clone() {
                            return Err(format!("Token {} is already reserved as identifier.", identifier.clone()));
                        }
                    }
                    let (token, _) = Self {content: self.content[3..].to_vec()}.eval(bindv.clone())?;
                    let mut bindv_new = bindv.clone();
                    bindv_new.push(Bind {
                        identifier: identifier.clone(),
                        value: token.clone(),
                    });
                    return Ok((token, bindv_new));
                },
                _ => return Err(format!("Cannot use '{}' as identifier.", self.content[1].to_string())),
            }
        }

        // 括弧処理
        while let Some((ib, ie)) = Self::find_bracket(&self.content, "(", ")")? {
            let mut tv1 = self.content[0..ib].to_vec();
            let tv2 = self.content[ib+1..ie].to_vec();
            let mut tv3 = self.content[ie+1..].to_vec();
            let (token, _) = Self {content: tv2}.eval(bindv.clone())?;
            tv1.push(token);
            tv1.append(&mut tv3);
            return Self {content: tv1}.eval(bindv);
        }

        // Identifierトークンの置き換え処理
        let mut content: Vec<Token> = Vec::new();
        for token in &self.content {
            content.push(Self::substitute(&token, bindv.clone())?);
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
                        return Self {content: tv1}.eval(bindv);
                    },
                    Operator::UnaryOp(unop) => {
                        if tv2.is_empty() {
                            return Err("Parse error: Nothing after binary operator.".to_string());
                        }
                        let t = tv2.remove(0);
                        let t_res = unop.apply(t)?;
                        tv1.push(t_res);
                        tv1.append(&mut tv2);
                        return Self {content: tv1}.eval(bindv);
                    },
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct Bind {
    identifier: String,
    value: Token,
}

pub fn eval_string(s: &String, bindv: Vec<Bind>) -> Result<(Token, Vec<Bind>), String> {
    return PurifiedTokenList::from_string(s)?.eval(bindv);
}
