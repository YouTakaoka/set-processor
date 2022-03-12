
pub const KEYWORD_LIST: [&str; 15] = ["let", "if", "then", "else", "true", "false", "def", "Set", "Bool", "Number","exit", "in", "size", "is_empty", "print"];

//2文字シンボルは必ず最初に入れること！
pub const SYMBOL_LIST: [&str; 29] = ["==", "!=", "&&", "||", ">=", "<=", "->", " ", "!", "=", "{", "}", ",", "#", "+", "*", "-", "(", ")", "[", "]", "<", ">", "&", ":", ";", "$", "%", "|"];

#[derive(Clone, PartialEq)]
pub enum Token {
    Symbol(&'static str),
    Keyword(&'static str),
    Identifier(String),
    Number(usize),
}

impl Token {
    pub fn split_by_token(string: &String) -> Option<(String, Token, String)> {
        // Symbolから順番にtokenを探してsplitしていく
        for symbol in SYMBOL_LIST {
            if let Some(n) = string.find(symbol) {
                let s1 = &string[0..n];
                let s2 = &string[n + symbol.len()..];
                let token = Token::Symbol(symbol);
                return Some((s1.to_string(), token, s2.to_string()));
            }
        }

        for kw in KEYWORD_LIST {
            if string == kw {
                let token = Token::Keyword(kw);
                return Some(("".to_string(), token, "".to_string()));
            }
        }

        // SymbolもKeywordも見つからなければNumberかIdentifier
        if let Ok(n) = string.parse::<usize>() { // parseできたらNumber
            return Some(("".to_string(), Token::Number(n), "".to_string()));
        }

        // 何にも当てはまらなければ一旦Identifierと見做す
        return Some(("".to_string(), Token::Identifier(string.clone()), "".to_string()));
    }

    pub fn tokenize(s: &String) -> Result<Vec<Token>, String> {
        // コメントを削除
        let string: &String = &s.split('%').map(|s| s.to_string()).collect::<Vec<String>>()[0];

        if string.is_empty() {
            return Ok(Vec::new());
        }

        match Self::split_by_token(string) {
            None => return Err("Parse error: Failed to tokenize.".to_string()),
            Some((s1, token, s2)) => {
                let mut tv1 = Self::tokenize(&s1)?;
                let mut tv2 = Self::tokenize(&s2)?;

                if token != Token::Symbol(" ") { // スペースはpushしない
                    tv1.push(token);
                }
                
                tv1.append(&mut tv2);
                return Ok(tv1);
            }
        }
    }
}
