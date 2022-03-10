
pub const KEYWORD_LIST: [&str; 13] = ["let", "if", "then", "else", "true", "false", "def", "Set", "Bool","exit", "in", "size", "is_empty"];

//2文字シンボルは必ず最初に入れること！
pub const SYMBOL_LIST: [&str; 23] = ["==", "!=", "&&", "||", "->", "!", "=", " ", "{", "}", ",", "+", "*", "-", "(", ")", "[", "]", "<", ">", "&", ":", ";"];

#[derive(Clone, PartialEq)]
pub enum Token {
    Symbol(&'static str),
    Keyword(&'static str),
    Identifier(String),
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
            if let Some(n) = string.find(kw) {
                let s1 = &string[0..n];
                let s2 = &string[n + kw.len()..];
                let token = Token::Keyword(kw);
                return Some((s1.to_string(), token, s2.to_string()));
            }
        }

        // SymbolもKeywordも見つからなければIdentifierと見做す
        return Some(("".to_string(), Token::Identifier(string.clone()), "".to_string()));
    }

    pub fn tokenize(string: &String) -> Result<Vec<Token>, String> {
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
