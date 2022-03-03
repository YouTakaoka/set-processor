mod constants;
mod setlike;

pub use self::constants::*;
pub use self::setlike::*;

#[derive(Clone, PartialEq)]
pub enum Token {
    SetToken(Set),
    KeywordToken(&'static str),
    SymbolToken(&'static str),
    IdentifierToken(String),
    BoolToken(bool),
    NullToken,
    FrozenToken(FrozenTokenList),
    OperatorToken(Operator),
}

pub enum TokenType {
    SetToken,
    KeywordToken,
    SymbolToken,
    IdentifierToken,
    BoolToken,
    NullToken,
    FrozenToken,
    OperatorToken,
}

impl Token {
    pub fn get_type(&self) -> TokenType {
        match self {
            Token::SetToken(_) => TokenType::SetToken,
            Token::KeywordToken(_) => TokenType::KeywordToken,
            Token::SymbolToken(_) => TokenType::SymbolToken,
            Token::IdentifierToken(_) => TokenType::IdentifierToken,
            Token::BoolToken(_) => TokenType::BoolToken,
            Token::NullToken => TokenType::NullToken,
            Token::FrozenToken(_) => TokenType::FrozenToken,
            Token::OperatorToken(_) => TokenType::OperatorToken,
        }
    }
   
    pub fn to_set(&self, mes: &String) -> Result<Set, String> {
        match self {
            Token::SetToken(set) => Ok(set.clone()),
            _ => Err(mes.clone()),
        }
    }

    pub fn to_bool(&self, mes: &String) -> Result<bool, String> {
        match self {
            Token::BoolToken(b) => Ok(*b),
            _ => Err(mes.clone()),
        }
    }

    pub fn to_operator(&self, mes: &String) -> Result<Operator, String> {
        match self {
            Token::OperatorToken(op) => Ok(op.clone()),
            _ => Err(mes.clone()),
        }
    }

    pub fn split_by_token(string: &String) -> Option<(String, Token, String)> {
        // Symbolから順番にtokenを探してsplitしていく
        for symbol in SYMBOL_LIST {
            if let Some(n) = string.find(symbol) {
                let s1 = &string[0..n];
                let s2 = &string[n + symbol.len()..];
                let token = Token::SymbolToken(symbol);
                return Some((s1.to_string(), token, s2.to_string()));
            }
        }

        for kw in KEYWORD_LIST {
            if let Some(n) = string.find(kw) {
                let s1 = &string[0..n];
                let s2 = &string[n + kw.len()..];
                let token = Token::KeywordToken(kw);
                return Some((s1.to_string(), token, s2.to_string()));
            }
        }

        // SymbolTokenもKeywordTokenも見つからなければIdentifierTokenと見做す
        return Some(("".to_string(), Token::IdentifierToken(string.clone()), "".to_string()));
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

                if token != Token::SymbolToken(" ") { // スペースはpushしない
                    tv1.push(token);
                }
                
                tv1.append(&mut tv2);
                return Ok(tv1);
            }
        }
    }

    pub fn to_string(self: &Self) -> String {
        match self {
            Self::SetToken(set) => set.to_string(),
            Self::SymbolToken(s) => s.to_string(),
            Self::KeywordToken(s) => s.to_string(),
            Self::IdentifierToken(s) => s.to_string(),
            Self::BoolToken(b) => b.to_string(),
            Self::NullToken => "".to_string(),
            Self::FrozenToken(ftl) => ftl.to_string(),
            Self::OperatorToken(op) => op.name(),
        }
    }

    pub fn tokenv_to_string(tv: &Vec<Token>) -> String {
        let mut s = "".to_string();
        for token in tv {
            s = format!("{}'{}',", s, token.to_string());
        }
        if !s.is_empty() {
            s.pop();
        }
        s = format!("[{}]", s);
        return s;
    }

    pub fn find_bracket(tokenv: &Vec<Token>, tb: Token, te: Token) -> Result<Option<(usize, usize)>, String> {
        let mut ib: Option<usize> = None;
        let mut cnt = 0;

        for i in 0..tokenv.len() {
            if tokenv[i] == te {
                if cnt < 1 {
                    return Err("Bracket is not closed.".to_string());
                } else if cnt == 1 {
                    return Ok(Some((ib.unwrap(), i)));
                }
                cnt -= 1;
            } else if tokenv[i] == tb {
                if cnt == 0 {
                    ib = Some(i);
                }
                cnt += 1;
            }
        }

        if let None = ib {
            return Ok(None);
        }
        
        return Err("Bracket is not closed.".to_string());
    }

}

pub fn split_drop(tokenv: &Vec<Token>, i1: usize, i2: usize) -> (Vec<Token>, Vec<Token>) {
    if i1 > i2 {
        panic!("i1 is greater than i2. i1={} while i2={}", i1, i2);
    }

    if tokenv.len() < i2+1 {
        panic!("Length of tokenv is too short. tokenv.len()={}, but i2={}.", tokenv.len(), i2);
    }

    let tv1 = tokenv[0..i1].to_vec();

    let tv2;
    if tokenv.len() >= i2+2 {
        tv2 = tokenv[i2+1..].to_vec();
    } else {
        tv2 = Vec::new();
    }
    
    return (tv1, tv2);
}

pub fn subst_range(tokenv: &Vec<Token>, i1: usize, i2: usize, token: Token) -> Vec<Token> {
    let (tv1, tv2) = split_drop(tokenv, i1, i2);
    let mut tv = tv1.clone();
    tv.push(token);
    tv.append(&mut tv2.clone());
    return tv;
}

#[derive(Clone, PartialEq)]
pub struct FrozenTokenList {
    contents: Vec<Token>,
    bound: Option<(String, String)>
}

pub fn display_bound(bound: &Option<(String, String)>) -> String {
    match bound {
        None => "None".to_string(),
        Some((b,e)) => format!("({}, {})", b, e)
    }
}

impl FrozenTokenList {
    fn find_frozenbound(tv: &Vec<Token>) -> Result<Option<(usize, usize)>, String> {
        let mut i_bound: Option<(usize, usize)> = None;

        for (b, e) in FROZEN_BOUND {
            if let Some((ib, ie)) = Token::find_bracket(tv, Token::SymbolToken(b), Token::SymbolToken(e))? {
                match i_bound {
                    None => i_bound = Some((ib, ie)),
                    Some((ib_old, _)) => {
                        if ib < ib_old {
                            i_bound = Some((ib, ie));
                        }
                    }
                }
            }
        }

        return Ok(i_bound);
    }

    pub fn from_tokenv(tv: Vec<Token>, bound: &Option<(String, String)>) -> Result<Self, String> {
        let mut contents = tv;

        while let Some((ib, ie)) = Self::find_frozenbound(&contents)? {
            let b = contents[ib].to_string();
            let e = contents[ie].to_string();

            let (tv_other, mut tv3) = split_drop(&contents, ie, ie);
            let (mut tv1, tv2) = split_drop(&tv_other, ib, ib);

            let token = Token::FrozenToken(Self::from_tokenv(tv2, &Some((b, e)))?);
            tv1.push(token);
            tv1.append(&mut tv3);
            contents = tv1;
        }
        
        return Ok(Self {
            contents: contents,
            bound: bound.clone(),
        })
    }

    pub fn from_string(string: &String) -> Result<Self, String> {
        return Self::from_tokenv(Token::tokenize(string)?, &None);
    }

    pub fn is_empty(&self) -> bool {
        return self.contents.is_empty();
    }

    pub fn len(&self) -> usize {
        return self.contents.len();
    }

    pub fn get(&self, i: usize) -> Option<Token> {
        return Some(self.contents.get(i)?.clone());
    }

    pub fn get_contents(&self) -> Vec<Token> {
        return self.contents.clone();
    }

    pub fn get_bound(&self) -> &Option<(String, String)> {
        return &self.bound;
    }

    pub fn bound_is_none(&self) -> bool {
        match self.get_bound() {
            None => return true,
            _ => return false,
        }
    }

    pub fn bound_is(&self, b: &str, e: &str) -> bool {
        match self.get_bound() {
            None => return false,
            Some((b1, e1)) => return b1 == b && e1 == e,
        }
    }

    pub fn to_string(&self) -> String {
        let mut string = String::new();

        for token in self.get_contents() {
            string = format!("{}, {}", string, token.to_string());
        }
        string.remove(0);
        string = format!("[{}]", string);

        return string;
    }
}

#[derive(Clone)]
pub struct BinaryOp {
    name: String,
    f: fn(Token, Token) -> Result<Token, String>,
    priority: usize,
}

impl BinaryOp {
    pub fn apply(&self, t1: Token, t2: Token) -> Result<Token, String> {
        return (self.f)(t1, t2);
    }
}

#[derive(Clone)]
pub struct UnaryOp {
    name: String,
    f: fn(Token) -> Result<Token, String>,
    priority: usize,
}

impl UnaryOp {
    pub fn apply(&self, t: Token) -> Result<Token, String> {
        return (self.f)(t);
    }
}

// presetの演算子はここに追加していく
pub fn preset_operators<'a>() -> std::collections::HashMap<String, Operator> {
    let opv = vec![
        Operator::UnaryOp(UnaryOp {
            name: "!".to_string(),
            priority: 5,
            f: |t: Token| {
                let b = t.to_bool(&"Type Error in the first argument of unary operator.".to_string())?;
                Ok(Token::BoolToken(!b))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "==".to_string(),
            priority: 4,
            f: |t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::BoolToken(s1 == s2))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "!=".to_string(),
            priority: 4,
            f: |t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::BoolToken(s1 != s2))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "in".to_string(),
            priority: 4,
            f: |t1: Token, t2: Token| {
                //println!("{}", t2.to_string()); //tofix
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::BoolToken(s1.is_in(&s2)))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "-".to_string(),
            priority: 4,
            f: |t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::SetToken(Set::set_diff(&s1,&s2)))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "+".to_string(),
            priority: 3,
            f: |t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::SetToken(Set::set_union(&s1,&s2)))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "*".to_string(),
            priority: 2,
            f: |t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::SetToken(Set::set_intersec(&s1,&s2)))
            },
        }),
    ];

    let opnames: Vec<String> = opv.iter().map(|x| x.name()).collect();
    return opnames.into_iter().zip(opv.into_iter()).collect();
}

#[derive(Clone)]
pub enum Operator {
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
}

impl PartialEq for Operator {
    fn eq(&self, rhs: &Self) -> bool {
        return self.name() == rhs.name();
    }
}

impl Operator {
    pub fn priority(&self) -> usize {
        match self {
            Self::BinaryOp(op) => op.priority,
            Self::UnaryOp(op) => op.priority,
        }
    }

    pub fn name(&self) -> String {
        match self {
            Self::BinaryOp(op) => op.name.clone(),
            Self::UnaryOp(op) => op.name.clone(),
        }
    }

    pub fn from_token(token: Token) -> Option<Self> {
        let preset_opmap = preset_operators();
        for opname in preset_opmap.keys() {
            if token.to_string() == opname.clone() {
                return Some(preset_opmap.get(opname)?.clone());
            }
        }
        return None;
    }
}
