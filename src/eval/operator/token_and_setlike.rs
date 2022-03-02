use std::cmp::Ordering;
mod constants;

pub use self::constants::*;

#[derive(Clone)]
pub struct Bind {
    pub identifier: String,
    pub value: Token,
}

pub trait SetLike<T> where T: std::clone::Clone, T: SetLike<T> {
    fn contents(self: &Self) -> &Vec<T>;
    fn new(contents: Vec<T>) -> Self;

    fn len(self: &Self) -> usize {
        return self.contents().len();
    }

    fn is_empty(self: &Self) -> bool {
        return self.contents().is_empty();
    }
    
    fn to_string(self: &Self) -> String {
        if self.is_empty() {
            return "{}".to_string();
        }
        let str_ls: Vec<String> = self.iter().map(|x| x.to_string()).collect();
        let mut ret = String::new();
        for s in str_ls {
            ret = format!("{},{}", ret, s);
        }
        ret.remove(0);
        return format!("{{{}}}", ret);
    }

    fn iter(self: &Self) -> std::slice::Iter<T> {
        return self.contents().iter();
    }
}

#[derive(Clone)]
pub struct SetList {
    contents: Vec<Set>,
}

impl SetLike<Set> for SetList {
    fn new(contents: Vec<Set>) -> Self {
        return Self {contents: contents};
    }

    fn contents(self: &Self) -> &Vec<Set> {
        return &self.contents;
    }
}

impl SetList {
    // 一意化及びソートを行う
    pub fn uniquify(self: &Self) -> Set {
        if self.is_empty() {
            return Set::new(Vec::new());
        }

        let mut sv: Vec<Set> = self.contents.clone();

        // ソート
        sv.sort_by(Set::cmp);

        // 一意化
        let mut tmp = sv[0].clone();
        let mut sv_ret :Vec<Set> = Vec::new();
        sv_ret.push(tmp.clone());
        for i in 1..sv.len() {
            if Set::cmp(&sv[i], &tmp) != Ordering::Equal {
                sv_ret.push(sv[i].clone());
                tmp = sv[i].clone();
            }
        }

        return Set {contents: sv_ret};
    }
}

#[derive(Clone)]
pub struct Set {
    contents: Vec<Set>,
}

impl SetLike<Self> for Set {
    fn new(contents: Vec<Self>) -> Self {
        return Self {contents: contents};
    }

    fn contents(self: &Self) -> &Vec<Self> {
        return &self.contents;
    }
}

impl Set {
    // 一意化・ソート済みのSetListを入力とする
    // 辞書式順序で比較する
    fn cmp(a: &Set, b: &Set) -> Ordering {
        if a.is_empty() {
            if b.is_empty() {
                return Ordering::Equal;
            } else {
                return Ordering::Less;
            }
        } else if b.is_empty() {
            return Ordering::Greater;
        }
        let n = std::cmp::min(a.len(), b.len());
        for i in 0..n {
            let tmp = Self::cmp(&a.contents[i], &b.contents[i]);
            if tmp != Ordering::Equal {
                return tmp;
            }
        }

        if a.len() < b.len() {
            return Ordering::Less;
        } else if a.len() > b.len() {
            return Ordering::Greater;
        } else {
            return Ordering::Equal;
        }
    }

    pub fn is_in(&self, set: &Self) -> bool {
        for e in set.contents() {
            if self == e {
                return true;
            }
        }
        return false;
    }

    fn to_setlist(&self) -> SetList {
        return SetList { contents: self.contents.clone() };
    }

    pub fn set_union(set1: &Set, set2: &Set) -> Set {
        let mut contents1 = set1.to_setlist().contents;
        let mut contents2 = set2.to_setlist().contents;
        contents1.append(&mut contents2);
        let sl = SetList { contents: contents1 };
        return sl.uniquify();
    }

    pub fn set_intersec(set1: &Set, set2: &Set) -> Set {
        let mut contents: Vec<Set> = Vec::new();

        for s in set1.iter() {
            if s.is_in(set2) {
                contents.push(s.clone());
            }
        }

        return Set {contents: contents};
    }

    pub fn set_diff(set1: &Set, set2: &Set) -> Set {
        let mut contents: Vec<Set> = Vec::new();

        for s in set1.iter() {
            if !s.is_in(set2) {
                contents.push(s.clone());
            }
        }

        return Set {contents: contents};
    }
}

impl PartialEq for Set {
    fn eq(self: &Self, other: &Self) -> bool {
        return Set::cmp(self, other) == Ordering::Equal;
    }

    fn ne(self: &Self, other: &Self) -> bool {
        return Set::cmp(self, other) != Ordering::Equal;
    }
}

#[derive(Clone, PartialEq)]
pub enum Token {
    SetToken(Set),
    KeywordToken(&'static str),
    SymbolToken(&'static str),
    IdentifierToken(String),
    BoolToken(bool),
    NullToken,
    FrozenToken(FrozenTokenList),
}

pub enum TokenType {
    SetToken,
    KeywordToken,
    SymbolToken,
    IdentifierToken,
    BoolToken,
    NullToken,
    FrozenToken,
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
        }
    }

    pub fn substitute(token: &Token, bindv: Vec<Bind>) -> Result<Token, String> {
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

    pub fn to_set(&self, mes: &String) -> Result<&Set, String> {
        match self {
            Token::SetToken(set) => Ok(set),
            _ => Err(mes.clone()),
        }
    }

    pub fn to_bool(&self, mes: &String) -> Result<&bool, String> {
        match self {
            Token::BoolToken(b) => Ok(b),
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
    fn find_frozenbound(tv:&Vec<Token>) -> Result<Option<(usize, usize)>, String> {
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

    pub fn from_tokenv(tv: &Vec<Token>, bound: &Option<(String, String)>) -> Result<Self, String> {
        let mut contents = tv.clone();

        while let Some((ib, ie)) = Self::find_frozenbound(&contents)? {
            let b = contents[ib].to_string();
            let e = contents[ie].to_string();
            let mut tv1 = contents[0..ib].to_vec();
            let tv2 = contents[ib+1..ie].to_vec();
            let mut tv3 = contents[ie+1..].to_vec();
            let token = Token::FrozenToken(Self::from_tokenv(&tv2, &Some((b, e)))?);
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
        return Self::from_tokenv(&Token::tokenize(string)?, &None);
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

    pub fn get_contents(&self) -> &Vec<Token> {
        return &self.contents;
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