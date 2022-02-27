use std::cmp::Ordering;
mod constants;

pub use self::constants::*;

pub trait SetLike<T> where T: std::clone::Clone, T: SetLike<T> {
    fn content(self: &Self) -> &Vec<T>;
    fn new(content: Vec<T>) -> T;

    fn len(self: &Self) -> usize {
        return self.content().len();
    }

    fn is_empty(self: &Self) -> bool {
        return self.content().is_empty();
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
        return self.content().iter();
    }
}

#[derive(Clone)]
pub struct SetList {
    content: Vec<SetList>,
}

impl SetLike<Self> for SetList {
    fn new(content: Vec<Self>) -> Self {
        return Self {content: content};
    }

    fn content(self: &Self) -> &Vec<Self> {
        return &self.content;
    }
}

impl SetList {
    fn from_tokenv(tv: Vec<Token>) -> Result<(Self, Vec<Token>), String> {
        if tv.is_empty() {
            panic!("SetList::create: Got empty vector.");
        }
        let mut tv1: Vec<Token> = tv.clone();
        let t0 = tv1.remove(0);
        if t0 != Token::SymbolToken("{") {
            return Err("Parse error of set literal.".to_string());
        }

        let mut content: Vec<SetList> = Vec::new();
        loop {
            tv1 = Token::check_empty_and_remove_spaces(&tv1)?;

            if tv1[0] == Token::SymbolToken("}") {
                tv1.remove(0);  // remove "}"
                return Ok((SetList {content: content}, tv1));
            }
            let (sl, tv2) = Self::from_tokenv(tv1)?;
            tv1 = tv2;
            content.push(sl);
            
            tv1 = Token::check_empty_and_remove_spaces(&tv1)?;
            if tv1[0] == Token::SymbolToken(",") {
                tv1.remove(0);
            }
        }
    }

    // 一意化及びソートを行う
    fn uniquify(self: &Self) -> Set {
        if self.is_empty() {
            return Set {content: Vec::new()};
        }

        // まず，各要素をuniquify
        let mut sv: Vec<Set> = self.iter().map(|x| x.uniquify()).collect();

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

        return Set {content: sv_ret};
    }
}

#[derive(Clone)]
pub struct Set {
    content: Vec<Set>,
}

impl SetLike<Self> for Set {
    fn new(content: Vec<Self>) -> Self {
        return Self {content: content};
    }

    fn content(self: &Self) -> &Vec<Self> {
        return &self.content;
    }
}

impl Set {
    fn from_tokenv(tv: Vec<Token>) -> Result<(Set, Vec<Token>), String> {
        let (sl, tv1) = SetList::from_tokenv(tv)?;
        return Ok((sl.uniquify(), tv1));
    }

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
            let tmp = Self::cmp(&a.content[i], &b.content[i]);
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

    pub fn parse_all_sets(tv: Vec<Token>) -> Result<Vec<Token>, String> {
        let mut tv1 = tv.clone();
        let mut tv2 = Vec::new();
        while !tv1.is_empty() {
            if tv1[0] == Token::SymbolToken("{") {
                let (set, tv3) = Self::from_tokenv(tv1)?;
                tv1 = tv3;
                tv2.push(Token::SetToken(set));
            } else {
                tv2.push(tv1.remove(0));
            }
        }
        return Ok(tv2);
    }

    pub fn is_in(&self, set: &Self) -> bool {
        for e in set.content() {
            if self == e {
                return true;
            }
        }
        return false;
    }

    fn to_setlist(&self) -> SetList {
        if self.is_empty() {
            return SetList { content: Vec::new() };
        }

        let content: Vec<SetList> = self.iter().map(|x| x.to_setlist()).collect();
        return SetList { content: content };
    }

    pub fn set_union(set1: &Set, set2: &Set) -> Set {
        let mut content1 = set1.to_setlist().content;
        let mut content2 = set2.to_setlist().content;
        content1.append(&mut content2);
        let sl = SetList { content: content1 };
        return sl.uniquify();
    }

    pub fn set_intersec(set1: &Set, set2: &Set) -> Set {
        let mut content: Vec<Set> = Vec::new();

        for s in set1.iter() {
            if s.is_in(set2) {
                content.push(s.clone());
            }
        }

        return Set {content: content};
    }

    pub fn set_diff(set1: &Set, set2: &Set) -> Set {
        let mut content: Vec<Set> = Vec::new();

        for s in set1.iter() {
            if !s.is_in(set2) {
                content.push(s.clone());
            }
        }

        return Set {content: content};
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
}

pub enum TokenType {
    SetToken,
    KeywordToken,
    SymbolToken,
    IdentifierToken,
    BoolToken,
    NullToken,
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
        }
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
                tv1.push(token);
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

    fn check_empty_and_remove_spaces(tv: &Vec<Token>) -> Result<Vec<Token>, String> {
        let mut tv1 = tv.clone();
        if tv1.is_empty() {
            return Err("Curly brace is not closed.".to_string());
        }
        while tv1[0] == Token::SymbolToken(" ") {
            tv1.remove(0);
            if tv1.is_empty() {
                return Err("Curly brace is not closed.".to_string());
            }
        }
        return Ok(tv1);
    }
}
