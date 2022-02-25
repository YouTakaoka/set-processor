use std::cmp::Ordering;

fn main() -> std::io::Result<()> {
    loop{
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer).expect("Fail to read line.");
        buffer.pop();
        if buffer == "exit" {
            break;
        }
        if buffer.is_empty() {
            continue;
        }
        match PurifiedTokenList::from_string(&buffer) {
            Ok(ptl) => println!("{}", ptl.to_string()),
            Err(e) => println!("{}", e),
        }
    }
    Ok(())
}

const KEYWORD_LIST: [&str; 3] = ["in", "size", "is_empty"];
const SYMBOL_LIST: [char; 6] = [' ', '{', '}', ',', '+', '*'];

#[derive(Clone, PartialEq)]
enum Token<'a> {
    SetToken(Set),
    KeywordToken(&'a str),
    SymbolToken(char),
    IdentifierToken(String),
}

impl<'a> Token<'a> {
    fn keyword_from_string(s: &String) -> Option<Token<'a>> {
        for t in KEYWORD_LIST {
            if s == t {
                return Some(Self::KeywordToken(t));
            }
        }
        return None;
    }

    fn symbol_from_char(c: char) -> Option<Token<'a>> {
        for t in SYMBOL_LIST {
            if c == t {
                return Some(Self::SymbolToken(c));
            }
        }
        return None;
    }

    fn token_from_string(s: &String) -> Option<Token<'a>> {
        for f in [Self::keyword_from_string] {
            match f(&s) {
                None => (),
                Some(t) => return Some(t),
            }
        }
        return None;
    }

    fn read_token(s: &String) -> Option<(Token<'a>, String)> {
        let mut s1: String = String::new();
        let mut s2: String = s.clone();

        // s1の値がTokenとして解釈できるまでs2の頭文字をpushし続ける
        loop {
            let (c, tmp) = read_char(s2.clone())?;

            // 記号として解釈できるなら返す
            match Self::symbol_from_char(c) {
                None => (),
                Some(symbol) => {
                    if s1.is_empty() {
                        return Some((symbol, tmp));
                    } else {
                        return Some((Token::token_from_string(&s1)?, s2));
                    }
                },
            }

            s2 = tmp;
            s1.push(c);
        }
    }

    fn tokenize(string: &String) -> Result<Vec<Token<'a>>, String> {
        let mut s1 = string.clone();
        let mut tv: Vec<Token> = Vec::new();
        while !s1.is_empty() {
            let (token, s) = match Self::read_token(&s1) {
                Some(touple) => touple,
                None => return Err("Parse error.".to_string()),
            };
            s1 = s;
            tv.push(token);
        }
        return Ok(tv);
    }

    fn to_string(self: &Self) -> String {
        match self {
            Self::SetToken(set) => set.to_string(),
            Self::SymbolToken(s) => s.to_string(),
            Self::KeywordToken(s) => s.to_string(),
            Self::IdentifierToken(s) => s.to_string(),
        }
    }

    fn tokenv_to_string(tv: &Vec<Token>) -> String {
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

    fn check_empty_and_remove_spaces(tv: &Vec<Token<'a>>) -> Result<Vec<Token<'a>>, String> {
        let mut tv1 = tv.clone();
        if tv1.is_empty() {
            return Err("Curly brace is not closed.".to_string());
        }
        while tv1[0] == Token::SymbolToken(' ') {
            tv1.remove(0);
            if tv1.is_empty() {
                return Err("Curly brace is not closed.".to_string());
            }
        }
        return Ok(tv1);
    }
}

struct PurifiedTokenList<'a> {
    content: Vec<Token<'a>>,
}

impl<'a> PurifiedTokenList<'a> {
    fn from_tokenv(tv: Vec<Token<'a>>) -> Result<Self, String> {
        let mut tv1: Vec<Token> = Vec::new();

        let mut previous_is_set = false; // 連続したSetを判定するためのフラグ
        for token in tv {
            // '}'があったらError
            if token == Token::SymbolToken('}') {
                return Err("Curly brace is not closed.".to_string());
            }

            // 連続したSetがあったらError
            match token {
                Token::SetToken(_) => {
                    if previous_is_set {
                        return Err("Parse error: Two contiguous set literals without any spaces between them.".to_string());
                    }
                    previous_is_set = true; // フラグを更新
                },
                _ => { previous_is_set = false; }, // フラグを更新
            }
            
            // tokenがスペースでない限り追加
            if token != Token::SymbolToken(' ') {
                tv1.push(token);
            }
        }

        return Ok(PurifiedTokenList {content: tv1});
    }

    fn from_string(s: &String) -> Result<Self, String> {
        return Ok(Self::from_tokenv(Set::parse_all_sets(Token::tokenize(&s)?)?)?);
    }

    fn to_string(self: &Self) -> String {
        return Token::tokenv_to_string(&self.content);
    }
}

fn read_char(s: String) -> Option<(char, String)> {
    if s.is_empty() {
        return None;
    }
    let mut s2 = s.clone();
    let c: char = s2.remove(0);
    return Some((c, s2));
}

trait SetLike<T> where T: std::clone::Clone, T: SetLike<T> {
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
struct SetList {
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
        if t0 != Token::SymbolToken('{') {
            return Err("Parse error of set literal.".to_string());
        }

        let mut content: Vec<SetList> = Vec::new();
        loop {
            tv1 = Token::check_empty_and_remove_spaces(&tv1)?;

            if tv1[0] == Token::SymbolToken('}') {
                tv1.remove(0);  // remove "}"
                return Ok((SetList {content: content}, tv1));
            }
            let (sl, tv2) = Self::from_tokenv(tv1)?;
            tv1 = tv2;
            content.push(sl);
            
            tv1 = Token::check_empty_and_remove_spaces(&tv1)?;
            if tv1[0] == Token::SymbolToken(',') {
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
struct Set {
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

    fn parse_all_sets(tv: Vec<Token>) -> Result<Vec<Token>, String> {
        let mut tv1 = tv.clone();
        let mut tv2 = Vec::new();
        while !tv1.is_empty() {
            if tv1[0] == Token::SymbolToken('{') {
                let (set, tv3) = Self::from_tokenv(tv1)?;
                tv1 = tv3;
                tv2.push(Token::SetToken(set));
            } else {
                tv2.push(tv1.remove(0));
            }
        }
        return Ok(tv2);
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

