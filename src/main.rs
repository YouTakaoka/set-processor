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
        match Token::tokenize(buffer) {
            Some(tv) => {
                match Set::parse_all_sets(tv) {
                    Ok(tv1) => println!("{}", Token::token_vec_to_string(tv1)),
                    Err(e) => println!("{}", e),
                }
            },
            None => println!("Error!"),
        }
    }
    Ok(())
}

const KEYWORD_LIST: [&str; 3] = ["in", "size", "is_empty"];
const SYMBOL_LIST: [&str; 4] = [" ", "{", "}", ","];

#[derive(Clone, PartialEq)]
enum Token {
    SetToken(Set),
    KeywordToken(String),
    SymbolToken(String),
    IdentifierToken(String),
}

impl Token {
    fn keyword_from_string(s: String) -> Option<Token> {
        for t in KEYWORD_LIST {
            if s == t.to_string() {
                return Some(Self::KeywordToken(s));
            }
        }
        return None;
    }

    fn symbol_from_string(s: String) -> Option<Token> {
        for t in SYMBOL_LIST {
            if s == t.to_string() {
                return Some(Self::SymbolToken(s));
            }
        }
        return None;
    }

    fn token_from_string(s: String) -> Option<Token> {
        for f in [Self::keyword_from_string, Self::symbol_from_string] {
            match f(s.clone()) {
                None => (),
                Some(t) => return Some(t),
            }
        }
        return None;
    }

    fn read_token(s: String) -> Option<(Token, String)> {
        let mut s1: String = String::new();
        let mut s2: String = s.clone();
        loop {
            let (c, tmp) = read_char(s2.clone())?;
            if c == ' ' && !s1.is_empty() {
                return Some((Token::token_from_string(s1)?, s2));
            }
            s2 = tmp;
            s1.push(c);

            match Self::symbol_from_string(s1.clone()) {
                None => continue,
                Some(symbol) => return Some((symbol, s2)),
            }
        }
    }

    fn tokenize(string: String) -> Option<Vec<Token>> {
        let mut s1 = string.clone();
        let mut tv: Vec<Token> = Vec::new();
        while !s1.is_empty() {
            let (token, s) = Self::read_token(s1)?;
            s1 = s;
            tv.push(token);
        }
        return Some(tv);
    }

    fn to_string(self: &Self) -> String {
        match self {
            Self::SetToken(set) => set.to_string(),
            Self::SymbolToken(s) => s.to_string(),
            Self::KeywordToken(s) => s.to_string(),
            Self::IdentifierToken(s) => s.to_string(),
        }
    }

    fn token_vec_to_string(tv: Vec<Token>) -> String {
        let mut s = "[".to_string();
        for token in tv {
            s = format!("{}'{}',", s, token.to_string());
        }
        s.pop();
        s.push(']');
        return s;
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

struct SetList {
    content: Vec<SetList>,
}

impl SetList {
    fn create(tv: Vec<Token>) -> Result<(SetList, Vec<Token>), String> {
        if tv.is_empty() {
            panic!("SetList::create: Got empty vector.");
        }
        let mut tv1: Vec<Token> = tv.clone();
        let t0 = tv1.remove(0);
        if t0 != Token::SymbolToken("{".to_string()) {
            return Err("Parse error of set literal.".to_string());
        }

        let mut content: Vec<SetList> = Vec::new();
        loop {
            if tv1.is_empty() {
                return Err("Curly brace is not closed.".to_string());
            }
            while tv1[0] == Token::SymbolToken(" ".to_string()) {
                tv1.remove(0);
                if tv1.is_empty() {
                    return Err("Curly brace is not closed.".to_string());
                }
            }

            if tv1[0] == Token::SymbolToken("}".to_string()) {
                tv1.remove(0);  // remove "}"
                return Ok((SetList {content: content}, tv1));
            }
            let (sl, tv2) = Self::create(tv1)?;
            tv1 = tv2;
            content.push(sl.copy());
            
            if tv1.is_empty() {
                return Err("Curly brace is not closed.".to_string());
            }
            while tv1[0] == Token::SymbolToken(" ".to_string()) {
                tv1.remove(0);
                if tv1.is_empty() {
                    return Err("Curly brace is not closed.".to_string());
                }
            }
            if tv1[0] == Token::SymbolToken(",".to_string()) {
                tv1.remove(0);
            }
        }
    }

    fn iter(self: &Self) -> std::slice::Iter<SetList> {
        return self.content.iter();
    }

    fn len(self: &Self) -> usize {
        return self.content.len();
    }

    fn is_empty(self: &Self) -> bool {
        return self.content.is_empty();
    }
    
    fn copy(self: &Self) -> SetList {
        return SetList {content: self.iter().map(|x| x.copy()).collect()};
    }

    // 一意化及びソートを行う
    fn uniquify(self: &Self) -> Set {
        if self.is_empty() {
            return Set {content: Vec::new()};
        }

        // まず，各要素をuniquify
        let mut sv: Vec<Set> = self.iter().map(|x| x.uniquify()).collect();

        // ソート
        sv.sort_by(set_cmp);

        // 一意化
        let mut tmp = sv[0].clone();
        let mut sv_ret :Vec<Set> = Vec::new();
        sv_ret.push(tmp.clone());
        for i in 1..sv.len() {
            if set_cmp(&sv[i], &tmp) != Ordering::Equal {
                sv_ret.push(sv[i].clone());
                tmp = sv[i].clone();
            }
        }

        return Set {content: sv_ret};
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
}

struct Set {
    content: Vec<Set>,
}

impl Set {
    fn create(tv: Vec<Token>) -> Result<(Set, Vec<Token>), String> {
        let (sl, tv1) = SetList::create(tv)?;
        return Ok((sl.uniquify(), tv1));
    }

    fn iter(self: &Self) -> std::slice::Iter<Set> {
        return self.content.iter();
    }

    fn len(self: &Self) -> usize {
        return self.content.len();
    }

    fn is_empty(self: &Self) -> bool {
        return self.content.is_empty();
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

    fn parse_all_sets(tv: Vec<Token>) -> Result<Vec<Token>, String> {
        let mut tv1 = tv.clone();
        let mut tv2 = Vec::new();
        while !tv1.is_empty() {
            if tv1[0] == Token::SymbolToken("{".to_string()) {
                let (set, tv3) = Self::create(tv1)?;
                tv1 = tv3;
                tv2.push(Token::SetToken(set));
            } else {
                tv2.push(tv1.remove(0));
            }
        }
        return Ok(tv2);
    }
}

impl Clone for Set {
    fn clone(self: &Self) -> Set {
        return Set {content: self.iter().map(|x| x.clone()).collect()};
    }
}

impl PartialEq for Set {
    fn eq(self: &Self, other: &Self) -> bool {
        return set_cmp(self, other) == Ordering::Equal;
    }

    fn ne(self: &Self, other: &Self) -> bool {
        return set_cmp(self, other) != Ordering::Equal;
    }
}

// 一意化・ソート済みのSetListを入力とする
// 辞書式順序で比較する
fn set_cmp(a: &Set, b: &Set) -> Ordering {
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
        let tmp = set_cmp(&a.content[i], &b.content[i]);
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

