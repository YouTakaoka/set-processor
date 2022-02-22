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
            Some(tv) => println!("{}", Token::token_vec_to_string(tv)),
            None => println!("Error!"),
        }
    }
    Ok(())
}

const KEYWORD_LIST: [&str; 3] = ["in", "size", "is_empty"];
const SYMBOL_LIST: [&str; 2] = ["{", "}"];

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
            let (c, tmp) = read_char(s2)?;
            s2 = tmp;
            if c == ' ' {
                if s1.is_empty() {
                    continue;
                }
                let t: Token = Self::token_from_string(s1.clone())?;
                return Some((t, s2));
            }
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
    fn create(s: String) -> Result<SetList, String> {
        let pealed = peal(s)?;
        if pealed.is_empty() {
            return Ok(SetList {content: Vec::new()});
        };
        let resv: Vec<Result<SetList, String>> = pealed.split(',').map(String::from).map(SetList::create).collect();
        let mut slv: Vec<SetList> = Vec::new();
        for res in resv.iter() {
            match res {
                Ok(sl) => slv.push(sl.copy()),
                Err(e) => return Err(e.to_string()),
            }
        }
        Ok(SetList {content: slv})
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
        let mut tmp = sv[0].copy();
        let mut sv_ret :Vec<Set> = Vec::new();
        sv_ret.push(tmp.copy());
        for i in 1..sv.len() {
            if set_cmp(&sv[i], &tmp) != Ordering::Equal {
                sv_ret.push(sv[i].copy());
                tmp = sv[i].copy();
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
    fn create(s: String) -> Result<Set, String> {
        let sl: SetList = SetList::create(s)?;
        return Ok(sl.uniquify());
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

    fn copy(self: &Self) -> Set {
        return Set {content: self.iter().map(|x| x.copy()).collect()};
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

fn peal(s: String) -> Result<String, String> {
    let mut r = s.clone();
    if r.remove(0) != '{' || r.pop() != Some('}') {
       return Err("Irregal input.".to_string());
    }
    return Ok(r);
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

