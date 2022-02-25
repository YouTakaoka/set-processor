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
        match eval_string(&buffer) {
            Ok(token) => println!("{}", token.to_string()),
            Err(e) => println!("{}", e),
        }
    }
    Ok(())
}

fn eval_string(s: &String) -> Result<Token, String> {
    return PurifiedTokenList::from_string(s)?.eval();
}

const KEYWORD_LIST: [&str; 3] = ["in", "size", "is_empty"];
const SYMBOL_LIST: [char; 6] = [' ', '{', '}', ',', '+', '*'];

#[derive(Clone, PartialEq)]
enum Token {
    SetToken(Set),
    KeywordToken(&'static str),
    SymbolToken(char),
    IdentifierToken(String),
    BoolToken(bool),
}

impl Token {
    fn keyword_from_string(s: &String) -> Option<Token> {
        for t in KEYWORD_LIST {
            if s == t {
                return Some(Self::KeywordToken(t));
            }
        }
        return None;
    }

    fn symbol_from_char(c: char) -> Option<Token> {
        for t in SYMBOL_LIST {
            if c == t {
                return Some(Self::SymbolToken(c));
            }
        }
        return None;
    }

    fn token_from_string(s: &String) -> Option<Token> {
        for f in [Self::keyword_from_string] {
            match f(&s) {
                None => (),
                Some(t) => return Some(t),
            }
        }
        return None;
    }

    fn read_token(s: &String) -> Option<(Token, String)> {
        let mut s1: String = String::new();
        let mut s2: String = s.clone();

        // s1の値がTokenとして解釈できるまでs2の頭文字をpushし続ける
        loop {
            let (c, tmp) = read_char(s2.clone())?;
            
            match Self::symbol_from_char(c) {
                None => (),
                Some(symbol) => {  // cが記号として解釈できるなら...
                    if s1.is_empty() {
                        // s1が空ならばsymbolを記号として返す
                        return Some((symbol, tmp));
                    } else {
                        // s1が空でないなら一旦s1をトークンとして返す
                        return Some((Token::token_from_string(&s1)?, s2));
                    }
                },
            }

            s2 = tmp;
            s1.push(c);
        }
    }

    fn tokenize(string: &String) -> Result<Vec<Token>, String> {
        let mut s1 = string.clone();
        let mut tv: Vec<Token> = Vec::new();
        while !s1.is_empty() {
            let (token, s) = match Self::read_token(&s1) {
                Some(touple) => touple,
                None => return Err("Parse error: Failed to tokenize.".to_string()),
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
            Self::BoolToken(b) => b.to_string(),
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

    fn check_empty_and_remove_spaces(tv: &Vec<Token>) -> Result<Vec<Token>, String> {
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

struct BinaryOp {
    f: Box<dyn Fn(Token, Token) -> Result<Token, String>>,
}

impl BinaryOp {
    fn apply(&self, t1: Token, t2: Token) -> Result<Token, String> {
        return (self.f)(t1, t2);
    }
}

enum Operator {
    BinaryOp(BinaryOp),
    //UnaryOp(Box<dyn Fn(Token) -> Token>),
}

impl Operator {
    fn from_token(token: Token) -> Option<Self> {
        if token == Token::KeywordToken("in") {
            let f = |t1: Token, t2: Token| {
                match t1 {
                    Token::SetToken(set1) => {
                        match t2 {
                            Token::SetToken(set2) => Ok(Token::BoolToken(set1.is_in(&set2))),
                            _ => Err("Type error.".to_string()),
                        }
                    },
                    _ => Err("Type error.".to_string()),
                }
            };
            return Some(Operator::BinaryOp(BinaryOp {f: Box::new(f)}));
        }
        return None;
    }
}

#[derive(Clone)]
struct PurifiedTokenList {
    content: Vec<Token>,
}

impl<'a> PurifiedTokenList {
    fn from_tokenv(tv: Vec<Token>) -> Result<Self, String> {
        let mut tv1: Vec<Token> = Vec::new();

        let mut previous_is_not_symbol = false; // 連続したnon-symbol tokenを判定するためのフラグ
        for token in tv {
            // '}'があったらError
            if token == Token::SymbolToken('}') {
                return Err("Curly brace is not closed.".to_string());
            }

            // 連続したnon-symbol tokenがあったらError
            match token {
                Token::SymbolToken(_) => previous_is_not_symbol = false,  // フラグを更新
                _ => {
                    if previous_is_not_symbol {
                        return Err("Parse error: Found two contiguous non-symbol tokens without any spaces between them.".to_string());
                    } else {
                        previous_is_not_symbol = true; // フラグを更新
                    }
                },
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

    fn is_empty(&self) -> bool {
        return self.content.is_empty();
    }

    fn eval(&self) -> Result<Token, String> {
        if self.is_empty() {
            panic!("PurifiedTokenList::eval: Got empty vector.");
        }
        if self.content.len() == 1 {
            return Ok(self.content[0].clone());
        }

        let mut tv1: Vec<Token> = Vec::new();
        let mut tv2: Vec<Token> = self.content.clone();

        while !tv2.is_empty() {
            let token = tv2.remove(0);

            // tokenがOperatorかどうか調べる
            match Operator::from_token(token.clone()) {
                None => tv1.push(token),  //Operatorじゃなかったらtv1に追加
                Some(operator) => {
                    match operator {
                        Operator::BinaryOp(op) => {
                            let t1:Token = tv1.pop().ok_or("Parse error: Nothing before binary operator.".to_string())?;
                            if tv2.is_empty() {
                                return Err("Parse error: Nothing after binary operator.".to_string());
                            }
                            let t2 = tv2.remove(0);
                            let t_res = op.apply(t1, t2)?;
                            tv1.push(t_res);
                            tv1.append(&mut tv2);
                            return Self::from_tokenv(tv1)?.eval();
                        },
                    }
                },
            }
        }
        return Err("Parse error".to_string());
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

    fn is_in(&self, set: &Self) -> bool {
        for e in set.content() {
            if self == e {
                return true;
            }
        }
        return false;
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

