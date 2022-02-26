use std::cmp::Ordering;

fn main() -> std::io::Result<()> {
    loop{
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer).expect("Fail to read line.");
        buffer.pop();
        if buffer == "exit" {
            break;
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
const SYMBOL_LIST: [&str; 9] = ["==", "!=", "=", " ", "{", "}", ",", "+", "*"];

#[derive(Clone, PartialEq)]
enum Token {
    SetToken(Set),
    KeywordToken(&'static str),
    SymbolToken(&'static str),
    IdentifierToken(String),
    BoolToken(bool),
    NullToken,
}

enum TokenType {
    SetToken,
    KeywordToken,
    SymbolToken,
    IdentifierToken,
    BoolToken,
    NullToken,
}

impl Token {
    fn get_type(&self) -> TokenType {
        match self {
            Token::SetToken(_) => TokenType::SetToken,
            Token::KeywordToken(_) => TokenType::KeywordToken,
            Token::SymbolToken(_) => TokenType::SymbolToken,
            Token::IdentifierToken(_) => TokenType::IdentifierToken,
            Token::BoolToken(_) => TokenType::BoolToken,
            Token::NullToken => TokenType::NullToken,
        }
    }

    fn to_set(&self, mes: &String) -> Result<&Set, String> {
        match self {
            Token::SetToken(set) => Ok(set),
            _ => Err(mes.clone()),
        }
    }

    fn split_by_token(string: &String) -> Option<(String, Token, String)> {
        // Symbolから順番にtokenを探してsplitしていく
        for symbol in SYMBOL_LIST {
            match string.find(symbol) {
                None => (),
                Some(n) => {
                    let s1 = &string[0..n];
                    let s2 = &string[n + symbol.len()..];
                    let token = Token::SymbolToken(symbol);
                    return Some((s1.to_string(), token, s2.to_string()));
                }
            }
        }

        for kw in KEYWORD_LIST {
            match string.find(kw) {
                None => (),
                Some(n) => {
                    let s1 = &string[0..n];
                    let s2 = &string[n + kw.len()..];
                    let token = Token::KeywordToken(kw);
                    return Some((s1.to_string(), token, s2.to_string()));
                }
            }
        }
        return None;
    }

    fn tokenize(string: &String) -> Result<Vec<Token>, String> {
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

    fn to_string(self: &Self) -> String {
        match self {
            Self::SetToken(set) => set.to_string(),
            Self::SymbolToken(s) => s.to_string(),
            Self::KeywordToken(s) => s.to_string(),
            Self::IdentifierToken(s) => s.to_string(),
            Self::BoolToken(b) => b.to_string(),
            Self::NullToken => "".to_string(),
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
        while tv1[0] == Token::SymbolToken(" ") {
            tv1.remove(0);
            if tv1.is_empty() {
                return Err("Curly brace is not closed.".to_string());
            }
        }
        return Ok(tv1);
    }
}

struct BinaryOp {
    name: String,
    f: Box<dyn Fn(Token, Token) -> Result<Token, String>>,
    priority: usize,
}

impl BinaryOp {
    fn apply(&self, t1: Token, t2: Token) -> Result<Token, String> {
        return (self.f)(t1, t2);
    }
}

const PRESET_OPNAMES: [&str; 2] = ["in", "+"];

fn preset_operators<'a>() -> std::collections::HashMap<String, Operator> {
    let opv = vec![
        Operator::BinaryOp(BinaryOp {
            name: "in".to_string(),
            priority: 4,
            f: Box::new(|t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::BoolToken(s1.is_in(s2)))
            }),
        }),
        Operator::BinaryOp(BinaryOp {
            name: "+".to_string(),
            priority: 3,
            f: Box::new(|t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::SetToken(Set::set_union(s1,s2)))
            }),
        })
    ];
    return Vec::from(PRESET_OPNAMES.map(String::from)).into_iter().zip(opv.into_iter()).collect();
}

enum Operator {
    BinaryOp(BinaryOp),
    //UnaryOp(Box<dyn Fn(Token) -> Token>),
}

impl Operator {
    fn priority(&self) -> usize {
        match self {
            Self::BinaryOp(op) => op.priority,
        }
    }

    fn from_token(token: Token) -> Option<String> {
        for opname in PRESET_OPNAMES {
            if token.to_string() == opname {
                return Some(opname.to_string());
            }
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
            if token == Token::SymbolToken("}") {
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
            if token != Token::SymbolToken(" ") {
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
            return Ok(Token::NullToken);
        }
        if self.content.len() == 1 {
            return Ok(self.content[0].clone());
        }

        let preset_opmap = preset_operators();
        let mut index: Option<usize> = None;
        let mut priority = 11;
        let mut operator: &Operator = preset_opmap.get(&"in".to_string()).unwrap();        

        for i in 0..self.content.len() {
            let token: Token = self.content[i].clone();

            // tokenがOperatorかどうか調べる
            if let Some(opname) = Operator::from_token(token.clone()) {
                // Operatorだったら優先順位を確認
                let op: &Operator = preset_opmap.get(&opname.to_string()).unwrap();
                let priority1: usize;
                match op {
                    Operator::BinaryOp(binop) => {
                        priority1 = binop.priority;
                    }        
                }
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
                let mut tv1: Vec<Token> = self.content[0..i].to_vec();
                let mut tv2: Vec<Token> = self.content[i+1..].to_vec();

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
                        return Self::from_tokenv(tv1)?.eval();
                    }
                }
            }
        }
    }
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

    fn is_in(&self, set: &Self) -> bool {
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

    fn set_union(set1: &Set, set2: &Set) -> Set {
        let mut content1 = set1.to_setlist().content;
        let mut content2 = set2.to_setlist().content;
        content1.append(&mut content2);
        let sl = SetList { content: content1 };
        return sl.uniquify();
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

