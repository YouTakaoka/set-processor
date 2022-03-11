mod constants;
mod setlike;
mod token;

pub use self::constants::*;
pub use self::setlike::*;
pub use self::token::Token;

use std::fmt;

pub const USER_TYPES: [WordType; 3] = [WordType::Bool, WordType::Set, WordType::Number];

#[derive(Clone, PartialEq)]
pub enum Word {
    Set(Set),
    Keyword(&'static str),
    Symbol(&'static str),
    Identifier(String),
    Bool(bool),
    Null,
    Frozen(FrozenWordList),
    Operator(Operator),
    Function(Function),
    ExitSignal,
    Type(WordType),
    Number(usize),
    PrintSignal(String),
}

#[derive(Clone, PartialEq)]
pub enum WordType {
    Set,
    Keyword,
    Symbol,
    Identifier,
    Bool,
    Null,
    Frozen,
    Operator,
    Function,
    ExitSignal,
    Type,
    Number,
    PrintSginal,
}

impl fmt::Display for WordType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            WordType::Set => write!(f, "Set"),
            WordType::Keyword => write!(f, "Keyword"),
            WordType::Symbol => write!(f, "Symbol"),
            WordType::Identifier => write!(f, "Identifier"),
            WordType::Bool => write!(f, "Bool"),
            WordType::Null => write!(f, "Null"),
            WordType::Frozen => write!(f, "Frozen"),
            WordType::Operator => write!(f, "Operator"),
            WordType::Function => write!(f, "Function"),
            WordType::ExitSignal => write!(f, "ExitSignal"),
            WordType::Type => write!(f, "Type"),
            WordType::Number => write!(f, "Number"),
            WordType::PrintSginal => write!(f, "PrintSignal")
        }
    }
}

impl fmt::Display for Word {
    fn fmt(self: &Self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Word {
    pub fn from_token(token: Token) -> Word {
        match token {
            Token::Keyword(s) => {
                if let Some(op) = preset_operators().get(s) {
                    return Word::Operator(op.clone());
                }

                if let Some(f) = preset_functions().get(s) {
                    return Word::Function(Function::Preset(f.clone()));
                }

                for b in vec![true, false] {
                    if s == b.to_string() {
                        return Word::Bool(b);
                    }
                }

                for t in USER_TYPES {
                    if s == t.to_string() {
                        return Word::Type(t);
                    }
                }

                return Word::Keyword(s);
            },
            Token::Symbol(s) => {
                if let Some(op) = preset_operators().get(s) {
                    return Word::Operator(op.clone());
                }

                if let Some(f) = preset_functions().get(s) {
                    return Word::Function(Function::Preset(f.clone()));
                }
                
                return Word::Symbol(s);
            },
            Token::Identifier(s) => {
                return Word::Identifier(s);
            },
            Token::Number(n) => {
                return Word::Number(n);
            }
        }
    }

    pub fn get_type(&self) -> WordType {
        match self {
            Word::Set(_) => WordType::Set,
            Word::Keyword(_) => WordType::Keyword,
            Word::Symbol(_) => WordType::Symbol,
            Word::Identifier(_) => WordType::Identifier,
            Word::Bool(_) => WordType::Bool,
            Word::Null => WordType::Null,
            Word::Frozen(_) => WordType::Frozen,
            Word::Operator(_) => WordType::Operator,
            Word::Function(_) => WordType::Function,
            Word::ExitSignal => WordType::ExitSignal,
            Word::Type(_) => WordType::Type,
            Word::Number(_) => WordType::Number,
            Word::PrintSignal(_) => WordType::PrintSginal,
        }
    }
   
    pub fn to_set(&self, mes: &str) -> Result<Set, String> {
        match self {
            Word::Set(set) => Ok(set.clone()),
            _ => Err(mes.to_string()),
        }
    }

    pub fn to_bool(&self, mes: &str) -> Result<bool, String> {
        match self {
            Word::Bool(b) => Ok(*b),
            _ => Err(mes.to_string()),
        }
    }

    pub fn to_operator(&self, mes: &str) -> Result<Operator, String> {
        match self {
            Word::Operator(op) => Ok(op.clone()),
            _ => Err(mes.to_string()),
        }
    }

    pub fn to_number(&self, mes: &str) -> Result<usize, String> {
        match self {
            Word::Number(n) => Ok(*n),
            _ => Err(mes.to_string()),
        }
    }

    pub fn to_type(&self, mes: &str) -> Result<WordType, String> {
        match self {
            Word::Type(t) => Ok(t.clone()),
            _ => Err(mes.to_string()),
        }
    }

    pub fn find_word(wordv: &Vec<Word>, word: Word) -> Option<usize> {
        for i in 0..wordv.len() {
            if wordv[i] == word {
                return Some(i);
            }
        }
        return None;
    }

    pub fn explode(&self, wordv: &Vec<Word>) -> Vec<Vec<Word>> {
        let mut wv = wordv.clone();
        let mut ret = Vec::new();

        while let Some(i) = Self::find_word(&wv, self.clone()) {
            let (wv1, wv2) = split_drop(&wv, i, i);
            ret.push(wv1);
            wv = wv2;
        }
        ret.push(wv);
        
        return ret;
    }

    pub fn explode_each(&self, wordv: &Vec<Word>, s: &str) -> Result<Vec<Word>, String> {
        let mut ret = Vec::new();
        for wv in self.explode(wordv) {
            if wv.len() != 1 {
                return Err(s.to_string());
            }
            ret.push(wv[0].clone());
        }

        return Ok(ret);
    }

    pub fn split(&self, wv: &Vec<Word>) -> Option<(Vec<Word>, Vec<Word>)> {
        if let Some(i) = Word::find_word(wv, self.clone()) {
            let (wv1, wv2) = split_drop(wv, i, i);
            return Some((wv1, wv2));
        }
        return None;
    }

    pub fn to_string(self: &Self) -> String {
        match self {
            Self::Set(set) => set.to_string(),
            Self::Symbol(s) => s.to_string(),
            Self::Keyword(s) => s.to_string(),
            Self::Identifier(s) => s.to_string(),
            Self::Bool(b) => b.to_string(),
            Self::Null => "".to_string(),
            Self::Frozen(fwl) => fwl.to_string(),
            Self::Operator(op) => op.name(),
            Self::Function(f) => f.to_string(),
            Self::ExitSignal => "(ExitSignal)".to_string(),
            Self::Type(t) => t.to_string(),
            Self::Number(n) => n.to_string(),
            Self::PrintSignal(s) => s.clone(),
        }
    }

    pub fn wordv_to_string(wv: &Vec<Word>) -> String {
        let mut s = "".to_string();
        for word in wv {
            s = format!("{}'{}',", s, word.to_string());
        }
        if !s.is_empty() {
            s.pop();
        }
        s = format!("[{}]", s);
        return s;
    }

    pub fn find_bracket(wordv: &Vec<Word>, tb: Word, te: Word) -> Result<Option<(usize, usize)>, String> {
        let mut ib: Option<usize> = None;
        let mut cnt = 0;

        for i in 0..wordv.len() {
            if wordv[i] == te {
                if cnt < 1 {
                    return Err("Bracket is not closed.".to_string());
                } else if cnt == 1 {
                    return Ok(Some((ib.unwrap(), i)));
                }
                cnt -= 1;
            } else if wordv[i] == tb {
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

pub fn split_drop(wordv: &Vec<Word>, i1: usize, i2: usize) -> (Vec<Word>, Vec<Word>) {
    if i1 > i2 {
        panic!("i1 is greater than i2. i1={} while i2={}", i1, i2);
    }

    if wordv.len() < i2+1 {
        panic!("Length of wordv is too short. wordv.len()={}, but i2={}.", wordv.len(), i2);
    }

    let wv1 = wordv[0..i1].to_vec();

    let wv2;
    if wordv.len() >= i2+2 {
        wv2 = wordv[i2+1..].to_vec();
    } else {
        wv2 = Vec::new();
    }
    
    return (wv1, wv2);
}

pub fn subst_range(wordv: &Vec<Word>, i1: usize, i2: usize, word: Word) -> Vec<Word> {
    let (wv1, wv2) = split_drop(wordv, i1, i2);
    let mut wv = wv1.clone();
    wv.push(word);
    wv.append(&mut wv2.clone());
    return wv;
}

#[derive(Clone, PartialEq)]
pub enum Env {
    Line,
    Main,
    Set,
    Bracket,
    Scope,
}

impl Env {
    pub fn from_bound(b: String, e: String) -> Result<Self, String> {
        if b == "(" && e == ")" {
            return Ok(Env::Bracket);
        } else if b == "{" && e == "}" {
            return Ok(Env::Set);
        }

        panic!("Frozen bound didn't match any of preset type.");
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Env::Line => "Line",
            Env::Main => "Main",
            Env::Set => "Set",
            Env::Bracket => "Bracket",
            Env::Scope => "Scope",
        };
        return write!(f, "{}", s);
    }
}

#[derive(Clone, PartialEq)]
pub struct FrozenWordList {
    contents: Vec<Word>,
    env: Env,
}

impl FrozenWordList {
    fn find_frozenbound(wv: &Vec<Word>) -> Result<Option<(usize, usize)>, String> {
        let mut i_bound: Option<(usize, usize)> = None;

        for (b, e) in FROZEN_BOUND {
            if let Some((ib, ie)) = Word::find_bracket(wv, Word::Symbol(b), Word::Symbol(e))? {
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

    pub fn from_wordv(wv: Vec<Word>, env: Env) -> Result<Self, String> {
        let mut contents = wv;

        while let Some((ib, ie)) = Self::find_frozenbound(&contents)? {
            let b = contents[ib].to_string();
            let e = contents[ie].to_string();
            let mut env = Env::from_bound(b, e)?;
            if env == Env::Bracket && ib > 0 {
                if let Some(Word::Symbol("$")) = contents.get(ib - 1) {
                    env = Env::Scope;
                }
            }

            let (wv_other, mut wv3) = split_drop(&contents, ie, ie);
            let (mut wv1, wv2) = split_drop(&wv_other, ib, ib);
            if env == Env::Scope {
                wv1.pop();
            }

            let word = Word::Frozen(Self::from_wordv(wv2, env)?);
            wv1.push(word);
            wv1.append(&mut wv3);
            contents = wv1;
        }
        
        return Ok(Self {
            contents: contents,
            env: env.clone(),
        })
    }

    pub fn from_tokenv(tv: Vec<Token>, env: Env) -> Result<Self, String> {
        let mut wv = Vec::new();

        for t in tv {
            let w = Word::from_token(t);
            wv.push(w);
        }

        return Self::from_wordv(wv, env);
    }

    pub fn from_string(string: &String) -> Result<Self, String> {
        return Self::from_tokenv(Token::tokenize(string)?, Env::Line);
    }

    pub fn is_empty(&self) -> bool {
        return self.contents.is_empty();
    }

    pub fn len(&self) -> usize {
        return self.contents.len();
    }

    pub fn get(&self, i: usize) -> Option<Word> {
        return Some(self.contents.get(i)?.clone());
    }

    pub fn get_contents(&self) -> Vec<Word> {
        return self.contents.clone();
    }

    pub fn get_env(&self) -> Env {
        return self.env.clone();
    }

    pub fn env_is(&self, env: Env) -> bool {
        return self.get_env() == env;
    }

    pub fn to_string(&self) -> String {
        let mut string = String::new();

        for word in self.get_contents() {
            string = format!("{}, {}", string, word.to_string());
        }

        if !string.is_empty() {
            string.remove(0);
        }
        string = format!("[{}]", string);

        return string;
    }
}

//------------------------------------------------
//            ここからOperator, PresetFunction
//------------------------------------------------

#[derive(Clone)]
pub struct BinarySig {
    args: (WordType, WordType),
    ret: WordType,
}

impl BinarySig {
    pub fn new(args: (WordType, WordType), ret: WordType) -> Self {
        return Self {args: args, ret: ret};
    }
}

#[derive(Clone)]
pub struct BinaryOp {
    name: String,
    fs: Vec<(BinarySig, fn(Word, Word) -> Result<Word, String>)>,
    priority: usize,
}

impl BinaryOp {
    pub fn name(&self) -> String {
        return self.name.clone();
    }

    pub fn accepts(&self) -> String {
        let mut string = String::new();

        for (sig, _) in self.fs.clone() {
            let (t1, t2) = sig.args;
            string = format!("{},({},{})", string, t1, t2);
        }

        if string.is_empty() {
            panic!("Variable 'string' is empty somehow.");
        }
        string.remove(0);
        string = format!("[{}]", string);

        return string;
    }

    pub fn apply(&self, w1: Word, w2: Word) -> Result<Word, String> {
        for (sig, f) in self.fs.clone() {
            if (w1.get_type(), w2.get_type()) == sig.args {
                return f(w1, w2);
            }
        }
        return Err(format!("Bianry operator '{}': Type error. Expected one of pairs of types {}, got ({},{}).",
                    self.name(), self.accepts(), w1.get_type(), w2.get_type()));
    }
}

#[derive(Clone)]
pub struct UnarySig {
    arg: WordType,
    ret: WordType,
}

impl UnarySig {
    pub fn new(arg: WordType, ret: WordType) -> Self {
        return Self {arg: arg, ret: ret};
    }
}

#[derive(Clone)]
pub struct UnaryOp {
    name: String,
    fs: Vec<(UnarySig, fn(Word) -> Result<Word, String>)>,
    priority: usize,
}

impl UnaryOp {
    pub fn name(&self) -> String {
        return self.name.clone();
    }

    pub fn apply(&self, w: Word) -> Result<Word, String> {
        for (sig, f) in self.fs.clone() {
            if sig.arg == w.get_type() {
                return f(w);
            }
        }
        return Err(format!("Unary operator '{}': Type error at the first argument. Expected one of {}, but got {}.",
                    self.name(), self.accepts(), w.get_type()));
    }

    pub fn accepts(&self) -> String {
        let mut string = String::new();
        
        for (sig, _) in self.fs.clone() {
            string = format!("{},{}", string, sig.arg);
        }

        if string.is_empty() {
            panic!("Variable 'string' is empty somehow.");
        }
        string.remove(0);
        string = format!("[{}]", string);

        return string;
    }
}

// presetの演算子はここに追加していく
pub fn preset_operators() -> std::collections::HashMap<String, Operator> {
    let opv = vec![
        Operator::UnaryOp(UnaryOp {
            name: "!".to_string(),
            fs: vec![
                    (UnarySig::new(WordType::Bool, WordType::Bool),
                    |w: Word| {
                        let b = w.to_bool("")?;
                        Ok(Word::Bool(!b))
                    })
                ],
            priority: 5,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "||".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Bool, WordType::Bool), WordType::Bool),
                |w1: Word, w2: Word| {
                    let b1 = w1.to_bool("")?;
                    let b2 = w2.to_bool("")?;
                    Ok(Word::Bool(b1 || b2))
                }),
            ],
            priority: 7,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "&&".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Bool, WordType::Bool), WordType::Bool),
                |w1: Word, w2: Word| {
                    let b1 = w1.to_bool("")?;
                    let b2 = w2.to_bool("")?;
                    Ok(Word::Bool(b1 && b2))
                }),
            ],
            priority: 6,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "==".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Bool),
                |w1: Word, w2: Word| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(Word::Bool(s1 == s2))
                }),
                (BinarySig::new((WordType::Bool, WordType::Bool), WordType::Bool),
                |w1: Word, w2: Word| {
                    let s1 = w1.to_bool("")?;
                    let s2 = w2.to_bool("")?;
                    Ok(Word::Bool(s1 == s2))
                }),
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: Word, w2: Word| {
                    let s1 = w1.to_number("")?;
                    let s2 = w2.to_number("")?;
                    Ok(Word::Bool(s1 == s2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "!=".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Bool),
                |w1: Word, w2: Word| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(Word::Bool(s1 != s2))
                }),
                (BinarySig::new((WordType::Bool, WordType::Bool), WordType::Bool),
                |w1: Word, w2: Word| {
                    let s1 = w1.to_bool("")?;
                    let s2 = w2.to_bool("")?;
                    Ok(Word::Bool(s1 != s2))
                }),
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: Word, w2: Word| {
                    let s1 = w1.to_number("")?;
                    let s2 = w2.to_number("")?;
                    Ok(Word::Bool(s1 != s2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "<".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: Word, w2: Word| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(Word::Bool(n1 < n2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: ">".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: Word, w2: Word| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(Word::Bool(n1 > n2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "<=".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: Word, w2: Word| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(Word::Bool(n1 <= n2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: ">=".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: Word, w2: Word| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(Word::Bool(n1 >= n2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "in".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Bool),
                |w1: Word, w2: Word| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(Word::Bool(s1.is_in(&s2)))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "-".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Set),
                |w1: Word, w2: Word| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(Word::Set(Set::set_diff(&s1,&s2)))
                }),
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Number),
                |w1: Word, w2: Word| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    if n1 >= n2 {
                        Ok(Word::Number(n1 - n2))
                    } else {
                        Err(format!("Value error: The left hand side of binary operator '-' is '{}', which is less than the right hand side '{}'.", n1, n2))
                    }
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "+".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Set),
                |w1: Word, w2: Word| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(Word::Set(Set::set_union(&s1,&s2)))
                }),
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Number),
                |w1: Word, w2: Word| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(Word::Number(n1 + n2))
                }),
            ],
            priority: 3,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "*".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Set),
                |w1: Word, w2: Word| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(Word::Set(Set::set_intersec(&s1,&s2)))
                }),
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Number),
                |w1: Word, w2: Word| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(Word::Number(n1 * n2))
                }),
            ],
            priority: 2,
        }),
        Operator::UnaryOp(UnaryOp {
            name: "#".to_string(),
            fs: vec![
                    (UnarySig::new(WordType::Set, WordType::Number),
                    |w: Word| {
                        let set = w.to_set("")?;
                        Ok(Word::Number(set.len()))
                    }),
                ],
            priority: 1,
        }),
        Operator::UnaryOp(UnaryOp {
            name: "print".to_string(),
            fs: vec![
                    (UnarySig::new(WordType::Set, WordType::PrintSginal),
                    |w: Word| {
                        let s = w.to_string();
                        Ok(Word::PrintSignal(s))
                    }),
                    (UnarySig::new(WordType::Number, WordType::PrintSginal),
                    |w: Word| {
                        let s = w.to_string();
                        Ok(Word::PrintSignal(s))
                    }),
                    (UnarySig::new(WordType::Bool, WordType::PrintSginal),
                    |w: Word| {
                        let s = w.to_string();
                        Ok(Word::PrintSignal(s))
                    }),
                ],
            priority: 10,
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

    pub fn from_word(word: Word) -> Option<Self> {
        let preset_opmap = preset_operators();
        for opname in preset_opmap.keys() {
            if word.to_string() == opname.clone() {
                return Some(preset_opmap.get(opname)?.clone());
            }
        }
        return None;
    }
}

#[derive(Clone, PartialEq)]
pub enum Function {
    Preset(PresetFunction),
    User(UserFunction),
}

impl Function {
    pub fn to_string(&self) -> String {
        match self {
            Function::Preset(f) => f.to_string(),
            Function::User(f) => f.to_string(),
        }
    }

    pub fn sig(&self) -> Signature {
        match self {
            Function::Preset(f) => f.sig(),
            Function::User(f) => f.get_sig(),
        }
    }

    pub fn type_check(&self, wv: Vec<Word>) -> Result<(), String>{
        if wv.len() != self.sig().args.len() {
            return Err(format!("Function {}: Number of argument(s) mismatch. Expected {} argument(s), got {}.",
                        self.to_string(), self.sig().args.len(), wv.len()));
        }

        for i in 0..wv.len() {
            if wv[i].get_type() != self.sig().args[i] {
                return Err(format!("Function {}: Type mismatch at the {}-th argument. Expected {}, got {}.",
                            self.to_string(), i+1, self.sig().args[i], wv[i].get_type()));
            }
        }

        return Ok(());
    }
}

#[derive(Clone, PartialEq)]
pub struct PresetFunction {
    name: Option<String>,
    f: fn(Vec<Word>) -> Result<Word, String>,
    sig: Signature, 
}

impl PresetFunction {
    pub fn apply(&self, wv: Vec<Word>) -> Result<Word, String> {
        return (self.f)(wv);
    }

    pub fn name(&self) -> Option<String> {
        return self.name.clone();
    }

    pub fn sig(&self) -> Signature {
        return self.sig.clone();
    }

    pub fn to_string(&self) -> String {
        let mut name = "(anonymous function)".to_string();
        if let Some(n) = self.name() {
            name = n;
        }
        return format!("{}: {}", name, self.sig());
    }
}

// presetの関数はここに追加
pub fn preset_functions() -> std::collections::HashMap<String, PresetFunction> {
    let funcv = vec![
        PresetFunction {
            name: Some("is_empty".to_string()),
            sig: Signature::new(vec![WordType::Set], WordType::Bool),
            f: |wv: Vec<Word>| {
                let set = wv[0].to_set("")?;
                return Ok(Word::Bool(set.is_empty()));
            }
        },
        PresetFunction {
            name: Some("exit".to_string()),
            sig: Signature::new(vec![], WordType::ExitSignal),
            f: |_: Vec<Word>| {
                return Ok(Word::ExitSignal);
            }
        },
    ];

    let funcnames: Vec<String> = funcv.iter().map(|x| x.name().unwrap()).collect();

    return funcnames.into_iter().zip(funcv.into_iter()).collect();
}

#[derive(Clone, PartialEq)]
pub struct Signature {
    args: Vec<WordType>,
    ret: WordType,
}

impl Signature {
    pub fn new(args: Vec<WordType>, ret: WordType) -> Self {
        return Self {args: args, ret: ret}
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", vec_to_string(self.args.clone()), self.ret)
    }
}

pub fn vec_to_string<T: ToString>(vec: Vec<T>) -> String {
    let mut string = String::new();
    for v in vec {
        string = format!("{},{}", string, v.to_string())
    }
    if !string.is_empty() {
        string.remove(0);
    }
    
    return string;
}

#[derive(Clone, PartialEq)]
pub struct UserFunction {
    name: Option<String>,
    sig: Signature,
    xv: Vec<String>,
    expr: Vec<Word>,
}

impl UserFunction {
    pub fn new(name: Option<String>, sig: Signature, xv: Vec<String>, expr: Vec<Word>) -> Self {
        return Self {name: name, sig: sig, xv: xv, expr: expr};
    }

    pub fn get_name(&self) -> Option<String> {
        return self.name.clone();
    }

    pub fn get_xv(&self) -> Vec<String> {
        return self.xv.clone();
    }

    pub fn get_sig(&self) -> Signature {
        return self.sig.clone();
    }

    pub fn get_expr(&self) -> Vec<Word> {
        return self.expr.clone();
    }

    pub fn to_word(&self) -> Word {
        return Word::Function(Function::User(self.clone()));
    }

    pub fn to_string(&self) -> String {
        let mut name = "(anonymous function)".to_string();
        if let Some(n) = self.get_name() {
            name = n;
        }

        return format!("{}: {}", name, self.get_sig());
    }
}
