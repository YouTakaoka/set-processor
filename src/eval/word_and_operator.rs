mod constants;
mod setlike;
mod token;

pub use self::constants::*;
pub use self::setlike::*;
pub use self::token::*;

use std::fmt;

pub const USER_TYPES: [WordType; 3] = [WordType::Bool, WordType::Set, WordType::Number];

pub trait WordKind<T: Clone + fmt::Display + PartialEq> {
    fn is_wordtype() -> bool;
    fn get_type(&self) -> WordType;
    fn null() -> Self;
    fn to_set(&self, mes: &str) -> Result<Set, String>;
    fn to_bool(&self, mes: &str) -> Result<bool, String>;
    fn from_userf(uf: UserFunction<T>) -> Self;
    fn from_word(w: Word) -> Self;
    fn to_func(&self, mes: &str) -> Result<Function<T>, String>;
    fn to_frozen(&self, mes: &str) -> Result<Frozen<T>, String>;
    fn to_operator(&self, mes: &str) -> Result<Operator<T>, String>;
    fn to_number(&self, mes: &str) -> Result<usize, String>;
    fn to_keyword(&self, mes: &str) -> Result<&'static str, String>;
    fn vec_to_frozen(vec: Vec<T>, env: &Env) -> Result<Frozen<T>, String>;
    fn to_identifier(&self, mes: &str) -> Result<String, String>;
    fn to_type(&self, mes: &str) -> Result<WordType, String>;
    fn explode(&self, wordv: &Vec<T>) -> Vec<Vec<T>>;
    fn find_word(wordv: &Vec<T>, word: Self) -> Option<usize>;
    fn from_symbol(s: &'static str) -> Self;
    fn from_keyword(s: &'static str) -> Self;
    fn from_set(set: Set) -> Self;
    fn from_bool(b: bool) -> Self;
    fn from_number(n: usize) -> Self;
    fn from_printsignal(s: String) -> Self;
    fn from_wordtype(wt: WordType) -> Self;
    fn from_function(f: Function<T>) -> Self;
    fn find_bracket(wordv: &Vec<T>, tb: Self, te: Self) -> Result<Option<(usize, usize)>, String>;
}

#[derive(Clone, PartialEq, Debug)]
pub enum Word {
    Set(Set),
    Keyword(&'static str),
    Symbol(&'static str),
    Identifier(String),
    Bool(bool),
    Null,
    Frozen(Frozen<Word>),
    Operator(Operator<Word>),
    Function(Function<Word>),
    ExitSignal,
    Type(WordType),
    Number(usize),
    PrintSignal(String),
}

fn find<T: PartialEq>(elemv: &Vec<T>, elem: T) -> Option<usize> {
    for i in 0..elemv.len() {
        if elemv[i] == elem {
            return Some(i);
        }
    }
    return None;
}

pub fn split<T: Clone + PartialEq>(d: T, vec: &Vec<T>) -> Option<(Vec<T>, Vec<T>)> {
    if let Some(i) = find(vec, d) {
        let (vec1, vec2) = split_drop(vec, i, i);
        return Some((vec1, vec2));
    }
    return None;
}

impl WordKind<Word> for Word {
    fn to_type(&self, mes: &str) -> Result<WordType, String> {
        match self {
            Word::Type(t) => Ok(t.clone()),
            _ => Err(mes.to_string()),
        }
    }

    fn find_bracket(wordv: &Vec<Word>, tb: Word, te: Word) -> Result<Option<(usize, usize)>, String> {
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

    fn to_keyword(&self, mes: &str) -> Result<&'static str, String> {
        match self {
            Self::Keyword(kw) => Ok(kw),
            _ => Err(mes.to_string()),
        }
    }

    fn from_function(f: Function<Self>) -> Self {
        return Self::Function(f);
    }

    fn from_wordtype(_: WordType) -> Self {
        return Word::Null;
    }

    fn from_printsignal(s: String) -> Self {
        return Self::PrintSignal(s);
    }

    fn from_number(n: usize) -> Self {
        return Self::Number(n);
    }

    fn to_number(&self, mes: &str) -> Result<usize, String> {
        match self {
            Word::Number(n) => Ok(*n),
            _ => Err(mes.to_string()),
        }
    }

    fn from_bool(b: bool) -> Self {
        return Self::Bool(b);
    }

    fn to_operator(&self, mes: &str) -> Result<Operator<Word>, String> {
        match self {
            Self::Operator(op) => Ok(op.clone()),
            _ => Err(mes.to_string()),
        }
    }

    fn to_func(&self, mes: &str) -> Result<Function<Self>, String> {
        match self {
            Self::Function(f) => Ok(f.clone()),
            _ => Err(mes.to_string()),
        }
    }

    fn to_bool(&self, mes: &str) -> Result<bool, String> {
        match self {
            Word::Bool(b) => Ok(*b),
            _ => Err(mes.to_string()),
        }
    }

    fn from_set(set: Set) -> Self {
        return Word::Set(set);
    }

    fn from_keyword(s: &'static str) -> Self {
        return Word::Keyword(s);
    }

    fn is_wordtype() -> bool {
        return false;
    }

    fn get_type(&self) -> WordType {
        match self {
            Word::Set(_) => WordType::Set,
            Word::Keyword(s) => WordType::Keyword(s),
            Word::Symbol(s) => WordType::Symbol(s),
            Word::Identifier(s) => WordType::Identifier(s.clone()),
            Word::Bool(_) => WordType::Bool,
            Word::Null => WordType::Null,
            Word::Frozen(frozen) => WordType::Frozen(Box::new(frozen.to_wordtype())),
            Word::Operator(op) => WordType::Operator(op.to_wordtype()),
            Word::Function(f) => WordType::Function(Box::new(f.sig())),
            Word::ExitSignal => WordType::ExitSignal,
            Word::Type(_) => WordType::Type,
            Word::Number(_) => WordType::Number,
            Word::PrintSignal(_) => WordType::PrintSignal,
        }
    }

    fn null() -> Self {
        return Word::Null;
    }

    fn to_set(&self, mes: &str) -> Result<Set, String> {
        match self {
            Word::Set(set) => Ok(set.clone()),
            _ => Err(mes.to_string()),
        }
    }

    fn from_userf(uf: UserFunction<Self>) -> Self {
        return Word::Function(Function::User(uf));
    }

    fn from_word(w: Word) -> Self {
        return w;
    }

    fn to_frozen(&self, mes: &str) -> Result<Frozen<Self>, String> {
        match self {
            Self::Frozen(frozen) => Ok(frozen.clone()),
            _ => Err(mes.to_string()),
        }
    }

    fn vec_to_frozen(wv: Vec<Self>, env: &Env) -> Result<Frozen<Self>, String> {
        let mut contents = wv;

        while let Some((ib, ie)) = FrozenWordList::find_fwlbound(&contents)? {
            let b = contents[ib].to_string();
            let e = contents[ie].to_string();
            let mut env1 = Env::from_bound(b, e)?;
            if env1 == Env::Bracket && ib > 0 {
                if let Some(Word::Symbol("$")) = contents.get(ib - 1) {
                    env1 = Env::Scope;
                }
            }

            let (wv_other, mut wv3) = split_drop(&contents, ie, ie);
            let (mut wv1, wv2) = split_drop(&wv_other, ib, ib);
            if env1 == Env::Scope {
                wv1.pop();
            }

            let word = Word::Frozen(Self::vec_to_frozen(wv2, &env1)?);
            wv1.push(word);
            wv1.append(&mut wv3);
            contents = wv1;
        }
        
        return Ok(
            FrozenWordList {
                contents: contents,
                env: env.clone(),
            }.to_frozen()?
        ) 
    }

    fn to_identifier(&self, mes: &str) -> Result<String, String> {
        match self {
            Self::Identifier(s) => Ok(s.clone()),
            _ => Err(mes.to_string()),
        }
    }

    fn explode(&self, wordv: &Vec<Word>) -> Vec<Vec<Word>> {
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

    fn find_word(wordv: &Vec<Word>, word: Word) -> Option<usize> {
        for i in 0..wordv.len() {
            if wordv[i] == word {
                return Some(i);
            }
        }
        return None;
    }

    fn from_symbol(s: &'static str) -> Self {
        return Word::Symbol(s);
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum WordType {
    Set,
    Keyword(&'static str),
    Symbol(&'static str),
    Identifier(String),
    Bool,
    Null,
    Frozen(Box<Frozen<WordType>>),
    Operator(Operator<WordType>),
    Function(Box<Signature>),
    ExitSignal,
    Type,
    Number,
    PrintSignal,
}

impl WordKind<WordType> for WordType {
    fn to_type(&self, _: &str) -> Result<WordType, String> {
        return Ok(self.clone());
    }

    fn find_bracket(wordv: &Vec<WordType>, tb: WordType, te: WordType) -> Result<Option<(usize, usize)>, String> {
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

    fn to_keyword(&self, mes: &str) -> Result<&'static str, String> {
        match self {
            Self::Keyword(kw) => Ok(kw),
            _ => Err(mes.to_string()),
        }
    }

    fn from_function(f: Function<Self>) -> Self {
        return Self::Function(Box::new(f.sig()));
    }

    fn from_wordtype(wt: WordType) -> Self {
        return wt;
    }

    fn from_printsignal(_: String) -> Self {
        return Self::PrintSignal;
    }

    fn from_number(_: usize) -> Self {
        return Self::Number;
    }

    fn to_number(&self, mes: &str) -> Result<usize, String> {
        match self {
            Self::Number => Ok(0),
            _ => Err(mes.to_string()),
        }
    }

    fn from_bool(_: bool) -> Self {
        return Self::Bool;
    }

    fn vec_to_frozen(vec: Vec<Self>, env: &Env) -> Result<Frozen<Self>, String> {
        return Ok(FrozenWordList {
            contents: vec,
            env: env.clone(),
        }.to_frozen()?)
    }

    fn to_operator(&self, mes: &str) -> Result<Operator<WordType>, String> {
        match self {
            Self::Operator(op) => Ok(op.clone()),
            _ => Err(mes.to_string()),
        }
    }

    fn to_func(&self, mes: &str) -> Result<Function<Self>, String> {
        match self {
            Self::Function(sig) => Ok(Function::User(UserFunction::new(
                None,
                *sig.clone(),
                vec![],
                vec![]
            ))),
            _ => Err(mes.to_string()),
        }
    }

    fn from_set(_: Set) -> Self {
        return Self::Set;
    }

    fn from_keyword(s: &'static str) -> Self {
        return WordType::Keyword(s);
    }

    fn is_wordtype() -> bool {
        return true;
    }

    fn get_type(&self) -> WordType {
        return self.clone();
    }

    fn null() -> Self {
        return Self::Null;
    }

    fn to_set(&self, mes: &str) -> Result<Set, String> {
        if self.clone() == WordType::Set {
            return Ok(Set::new(vec![]));
        } else {
            return Err(mes.to_string());
        }
    }

    fn to_bool(&self, mes: &str) -> Result<bool, String> {
        if self.clone() == WordType::Bool {
            return Ok(true);
        } else {
            return Err(mes.to_string());
        }
    }

    fn from_userf(uf: UserFunction<Self>) -> Self {
        return WordType::Function(Box::new(uf.get_sig()));
    }

    fn from_word(w: Word) -> Self {
        return w.get_type();
    }

    fn to_frozen(&self, mes: &str) -> Result<Frozen<Self>, String> {
        match self {
            Self::Frozen(frozen) => Ok(*frozen.clone()),
            _ => Err(mes.to_string()),
        }
    }

    fn to_identifier(&self, mes: &str) -> Result<String, String> {
        match self {
            WordType::Identifier(s) => Ok(s.clone()),
            _ => Err(mes.to_string()),
        }
    }

    fn explode(&self, wordv: &Vec<Self>) -> Vec<Vec<Self>> {
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

    fn find_word(wordv: &Vec<Self>, word: Self) -> Option<usize> {
        for i in 0..wordv.len() {
            if wordv[i] == word {
                return Some(i);
            }
        }
        return None;
    }

    fn from_symbol(s: &'static str) -> Self {
        return WordType::Symbol(s);
    }
}

impl fmt::Display for WordType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            WordType::Set => write!(f, "Set"),
            WordType::Keyword(kw) => write!(f, "{}", kw),
            WordType::Symbol(sym) => write!(f, "{}", sym),
            WordType::Identifier(id) => write!(f, "{}", id),
            WordType::Bool => write!(f, "Bool"),
            WordType::Null => write!(f, "Null"),
            WordType::Frozen(_) => write!(f, "Frozen"),
            WordType::Operator(_) => write!(f, "Operator"),
            WordType::Function(_) => write!(f, "Function"),
            WordType::ExitSignal => write!(f, "ExitSignal"),
            WordType::Type => write!(f, "Type"),
            WordType::Number => write!(f, "Number"),
            WordType::PrintSignal => write!(f, "PrintSignal")
        }
    }
}

impl fmt::Display for Word {
    fn fmt(self: &Self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Word {
    pub fn vec_to_type(wv: &Vec<Word>) -> Vec<WordType> {
        let mut wtv = Vec::new();
        for w in wv {
            wtv.push(w.get_type());
        }
        return wtv;
    }

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
    
    pub fn to_operator(&self, mes: &str) -> Result<Operator<Word>, String> {
        match self {
            Word::Operator(op) => Ok(op.clone()),
            _ => Err(mes.to_string()),
        }
    }

    pub fn to_string(self: &Self) -> String {
        match self {
            Self::Set(set) => set.to_string(),
            Self::Symbol(s) => s.to_string(),
            Self::Keyword(s) => s.to_string(),
            Self::Identifier(s) => s.to_string(),
            Self::Bool(b) => b.to_string(),
            Self::Null => "".to_string(),
            Self::Frozen(frozen) => frozen.to_string(),
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
}

fn explode<T: PartialEq + Clone>(elem: T, vec: &Vec<T>) -> Vec<Vec<T>> {
    let mut v = vec.clone();
    let mut ret = Vec::new();

    while let Some(i) = find(&v, elem.clone()) {
        let (v1, v2) = split_drop(&v, i, i);
        ret.push(v1);
        v = v2;
    }
    ret.push(v);
    
    return ret;
}

pub fn explode_each<T: PartialEq + Clone>(elem: T, vec: &Vec<T>, s: &str) -> Result<Vec<T>, String> {
    let mut ret = Vec::new();
    for v in explode(elem, vec) {
        if v.len() != 1 {
            return Err(s.to_string());
        }
        ret.push(v[0].clone());
    }

    return Ok(ret);
}

pub fn split_drop<T: Clone>(wordv: &Vec<T>, i1: usize, i2: usize) -> (Vec<T>, Vec<T>) {
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

pub fn subst_range<T: Clone>(wordv: &Vec<T>, i1: usize, i2: usize, word: T) -> Vec<T> {
    let (wv1, wv2) = split_drop(wordv, i1, i2);
    let mut wv = wv1.clone();
    wv.push(word);
    wv.append(&mut wv2.clone());
    return wv;
}

#[derive(Clone, PartialEq, Debug)]
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

#[derive(Clone, PartialEq, Debug)]
pub enum Frozen<T> {
    WordList(FrozenWordList<T>),
    IfExpr(IfExpr<T>),
    Scope(Vec<Frozen<T>>),
    LetExpr(LetExpr<T>),
    FuncDef(FuncDef<T>),
    Bracket(Vec<Frozen<T>>),
}

impl<T: Clone + PartialEq + fmt::Display> Frozen<T> {
    pub fn to_string(&self) -> String {
        match self {
            Self::WordList(fwl) => fwl.to_string(),
            Self::FuncDef(_) => "(funcdef)".to_string(),
            Self::IfExpr(_) => "(if-expr)".to_string(),
            Self::LetExpr(_) => "(let-expr)".to_string(),
            Self::Scope(_) => "(scope)".to_string(),
            Self::Bracket(_) => "(bracket)".to_string(),
        }
    }
}

impl Frozen<Word> {
    pub fn from_string(string: &String) -> Result<Self, String> {
        return Self::from_tokenv(Token::tokenize(string)?, Env::Line);
    }

    pub fn from_tokenv(tv: Vec<Token>, env: Env) -> Result<Self, String> {
        let mut wv = Vec::new();

        for t in tv {
            let w = Word::from_token(t);
            wv.push(w);
        }

        return Word::vec_to_frozen(wv, &env);
    }

    pub fn to_wordtype(&self) -> Frozen<WordType> {
        match self {
            Self::WordList(fwl) => Frozen::WordList(fwl.to_wordtype()),
            Self::FuncDef(fd) => Frozen::FuncDef(fd.to_wordtype()),
            Self::LetExpr(le) => Frozen::LetExpr(le.to_wordtype()),
            Self::IfExpr(ie) => Frozen::IfExpr(ie.to_wordtype()),
            Self::Scope(fv) => {
                let mut ftv = Vec::new();
                for frozen in fv {
                    ftv.push(frozen.to_wordtype());
                }
                return Frozen::Scope(ftv);
            },
            Self::Bracket(fv) => {
                let mut ftv = Vec::new();
                for frozen in fv {
                    ftv.push(frozen.to_wordtype());
                }
                return Frozen::Bracket(ftv);
            },
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct FuncDef<T> {
    pub name: Option<String>,
    pub argtv: Vec<WordType>,
    pub rett: WordType,
    pub argv: Vec<String>,
    pub expr: Vec<T>,
}

impl FuncDef<Word> {
    pub fn to_wordtype(&self) -> FuncDef<WordType> {
        return FuncDef {
            name: self.name.clone(),
            argtv: self.argtv.clone(),
            rett: self.rett.clone(),
            argv: self.argv.clone(),
            expr: Word::vec_to_type(&self.expr),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct IfExpr<T> {
    pub wv_if: Vec<T>,
    pub wv_then: Vec<T>,
    pub wv_else: Vec<T>,
}

impl IfExpr<Word> {
    pub fn to_wordtype(&self) -> IfExpr<WordType> {
        return IfExpr {
            wv_if: Word::vec_to_type(&self.wv_if),
            wv_else: Word::vec_to_type(&self.wv_else),
            wv_then: Word::vec_to_type(&self.wv_then),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct LetExpr<T> {
    pub identifier: String,
    pub expr: Vec<T>,
}

impl LetExpr<Word> {
    pub fn to_wordtype(&self) -> LetExpr<WordType> {
        return LetExpr {
            identifier: self.identifier.clone(),
            expr: Word::vec_to_type(&self.expr),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct FrozenWordList<T> {
    contents: Vec<T>,
    env: Env,
}

impl FrozenWordList<Word> {
    fn find_fwlbound(wv: &Vec<Word>) -> Result<Option<(usize, usize)>, String> {
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
}

impl<T: Clone + PartialEq + std::fmt::Display> FrozenWordList<T> {
    pub fn is_empty(&self) -> bool {
        return self.contents.is_empty();
    }

    pub fn len(&self) -> usize {
        return self.contents.len();
    }

    pub fn get(&self, i: usize) -> Option<T> {
        return Some(self.contents.get(i)?.clone());
    }

    pub fn get_contents(&self) -> Vec<T> {
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

impl<T: Clone + PartialEq + fmt::Display + WordKind<T>> FrozenWordList<T> {
    pub fn to_frozen(&self) -> Result<Frozen<T>, String> {
        let env = self.get_env().clone();
        match env {
            Env::Line | Env::Set => (),
            Env::Bracket => {
                let wvv = T::from_symbol("|").explode(&self.get_contents());
                let mut fv = Vec::new();
                for wv in wvv {
                    fv.push(T::vec_to_frozen(wv, &Env::Line)?);
                }
                return Ok(Frozen::Bracket(fv))
            },
            Env::Scope => {
                let wvv = T::from_symbol("|").explode(&self.get_contents());
                let mut fv = Vec::new();
                for wv in wvv {
                    fv.push(T::vec_to_frozen(wv, &Env::Line)?);
                }
                return Ok(Frozen::Scope(fv))
            },
            _ => panic!("Invalid Env type {} in eval() function.", env),
        }

        let keyword;
        if let Some(w) = self.get(0) {
            if let Ok(kw) = w.to_keyword("") {
                keyword = kw;
            } else {
                return Ok(Frozen::WordList(self.clone()));
            }
        } else {
            return Ok(Frozen::WordList(self.clone()));
        }
        
        // 先頭がletキーワードだった場合の処理
        if keyword == "let" {
            if self.len() < 4 {
                return Err("Parse error: 'let' statement is too short.".to_string());
            }
    
            if self.get(2).unwrap() == T::from_symbol("=") {
                let word1 = self.get(1).unwrap();
                if let Ok(identifier) = word1.to_identifier("") {
                    // ここが中心部
                    let mut wordv = self.get_contents();
                    let expr = wordv.split_off(3);
                    return Ok(Frozen::LetExpr(LetExpr {identifier: identifier.clone(), expr: expr}));
                }
    
                // word1がIdentifierでなかった場合
                return Err(format!("Name error: Cannot use '{}' as identifier.", word1.to_string()));
            }
    
            // 2番目のトークンが'='でなかった場合
            return Err("Syntax error: Token '=' needed after 'let' keyword.".to_string());
        }
    
        // if文の処理
        if keyword == "if" {
            // then節を探す(なければerror)
            let option_then = rewrite_error(
                T::find_bracket(
                    &self.get_contents(),
                    T::from_keyword("if"),
                    T::from_keyword("then")
                ),
                "Syntax error: Keyword 'then' not found after 'if' keyword.".to_string()
            )?;
            let (_, i_then) = option_then.unwrap();
    
            // else節を探す(なければerror)
            let option_else = rewrite_error(
                T::find_bracket(
                    &self.get_contents(),
                    T::from_keyword("if"),
                    T::from_keyword("else")
                ),
                "Syntax error: Keyword 'else' not found after 'if' keyword.".to_string()
            )?;
            let (_, i_else) = option_else.unwrap();
    
            // thenがelseより後ろならerror
            if i_then > i_else {
                return Err("Syntax error: Keyword 'else' found before 'then'.".to_string());
            }
    
            // if節、then節、else節に分解
            let wordv = self.get_contents();
            let (wordv_other, wv_else) = split_drop(&wordv, i_else, i_else);
            let (mut wv_if, wv_then) = split_drop(&wordv_other, i_then, i_then);
            wv_if.remove(0);
    
            return Ok(Frozen::IfExpr(IfExpr {wv_if: wv_if, wv_then: wv_then, wv_else: wv_else}));
        }
    
        // def(関数定義)文の処理
        if keyword == "def" {
            if self.get(2) != Some(T::from_symbol(":")) {
                return Err("Syntax error: Token ':' needed after 'def' token.".to_string());
            } else if self.get(2) == None {
                return Err("Parse error: 'def' statement is too short.".to_string());
            }
    
            if let Ok(identifier) = self.get(1).unwrap().to_identifier("") {
                // ここが中心部
                let (_, wv1) = split_drop(&self.get_contents(), 2, 2);
                let f = parse_funcdef(Some(identifier.clone()), &wv1)?;
                return Ok(Frozen::FuncDef(f));
            } else {
                let w = self.get(1).unwrap();
                return Err(format!("Name error: Token '{}' cannot used as identifier.", w));
            }
        }
    
        return Err(format!("Keyword '{}' cannot be the initial token of statement.", keyword));
    }
}

impl FrozenWordList<Word> {
    pub fn to_wordtype(&self) -> FrozenWordList<WordType> {
        let contents = Word::vec_to_type(&self.get_contents());
        return FrozenWordList {
            contents: contents,
            env: self.get_env(),
        }
    }
}

fn rewrite_error<T>(result: Result<T, String>, string: String) -> Result<T, String> {
    match result {
        Ok(val) => Ok(val),
        Err(_) => Err(string),
    }
}

fn parse_funcdef<T: WordKind<T> + Clone + fmt::Display + PartialEq>
    (identifier: Option<String>, wv: &Vec<T>) -> Result<FuncDef<T>, String> {

    if let Some((wv_types, wv_other)) = split(T::from_symbol(";"), wv) {
        if let Some((wv_argst, wv_rett)) = split(T::from_symbol("->"), &wv_types) {
            // 返り値のSignatureをつくる
            let mut argst = Vec::new();
            for w in explode_each(
                    T::from_symbol(","),
                    &wv_argst,
                    "Syntax error in the argument type part of function definition."
                )? {
                    let t = w.to_type("Type error: Type name expected.")?;
                    argst.push(t);
                }

            if wv_rett.len() != 1 {
                return Err("Syntax error in the return type part of function definition.".to_string());
            }
            let rett = wv_rett[0].to_type("Type error: Type name expected.")?;

            if let Some((wv_args, wv_ret)) = split(T::from_symbol("->"), &wv_other) {
                // ここが中心部
                let mut args = Vec::new();
                for w in explode_each(T::from_symbol(","),
                        &wv_args,
                        "Syntax error in the arguments part of function definition."
                    )? {
                        if let Ok(id) = w.to_identifier("") {
                            if Some(id.clone()) == identifier {
                                return Err(format!("Name error: PresetFunction name '{}' cannot used in arguments.",
                                                    identifier.unwrap()));
                            }
                            args.push(id);
                        } else {
                            return Err(format!("Name error: Token '{}' cannot used in arguments.", w));
                        }
                }
                let fdef: FuncDef<T> = FuncDef {
                    name: identifier,
                    argtv: argst,
                    rett: rett,
                    argv: args,
                    expr: wv_ret
                };
                return Ok(fdef);
            } else {
                return Err("Syntax error: Token '->' not found in the main part of function definition.".to_string());
            }
        } else {
            return Err("Syntax error: Token '->' not found in the type declaration of function definition.".to_string());
        }
    } else {
        return Err("Syntax error: Token ';' not found in function definition.".to_string());
    }
}

//------------------------------------------------
//            ここからOperator, PresetFunction
//------------------------------------------------

#[derive(Clone, PartialEq, Debug)]
pub struct BinarySig {
    args: (WordType, WordType),
    ret: WordType,
}

impl BinarySig {
    pub fn new(args: (WordType, WordType), ret: WordType) -> Self {
        return Self {args: args, ret: ret};
    }
}

#[derive(Clone, Debug)]
pub struct BinaryOp<T> {
    name: String,
    fs: Vec<(BinarySig, fn(T, T) -> Result<T, String>)>,
    priority: usize,
}

impl BinaryOp<Word> {
    pub fn to_wordtype(&self) -> BinaryOp<WordType> {
        let mut fs: Vec<(BinarySig, fn(WordType, WordType) -> Result<WordType, String>)> = Vec::new();
        fn f(_: WordType, _: WordType) -> Result<WordType, String> {
            Ok(WordType::Null)
        }

        for (sig, _) in self.fs.clone() {
            fs.push((sig, f))
        }

        return BinaryOp {
            name: self.name(),
            priority: self.priority,
            fs: fs,
        }
    }
}

impl<T: WordKind<T> + Clone + fmt::Display + PartialEq> BinaryOp<T> {
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

    pub fn apply(&self, w1: T, w2: T) -> Result<T, String> {
        for (sig, f) in self.fs.clone() {
            if (w1.get_type(), w2.get_type()) == sig.args {
                if T::is_wordtype() {
                    return Ok(T::from_wordtype(sig.ret));
                } else {
                    return f(w1, w2);
                }
            }
        }
        return Err(format!("Bianry operator '{}': Type error. Expected one of pairs of types {}, got ({},{}).",
                    self.name(), self.accepts(), w1.get_type(), w2.get_type()));
    }

    pub fn sigs(&self) -> Vec<BinarySig> {
        let mut sigs = Vec::new();
        for (sig, _) in self.fs.clone() {
            sigs.push(sig);
        }
        return sigs;
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct UnarySig {
    arg: WordType,
    ret: WordType,
}

impl UnarySig {
    pub fn new(arg: WordType, ret: WordType) -> Self {
        return Self {arg: arg, ret: ret};
    }
}

#[derive(Clone, Debug)]
pub struct UnaryOp<T> {
    name: String,
    fs: Vec<(UnarySig, fn(T) -> Result<T, String>)>,
    priority: usize,
}

#[derive(Clone, PartialEq)]
pub enum OperatorSigs {
    Unary(Vec<UnarySig>),
    Binary(Vec<BinarySig>),
}

impl UnaryOp<Word> {
    pub fn to_wordtype(&self) -> UnaryOp<WordType> {
        let mut fs: Vec<(UnarySig, fn(WordType) -> Result<WordType, String>)> = Vec::new();
        fn f(_: WordType) -> Result<WordType, String> {
            Ok(WordType::Null)
        }

        for (sig, _) in self.fs.clone() {
            fs.push((sig, f))
        }

        return UnaryOp {
            name: self.name(),
            priority: self.priority,
            fs: fs,
        }
    }
}

impl<T: WordKind<T> + Clone + fmt::Display + PartialEq> UnaryOp<T> {
    pub fn name(&self) -> String {
        return self.name.clone();
    }

    pub fn apply(&self, w: T) -> Result<T, String> {
        for (sig, f) in self.fs.clone() {
            if sig.arg == w.get_type() {
                if T::is_wordtype() {
                    return Ok(T::from_wordtype(sig.ret.clone()));
                } else {
                    return f(w);
                }
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

    pub fn sigs(&self) -> Vec<UnarySig> {
        let mut sigs = Vec::new();
        for (sig, _) in self.fs.clone() {
            sigs.push(sig);
        }
        return sigs;
    }
}

// presetの演算子はここに追加していく
pub fn preset_operators<T: WordKind<T> + Clone + PartialEq + fmt::Display>() -> std::collections::HashMap<String, Operator<T>> {
    let opv: Vec<Operator<T>> = vec![
        Operator::UnaryOp(UnaryOp {
            name: "!".to_string(),
            fs: vec![
                    (UnarySig::new(WordType::Bool, WordType::Bool),
                    |w: T| {
                        let b = w.to_bool("")?;
                        Ok(T::from_bool(!b))
                    })
                ],
            priority: 5,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "||".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Bool, WordType::Bool), WordType::Bool),
                |w1: T, w2: T| {
                    let b1 = w1.to_bool("")?;
                    let b2 = w2.to_bool("")?;
                    Ok(T::from_bool(b1 || b2))
                }),
            ],
            priority: 7,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "&&".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Bool, WordType::Bool), WordType::Bool),
                |w1: T, w2: T| {
                    let b1 = w1.to_bool("")?;
                    let b2 = w2.to_bool("")?;
                    Ok(T::from_bool(b1 && b2))
                }),
            ],
            priority: 6,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "==".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Bool),
                |w1: T, w2: T| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(T::from_bool(s1 == s2))
                }),
                (BinarySig::new((WordType::Bool, WordType::Bool), WordType::Bool),
                |w1: T, w2: T| {
                    let s1 = w1.to_bool("")?;
                    let s2 = w2.to_bool("")?;
                    Ok(T::from_bool(s1 == s2))
                }),
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: T, w2: T| {
                    let s1 = w1.to_number("")?;
                    let s2 = w2.to_number("")?;
                    Ok(T::from_bool(s1 == s2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "!=".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Bool),
                |w1: T, w2: T| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(T::from_bool(s1 != s2))
                }),
                (BinarySig::new((WordType::Bool, WordType::Bool), WordType::Bool),
                |w1: T, w2: T| {
                    let s1 = w1.to_bool("")?;
                    let s2 = w2.to_bool("")?;
                    Ok(T::from_bool(s1 != s2))
                }),
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: T, w2: T| {
                    let s1 = w1.to_number("")?;
                    let s2 = w2.to_number("")?;
                    Ok(T::from_bool(s1 != s2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "<".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: T, w2: T| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(T::from_bool(n1 < n2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: ">".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: T, w2: T| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(T::from_bool(n1 > n2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "<=".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: T, w2: T| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(T::from_bool(n1 <= n2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: ">=".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Bool),
                |w1: T, w2: T| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(T::from_bool(n1 >= n2))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "in".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Bool),
                |w1: T, w2: T| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(T::from_bool(s1.is_in(&s2)))
                }),
            ],
            priority: 4,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "-".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Set),
                |w1: T, w2: T| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(T::from_set(Set::set_diff(&s1,&s2)))
                }),
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Number),
                |w1: T, w2: T| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    if n1 >= n2 {
                        Ok(T::from_number(n1 - n2))
                    } else {
                        Err(format!("Value error: The left hand side of binary operator '-' is '{}', which is less than the right hand side '{}'.", n1, n2))
                    }
                }),
            ],
            priority: 3,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "+".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Set),
                |w1: T, w2: T| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(T::from_set(Set::set_union(&s1,&s2)))
                }),
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Number),
                |w1: T, w2: T| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(T::from_number(n1 + n2))
                }),
            ],
            priority: 3,
        }),
        Operator::BinaryOp(BinaryOp {
            name: "*".to_string(),
            fs: vec![
                (BinarySig::new((WordType::Set, WordType::Set), WordType::Set),
                |w1: T, w2: T| {
                    let s1 = w1.to_set("")?;
                    let s2 = w2.to_set("")?;
                    Ok(T::from_set(Set::set_intersec(&s1,&s2)))
                }),
                (BinarySig::new((WordType::Number, WordType::Number), WordType::Number),
                |w1: T, w2: T| {
                    let n1 = w1.to_number("")?;
                    let n2 = w2.to_number("")?;
                    Ok(T::from_number(n1 * n2))
                }),
            ],
            priority: 2,
        }),
        Operator::UnaryOp(UnaryOp {
            name: "#".to_string(),
            fs: vec![
                    (UnarySig::new(WordType::Set, WordType::Number),
                    |w: T| {
                        let set = w.to_set("")?;
                        Ok(T::from_number(set.len()))
                    }),
                ],
            priority: 1,
        }),
        Operator::UnaryOp(UnaryOp {
            name: "print".to_string(),
            fs: vec![
                    (UnarySig::new(WordType::Set, WordType::PrintSignal),
                    |w: T| {
                        let s = w.to_string();
                        Ok(T::from_printsignal(s))
                    }),
                    (UnarySig::new(WordType::Number, WordType::PrintSignal),
                    |w: T| {
                        let s = w.to_string();
                        Ok(T::from_printsignal(s))
                    }),
                    (UnarySig::new(WordType::Bool, WordType::PrintSignal),
                    |w: T| {
                        let s = w.to_string();
                        Ok(T::from_printsignal(s))
                    }),
                ],
            priority: 10,
        }),
    ];

    let opnames: Vec<String> = opv.iter().map(|x| x.name()).collect();
    return opnames.into_iter().zip(opv.into_iter()).collect();
}

#[derive(Clone, Debug)]
pub enum Operator<T> {
    BinaryOp(BinaryOp<T>),
    UnaryOp(UnaryOp<T>),
}

impl Operator<Word> {
    pub fn to_wordtype(&self) -> Operator<WordType> {
        match self {
            Self::UnaryOp(uop) => Operator::UnaryOp(uop.to_wordtype()),
            Self::BinaryOp(bop) => Operator::BinaryOp(bop.to_wordtype()),
        }
    }
}

impl<T: WordKind<T> + Clone + fmt::Display + PartialEq> PartialEq for Operator<T> {
    fn eq(&self, rhs: &Self) -> bool {
        return self.name() == rhs.name();
    }
}

impl<T: WordKind<T> + Clone + fmt::Display + PartialEq> Operator<T> {
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

    pub fn sigs(&self) -> OperatorSigs {
        match self {
            Self::BinaryOp(op) => OperatorSigs::Binary(op.sigs()),
            Self::UnaryOp(op) => OperatorSigs::Unary(op.sigs()),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Function<T: Clone + PartialEq> {
    Preset(PresetFunction<T>),
    User(UserFunction<T>),
}

impl<T: Clone + PartialEq + fmt::Display + WordKind<T>> Function<T> {
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
}

impl<T: WordKind<T> + Clone + PartialEq + fmt::Display> Function<T> {
    pub fn type_check(&self, wv: Vec<T>) -> Result<(), String>{
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

impl Function<Word> {
    pub fn to_wordtype(&self) -> Function<WordType> {
        match self {
            Self::Preset(pf) => Function::Preset(pf.to_wordtype()),
            Self::User(uf) => Function::User(uf.to_wordtype()),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct PresetFunction<T: Clone + PartialEq> {
    name: Option<String>,
    f: fn(Vec<T>) -> Result<T, String>,
    sig: Signature, 
}

impl PresetFunction<Word> {
    pub fn to_wordtype(&self) -> PresetFunction<WordType> {
        let f = |_: Vec<WordType>| {
            return Ok(WordType::Null);
        };

        return PresetFunction::<WordType> {
            name: self.name.clone(),
            sig: self.sig.clone(),
            f: f,
        }
    }
}

impl<T: Clone + PartialEq> PresetFunction<T> {
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

    pub fn apply(&self, wv: Vec<T>) -> Result<T, String> {
        return (self.f)(wv);
    }
}

// presetの関数はここに追加
pub fn preset_functions() -> std::collections::HashMap<String, PresetFunction<Word>> {
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

#[derive(Clone, PartialEq, Debug)]
pub struct Signature {
    pub args: Vec<WordType>,
    pub ret: WordType,
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

#[derive(Clone, PartialEq, Debug)]
pub struct UserFunction<T: Clone + PartialEq> {
    name: Option<String>,
    sig: Signature,
    xv: Vec<String>,
    expr: Vec<T>,
}

impl<T: Clone + PartialEq + WordKind<T> + fmt::Display> UserFunction<T> {
    pub fn new(name: Option<String>, sig: Signature, xv: Vec<String>, expr: Vec<T>) -> Self {
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

    pub fn get_expr(&self) -> Vec<T> {
        return self.expr.clone();
    }

    pub fn to_string(&self) -> String {
        let mut name = "(anonymous function)".to_string();
        if let Some(n) = self.get_name() {
            name = n;
        }

        return format!("{}: {}", name, self.get_sig());
    }

    pub fn to_wordkind(&self) -> T {
        return T::from_userf(self.clone());
    }
}
    
impl UserFunction<Word> {
    pub fn to_wordtype(&self) -> UserFunction<WordType> {
        return UserFunction::<WordType>::new(
            self.name.clone(),
            self.sig.clone(),
            self.xv.clone(),
            vec![self.sig.ret.clone()],
        )
    }
}
