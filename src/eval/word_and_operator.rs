mod constants;
mod setlike;

pub use self::constants::*;
pub use self::setlike::*;

#[derive(Clone, PartialEq)]
pub enum Word {
    SetWord(Set),
    KeywordWord(&'static str),
    SymbolWord(&'static str),
    IdentifierWord(String),
    BoolWord(bool),
    NullWord,
    FrozenWord(FrozenWordList),
    OperatorWord(Operator),
    FunctionWord(Function),
}

pub enum WordType {
    SetWord,
    KeywordWord,
    SymbolWord,
    IdentifierWord,
    BoolWord,
    NullWord,
    FrozenWord,
    OperatorWord,
    FunctionWord,
}

impl Word {
    pub fn from_token(token: Token) -> Word {
        match token {
            Token::KeywordToken(s) => {
                if let Some(op) = preset_operators().get(s) {
                    return Word::OperatorWord(op.clone());
                }

                return Word::KeywordWord(s);
            },
            Token::SymbolToken(s) => {
                if let Some(op) = preset_operators().get(s) {
                    return Word::OperatorWord(op.clone());
                }

                return Word::SymbolWord(s);
            },
            Token::IdentifierToken(s) => {
                return Word::IdentifierWord(s);
            }
        }
    }

    pub fn get_type(&self) -> WordType {
        match self {
            Word::SetWord(_) => WordType::SetWord,
            Word::KeywordWord(_) => WordType::KeywordWord,
            Word::SymbolWord(_) => WordType::SymbolWord,
            Word::IdentifierWord(_) => WordType::IdentifierWord,
            Word::BoolWord(_) => WordType::BoolWord,
            Word::NullWord => WordType::NullWord,
            Word::FrozenWord(_) => WordType::FrozenWord,
            Word::OperatorWord(_) => WordType::OperatorWord,
            Word::FunctionWord(_) => WordType::FunctionWord,
        }
    }
   
    pub fn to_set(&self, mes: &String) -> Result<Set, String> {
        match self {
            Word::SetWord(set) => Ok(set.clone()),
            _ => Err(mes.clone()),
        }
    }

    pub fn to_bool(&self, mes: &String) -> Result<bool, String> {
        match self {
            Word::BoolWord(b) => Ok(*b),
            _ => Err(mes.clone()),
        }
    }

    pub fn to_operator(&self, mes: &String) -> Result<Operator, String> {
        match self {
            Word::OperatorWord(op) => Ok(op.clone()),
            _ => Err(mes.clone()),
        }
    }

    pub fn find_word(wordv: &Vec<Word>, word: Word) -> Option<usize> {
        let mut i_ret: Option<usize> = None;
        for i in 0..wordv.len() {
            if wordv[i] == word {
                i_ret = Some(i);
            }
        }
        return i_ret;
    }

    pub fn to_string(self: &Self) -> String {
        match self {
            Self::SetWord(set) => set.to_string(),
            Self::SymbolWord(s) => s.to_string(),
            Self::KeywordWord(s) => s.to_string(),
            Self::IdentifierWord(s) => s.to_string(),
            Self::BoolWord(b) => b.to_string(),
            Self::NullWord => "".to_string(),
            Self::FrozenWord(fwl) => fwl.to_string(),
            Self::OperatorWord(op) => op.name(),
            Self::FunctionWord(_) => "(Function)".to_string(),
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
pub struct FrozenWordList {
    contents: Vec<Word>,
    bound: Option<(String, String)>
}

pub fn display_bound(bound: &Option<(String, String)>) -> String {
    match bound {
        None => "None".to_string(),
        Some((b,e)) => format!("({}, {})", b, e)
    }
}

impl FrozenWordList {
    fn find_frozenbound(wv: &Vec<Word>) -> Result<Option<(usize, usize)>, String> {
        let mut i_bound: Option<(usize, usize)> = None;

        for (b, e) in FROZEN_BOUND {
            if let Some((ib, ie)) = Word::find_bracket(wv, Word::SymbolWord(b), Word::SymbolWord(e))? {
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

    pub fn from_wordv(wv: Vec<Word>, bound: Option<(String, String)>) -> Result<Self, String> {
        let mut contents = wv;

        while let Some((ib, ie)) = Self::find_frozenbound(&contents)? {
            let b = contents[ib].to_string();
            let e = contents[ie].to_string();

            let (wv_other, mut wv3) = split_drop(&contents, ie, ie);
            let (mut wv1, wv2) = split_drop(&wv_other, ib, ib);

            let word = Word::FrozenWord(Self::from_wordv(wv2, Some((b, e)))?);
            wv1.push(word);
            wv1.append(&mut wv3);
            contents = wv1;
        }
        
        return Ok(Self {
            contents: contents,
            bound: bound.clone(),
        })
    }

    pub fn from_tokenv(tv: Vec<Token>, bound: Option<(String, String)>) -> Result<Self, String> {
        let mut wv = Vec::new();

        for t in tv {
            let w = Word::from_token(t);
            wv.push(w);
        }

        return Self::from_wordv(wv, bound);
    }

    pub fn from_string(string: &String) -> Result<Self, String> {
        return Self::from_tokenv(Token::tokenize(string)?, None);
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

        for word in self.get_contents() {
            string = format!("{}, {}", string, word.to_string());
        }
        string.remove(0);
        string = format!("[{}]", string);

        return string;
    }
}

#[derive(Clone)]
pub struct BinaryOp {
    name: String,
    f: fn(Word, Word) -> Result<Word, String>,
    priority: usize,
}

impl BinaryOp {
    pub fn apply(&self, t1: Word, t2: Word) -> Result<Word, String> {
        return (self.f)(t1, t2);
    }
}

#[derive(Clone)]
pub struct UnaryOp {
    name: String,
    f: fn(Word) -> Result<Word, String>,
    priority: usize,
}

impl UnaryOp {
    pub fn apply(&self, t: Word) -> Result<Word, String> {
        return (self.f)(t);
    }
}

// presetの演算子はここに追加していく
pub fn preset_operators() -> std::collections::HashMap<String, Operator> {
    let opv = vec![
        Operator::UnaryOp(UnaryOp {
            name: "!".to_string(),
            priority: 5,
            f: |t: Word| {
                let b = t.to_bool(&"Type Error in the first argument of unary operator.".to_string())?;
                Ok(Word::BoolWord(!b))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "==".to_string(),
            priority: 4,
            f: |t1: Word, t2: Word| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Word::BoolWord(s1 == s2))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "!=".to_string(),
            priority: 4,
            f: |t1: Word, t2: Word| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Word::BoolWord(s1 != s2))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "in".to_string(),
            priority: 4,
            f: |t1: Word, t2: Word| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Word::BoolWord(s1.is_in(&s2)))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "-".to_string(),
            priority: 4,
            f: |t1: Word, t2: Word| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Word::SetWord(Set::set_diff(&s1,&s2)))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "+".to_string(),
            priority: 3,
            f: |t1: Word, t2: Word| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Word::SetWord(Set::set_union(&s1,&s2)))
            },
        }),
        Operator::BinaryOp(BinaryOp {
            name: "*".to_string(),
            priority: 2,
            f: |t1: Word, t2: Word| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Word::SetWord(Set::set_intersec(&s1,&s2)))
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
pub struct Function {
    f: fn(Vec<Word>) -> Result<Word, String>,
}

impl Function {
    pub fn apply(&self, wv: Vec<Word>) -> Result<Word, String> {
        return (self.f)(wv);
    }
}

#[derive(Clone, PartialEq)]
pub enum Token {
    SymbolToken(&'static str),
    KeywordToken(&'static str),
    IdentifierToken(String),
}

impl Token {
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
                let mut wv1 = Self::tokenize(&s1)?;
                let mut wv2 = Self::tokenize(&s2)?;

                if token != Token::SymbolToken(" ") { // スペースはpushしない
                    wv1.push(token);
                }
                
                wv1.append(&mut wv2);
                return Ok(wv1);
            }
        }
    }
}
