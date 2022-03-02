pub mod token_and_setlike;

pub use self::token_and_setlike::Token;
pub use self::token_and_setlike::Set;

pub struct BinaryOp {
    name: String,
    f: Box<dyn Fn(Token, Token) -> Result<Token, String>>,
    priority: usize,
}

impl BinaryOp {
    pub fn apply(&self, t1: Token, t2: Token) -> Result<Token, String> {
        return (self.f)(t1, t2);
    }
}

pub struct UnaryOp {
    name: String,
    f: Box<dyn Fn(Token) -> Result<Token, String>>,
    priority: usize,
}

impl UnaryOp {
    pub fn apply(&self, t: Token) -> Result<Token, String> {
        return (self.f)(t);
    }
}

// presetの演算子はここに追加していく
pub fn preset_operators<'a>() -> std::collections::HashMap<String, Operator> {
    let opv = vec![
        Operator::UnaryOp(UnaryOp {
            name: "!".to_string(),
            priority: 5,
            f: Box::new(|t: Token| {
                let b = t.to_bool(&"Type Error in the first argument of unary operator.".to_string())?;
                Ok(Token::BoolToken(!b))
            }),
        }),
        Operator::BinaryOp(BinaryOp {
            name: "==".to_string(),
            priority: 4,
            f: Box::new(|t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::BoolToken(s1 == s2))
            }),
        }),
        Operator::BinaryOp(BinaryOp {
            name: "!=".to_string(),
            priority: 4,
            f: Box::new(|t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::BoolToken(s1 != s2))
            }),
        }),
        Operator::BinaryOp(BinaryOp {
            name: "in".to_string(),
            priority: 4,
            f: Box::new(|t1: Token, t2: Token| {
                //println!("{}", t2.to_string()); //tofix
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::BoolToken(s1.is_in(s2)))
            }),
        }),
        Operator::BinaryOp(BinaryOp {
            name: "-".to_string(),
            priority: 4,
            f: Box::new(|t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::SetToken(Set::set_diff(s1,s2)))
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
        }),
        Operator::BinaryOp(BinaryOp {
            name: "*".to_string(),
            priority: 2,
            f: Box::new(|t1: Token, t2: Token| {
                let s1 = t1.to_set(&"Type Error in the first argument of binary operator.".to_string())?;
                let s2 = t2.to_set(&"Type Error in the second argument of binary operator.".to_string())?;
                Ok(Token::SetToken(Set::set_intersec(s1,s2)))
            }),
        }),
    ];

    let opnames: Vec<String> = opv.iter().map(|x| x.name()).collect();
    return opnames.into_iter().zip(opv.into_iter()).collect();
}

pub enum Operator {
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
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

    pub fn from_token(token: Token) -> Option<String> {
        for opname in preset_operators().keys() {
            if token.to_string() == opname.clone() {
                return Some(opname.to_string());
            }
        }
        return None;
    }
}
