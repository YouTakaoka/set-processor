pub const KEYWORD_LIST: [&str; 7] = ["let", "if", "then", "else", "in", "size", "is_empty"];

//2文字シンボルは必ず最初に入れること！
pub const SYMBOL_LIST: [&str; 15] = ["==", "!=", "&&", "||", "!", "=", " ", "{", "}", ",", "+", "*", "-", "(", ")"];

pub const FROZEN_BOUND: [(&str, &str); 3] = [("(", ")"), ("{", "}"), ("<", ">")];