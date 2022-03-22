mod word_and_operator;

pub use self::word_and_operator::*;
use std::fmt;

#[derive(Clone)]
pub struct Bind<T: Clone> {
    map: std::collections::HashMap<String, T>,
    funcmap: std::collections::HashMap<String, T>,
}

impl Bind<Word> {
    pub fn to_typebind(&self) -> Bind<WordType> {
        let mut map_new = std::collections::HashMap::new();
        let mut funcmap_new = std::collections::HashMap::new();

        for (s, w) in self.map.clone() {
            map_new.insert(s, w.get_type());
        }

        for (s, w) in self.funcmap.clone() {
            funcmap_new.insert(s, w.get_type());
        }

        return Bind {map: map_new, funcmap: funcmap_new};
    }
}

impl<T: Clone> Bind<T> {
    pub fn new() -> Self {
        return Self {
            map: std::collections::HashMap::new(),
            funcmap: std::collections::HashMap::new(),
        }
    }

    pub fn get(&self, key: &String) -> Option<T> {
        if let Some(w) = self.map.get(key) {
            return Some(w.clone());
        } else if let Some(w) = self.funcmap.get(key) {
            return Some(w.clone());
        } else {
            return None;
        }
    }

    pub fn insert(self: &mut Self, key: String, val: T) {
        self.map.insert(key, val);
    }

    pub fn insert_func(self: &mut Self, key: String, val: T) {
        self.funcmap.insert(key, val);
    }

    pub fn func_only(&self) -> Self {
        return Self {
            map: std::collections::HashMap::new(),
            funcmap: self.funcmap.clone(),
        }
    }
}

fn substitute<T: Clone + WordKind<T> + fmt::Display + PartialEq>(word: &T, bindm: Bind<T>) -> Result<Option<T>, String> {
    if let Ok(id) = word.to_identifier("") {
        if let Some(w) = bindm.get(&id) {
            return Ok(Some(w.clone()));
        }
        // Identifierにも関わらずbindmになければError
        return Err(format!("Parse error: Undefined token: '{}'", word.to_string()))
    }
    return Ok(None);
}

fn setlist_from_fwl<T: Clone + WordKind<T> + PartialEq + fmt::Display>(fwl: FrozenWordList<T>, bindm: &Bind<T>) -> Result<SetList, String> {
    if !fwl.env_is(Env::Set) {
        panic!("setlist_from_fwl: Irregal env: {}", fwl.get_env());
    }

    if fwl.is_empty() {
        return Ok(SetList::new(Vec::new()))
    }

    let wv = fwl.get_contents();
    let mut contents: Vec<Set> = Vec::new();

    for wv1 in T::from_symbol(",").explode(&wv) {
        let frozen = T::vec_to_frozen(wv1, &Env::Line)?;
        let (word, _) = eval(frozen, bindm)?;
        let set = word.to_set("Type error: Non-set object found in {} symbol.")?;
        contents.push(set);
    }

    return Ok(SetList::new(contents));
}

fn set_from_fwl<T: Clone + WordKind<T> + PartialEq + fmt::Display>(fwl: FrozenWordList<T>, bindm: &Bind<T>) -> Result<Set, String> {
    let sl = setlist_from_fwl(fwl, bindm)?;
    return Ok(sl.uniquify());
}

fn find_frozen<T: WordKind<T> + Clone + fmt::Display + PartialEq>(wv: &Vec<T>) -> Option<(usize, Frozen<T>)> {
    for i in 0..wv.len() {
        if let Ok(frozen) = wv[i].to_frozen("") {
            return Some((i, frozen));
        }
    }

    return None;
}

fn apply_function<T: Clone + WordKind<T> + PartialEq + fmt::Display>
    (f: Function<T>, frozen: Frozen<T>, bm: &Bind<T>) -> Result<T, String> {

    let contents;
    if let Frozen::Bracket(wvv) = frozen {
        if wvv.len() > 1 {
            panic!("Too many word vectors found in function call. Something is wrong.");
        }
        contents = wvv.get(0).unwrap().clone();
    } else {
        panic!("Token '(' must follow just after a function, found {}.", frozen.to_string());
    }

    let mut wv = Vec::new();
    for wv1 in T::from_symbol(",").explode(&contents) {
        let frozen = T::vec_to_frozen(wv1, &Env::Line)?;
        let (word1, _) = eval(frozen, bm)?;
        wv.push(word1);
    }

    if contents.is_empty() {
        wv = vec![];
    }

    // Type check
    f.type_check(wv.clone())?;

    // Now apply function
    if T::is_wordtype() {
        return Ok(T::from_wordtype(f.sig().ret.clone()));
    } else {
        match f {
            Function::Preset(pf) => return pf.apply(wv),
            Function::User(uf) => return apply_user(uf, wv, bm),
        }
    }
}

fn apply_user<T: Clone + WordKind<T> + PartialEq + fmt::Display>(f: UserFunction<T>, argv: Vec<T>, bindm: &Bind<T>) -> Result<T, String> {
    let mut bm = bindm.func_only();
    for (x, arg) in f.get_xv().iter().zip(argv) {
        bm.insert(x.clone(), arg);
    }

    let fwl = T::vec_to_frozen(f.get_expr(), &Env::Line)?;
    let (ret, _) = eval(fwl, &bm)?;
    return Ok(ret);
}

fn return_type_check<T: Clone + PartialEq + WordKind<T> + fmt::Display>
    (fv_def: &Vec<Frozen<T>>, bindm: &Bind<T>) -> Result<(), String> {

    let mut bm = bindm.func_only();
    for frozen in fv_def {
        let (_, bm1) = eval(frozen.clone(), &bm)?;
        bm = bm1;
    }

    for frozen in fv_def {
        if let Frozen::FuncDef(fd) = frozen {
            if fd.argv.len() != fd.argtv.len() {
                return Err(format!(
                    "Error: Number of arguments in type definition ({}) and main part ({}) of function definition doesn't match.",
                    fd.argtv.len(),
                    fd.argv.len()
                ));
            }

            let mut bm1 = bm.clone();
            for (xt, x) in fd.argtv.iter().zip(&fd.argv) {
                bm1.insert(x.clone(), T::from_wordtype(xt.clone()));
            }

            let expr = fd.expr.clone();
            let (w, _) = eval(T::vec_to_frozen(expr, &Env::Line)?, &bm1)?;
            let wt = w.get_type();
            if wt != fd.rett {
                return Err(format!(
                    "Type error: Return type of function {:?} doesn't match the signature. Expected {}, got {}.",
                    fd.name.clone(),
                    fd.rett.clone(),
                    wt
                ));
            }
        }
    }
    return Ok(());
}

fn compile_defs<T: Clone + WordKind<T> + PartialEq + fmt::Display>
    (frozenv: &Vec<Frozen<T>>, bindm: &Bind<T>) -> Result<(Vec<Frozen<T>>, Bind<T>), String> {

    let mut frozenv_def = Vec::new();
    let mut frozenv_other = Vec::new();
    for frozen in frozenv {
        if let Frozen::FuncDef(_) = frozen {
            frozenv_def.push(frozen.clone());
        } else {
            frozenv_other.push(frozen.clone());
        }
    }

    if T::is_wordtype() {
        return_type_check(&frozenv_def, bindm)?;
    }

    let mut bm = bindm.clone();
    for frozen in frozenv_def {
        let (_, bm1) = eval(frozen, &bm)?;
        bm = bm1;
    }

    return Ok((frozenv_other, bm));
}

fn eval_scope<T: Clone + WordKind<T> + PartialEq + fmt::Display>
    (frozenv: &Vec<Frozen<T>>, bm: &Bind<T>) -> Result<(T, Bind<T>), String> {

    let (frozenv_other, bindm1) = compile_defs(frozenv, bm)?;

    let mut bindm = bindm1;
    let mut word = T::null();
    for frozen in frozenv_other {
        let (w1, bm1) = eval(frozen, &bindm)?;
        word = w1;
        bindm = bm1;
    }
    return Ok((word, bindm));
}

fn eval_fwl<T: Clone + WordKind<T> + PartialEq + std::fmt::Display>
    (fwl: FrozenWordList<T>, bm: &Bind<T>) -> Result<(T, Bind<T>), String> {

    if fwl.is_empty() {
        return Ok((T::null(), bm.clone()));
    }

    let env = fwl.get_env();
    let mut bindm = bm.clone();

    // Identifierトークンの置き換え処理
    // 注：関数処理より前にやること！
    let mut contents: Vec<T> = Vec::new();
    for word in fwl.get_contents() {
        if let Some(w_ret) = substitute(&word, bindm.clone())? {
            contents.push(w_ret);
        } else {
            contents.push(word);
        }
    }

    // 関数処理
    // 注: 括弧処理より前にやること！
    // 注: ループではなく再帰で処理すること！(オペレータや関数を返す関数があり得るため)
    // 先に全部PresetFunctionで置き換えてしまう(関数を引数にとる関数やオペレータがあり得るため)
    for i in 0..contents.len() {
        if let Ok(f) = contents[i].to_func("") {
            if let Some(w) = contents.get(i+1) {
                if let Ok(Frozen::Bracket(wvv)) = w.to_frozen("") {
                    if wvv.len() > 1 {
                        return Err("Syntax error: Symbol '|' cannot be used in function call.".to_string());
                    }
                    let frozen = Frozen::Bracket(wvv.clone());
                    let word = apply_function(f.clone(), frozen.clone(), &bindm)?;
                    let contents_new = subst_range(&contents, i, i + 1, word);
                    if let Frozen::WordList(fwl_new) = T::vec_to_frozen(contents_new, &env)? {
                        return eval_fwl(fwl_new, &bindm);
                    }
                }
            }
        }
    }

    // 括弧処理
    while let Some((i, frozen)) = find_frozen(&contents) {
        match frozen {
            Frozen::Scope(wvv) => {
                let mut fv = Vec::new();
                for wv in wvv {
                    fv.push(T::vec_to_frozen(wv, &Env::Line)?);
                }
                let (word, _) = eval_scope(&fv, &bindm)?;
                contents[i] = word;
            },
            Frozen::Bracket(wvv) => {
                let mut fv = Vec::new();
                for wv in wvv {
                    fv.push(T::vec_to_frozen(wv, &Env::Line)?);
                }
                let (word, bindm1) = eval_scope(&fv, &bindm)?;
                contents[i] = word;
                bindm = bindm1;
            },
            Frozen::WordList(fwl) => {
                if fwl.env_is(Env::Set) { // Setの場合
                    let set = set_from_fwl(fwl, &bindm)?;
                    contents[i] = T::from_set(set);
                }
            },
            _ => panic!("Error: Invalid kind of frozen: {}.", frozen.to_string()),
        }
    }

    // Operator探し
    let mut index: Option<usize> = None;
    let mut priority = 11;

    for i in 0..contents.len() {
        let word: T = contents[i].clone();

        // wordがOperatorかどうか調べる
        if let Ok(op) = word.to_operator(&"".to_string()) {
            // 優先順位を確認
            let priority1 = op.priority();

            // 優先順位が既存より高ければindexと優先順位を記憶
            if priority1 < priority {
                index = Some(i);
                priority = priority1;
            }
        }    
    }

    // Operator処理
    // 注: ループではなく再帰で処理すること！(オペレータや関数を返すOperatorがあり得るため)
    if let Some(i) = index {  // Operator見つかった
        let (mut wv1, mut wv2) = split_drop(&contents, i, i);

        match contents[i].to_operator("Oops! Something is wrong.")? {
            Operator::BinaryOp(binop) => {
                let w1 :T = wv1.pop().ok_or("Parse error: Nothing before binary operator.".to_string())?;
                if wv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let w2 = wv2.remove(0);
                let w_res = binop.apply(w1, w2)?;
                wv1.push(w_res);
                wv1.append(&mut wv2);
                return eval(T::vec_to_frozen(wv1, &env)?, &bindm);
            },
            Operator::UnaryOp(unop) => {
                if wv2.is_empty() {
                    return Err("Parse error: Nothing after binary operator.".to_string());
                }
                let w = wv2.remove(0);
                let w_res = unop.apply(w)?;
                wv1.push(w_res);
                wv1.append(&mut wv2);
                return eval(T::vec_to_frozen(wv1, &env)?, &bindm);
            },
        }
    }

    // トークン列が長さ1ならそのまま返す
    if contents.len() == 1 {
        return Ok((contents[0].clone(), bindm));
    } else {
        return Err(format!("Parse error: More than 2 tokens remain: {}", vec_to_string(contents)));
    }
}

pub fn eval<T: Clone + PartialEq + WordKind<T> + fmt::Display>
    (frozen: Frozen<T>, bindm: &Bind<T>) -> Result<(T, Bind<T>), String> {

    match frozen {
        Frozen::WordList(fwl) => return eval_fwl(fwl, bindm),
        Frozen::Scope(wvv) | Frozen::Bracket(wvv) => {
            let mut frozenv = Vec::new();
            for wv in wvv {
                frozenv.push(T::vec_to_frozen(wv, &Env::Line)?);
            }
            return eval_scope(&frozenv, bindm);
        },
        Frozen::FuncDef(fd) => {
            let mut bm = bindm.clone();
            let mut w_func = T::null();
            if let Some(name) = fd.name.clone() {
                w_func = T::from_function(Function::User(UserFunction::new(
                    Some(name.clone()),
                    Signature::new(fd.argtv.clone(), fd.rett.clone()),
                    fd.argv,
                    fd.expr.clone(),
                )));
                bm.insert_func(
                    name.clone(),
                    w_func.clone()
                )
            }
            return Ok((w_func, bm));
        },
        Frozen::IfExpr(ie) => {
            let (w_if, bm1) = eval(T::vec_to_frozen(ie.wv_if.clone(), &Env::Line)?, bindm)?;
            if w_if.get_type() != WordType::Bool {
                return Err(format!("Type error: 'if' clause returned non-Bool value: {}", w_if));
            }

            if T::is_wordtype() { // T == WordTypeの場合
                let (wt_then, _) = eval(T::vec_to_frozen(ie.wv_then.clone(), &Env::Line)?, &bm1)?;
                let (wt_else, _) = eval(T::vec_to_frozen(ie.wv_else.clone(), &Env::Line)?, &bm1)?;
                if wt_then == wt_else {
                    return Ok((wt_then, bindm.clone()));
                } else {
                    return Err(format!(
                        "Type error: Mismatch in return type of 'then' clause ({}) and 'else' clause ({}).",
                        wt_then, wt_else
                    ))
                }
            } else { // T == Wordの場合
                if w_if.to_bool("")? { // true
                    let (wt_then, _) = eval(T::vec_to_frozen(ie.wv_then.clone(), &Env::Line)?, &bm1)?;
                    return Ok((wt_then, bindm.clone()));
                } else { // false
                    let (wt_else, _) = eval(T::vec_to_frozen(ie.wv_else.clone(), &Env::Line)?, &bm1)?;
                    return Ok((wt_else, bindm.clone()));
                }
            }
        },
        Frozen::LetExpr(le) => {
            let expr = le.expr.clone();
            if let Some(_) = bindm.get(&le.identifier) {
                return Err(format!("Name error: '{}' is already reserved as identifier.", le.identifier));
            } else {
                let (w_ret, _) = eval(T::vec_to_frozen(expr, &Env::Line)?, bindm)?;
                let mut bm1 = bindm.clone();
                bm1.insert(le.identifier.clone(), w_ret.clone());
                return Ok((w_ret, bm1));
            }
        }
    }
}

pub fn eval_line(s: &String, bindm: &Bind<Word>) -> Result<(Word, Bind<Word>), String> {
    let frozen = Frozen::from_string(s)?;

    let bmt = bindm.to_typebind();

    if let Frozen::FuncDef(_) = frozen.clone() {
        // 単にreturn type checkをするため
        compile_defs(&vec![frozen.to_wordtype()], &bmt)?;
    } else {
        // 型チェック
        eval(frozen.to_wordtype(), &bmt)?;
    }

    return eval(frozen, &bindm);
}

pub fn is_in<T: PartialEq>(x: T, vec: Vec<T>) -> bool {
    for v in vec {
        if v == x {
            return true;
        }
    }
    
    return false;
}

pub fn eval_main(string: &String) -> Result<(), String> {
    let sv: Vec<String> = string.split('\n').map(|s| s.to_string()).collect();
    if sv.len() == 0 {
        return Ok(());
    } else if sv.len() == 1 {
        let (w, _) = eval_line(&sv[0], &Bind::new())?;
        if let Word::PrintSignal(s) = w {
            println!("{}", s);
        }
        return Ok(())
    }

    let mut tvv: Vec<Vec<Token>> = Vec::new();
    let mut prev_tv: Vec<Token> = Token::tokenize(&sv[0])?;
    let mut concat_prev = false;
    for i in 1..sv.len() {
        let tv = Token::tokenize(&sv[i])?;
        if tv.len() == 0 {
            continue;
        }

        // concat_prevがtrueもしくはLINE_BEGIN_TOKENSで始まる行なら
        // 現在の行を前の行にくっつける
        if concat_prev || is_in(tv[0].clone(), LINE_BEGIN_TOKENS.to_vec()) {
            prev_tv.append(&mut tv.clone());
        } else {
            // くっつけないなら前の行はそれで終わりなのでpushする
            tvv.push(prev_tv);
            prev_tv = tv.clone();
        }

        concat_prev = false;
        if let Some(t) = tv.last() {
            if is_in(t.clone(), LINE_END_TOKENS.to_vec()) {
                // LINE_END_TOKENSで終わる行の場合
                concat_prev = true;
            }
        }
    }
    // 最後にここが必要
    tvv.push(prev_tv);

    let mut frozenv: Vec<Frozen<Word>> = Vec::new();
    let mut ftv: Vec<Frozen<WordType>> = Vec::new();
    for tv in tvv {
        let frozen = Frozen::from_tokenv(tv, Env::Line)?;
        ftv.push(frozen.to_wordtype());
        frozenv.push(frozen);
    }

    // Return type checkを実施するため2回走らせる
    let (ftv_other, mut bmt) = compile_defs(&ftv, &Bind::new())?;
    let (frozenv_other, mut bindm) = compile_defs(&frozenv, &Bind::new())?;

    // 型チェック
    for frozen in ftv_other {
        let (_, bmt1) = eval(frozen, &bmt)?;
        bmt = bmt1;
    }

    // 実行部分
    for frozen in frozenv_other {
        let (w1, bm1) = eval(frozen, &bindm)?;
        bindm = bm1;
        match w1 {
            Word::PrintSignal(s) => println!("{}", s),
            Word::ExitSignal => break,
            _ => (),
        }
    }

    return Ok(());
}
