use std::cmp::Ordering;

fn main() -> std::io::Result<()> {
    loop{
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer).expect("Fail to read line.");
        buffer.pop();
        if buffer == "exit" {
            break;
        }
        if buffer == "" {
            continue;
        }
        match Set::create(buffer) {
            Ok(set) => println!("{}", set.to_string()),
            Err(e) => println!("Error: {}", e),
        }
    }
    Ok(())
}

struct SetList {
    content: Vec<SetList>,
}

impl SetList {
    fn create(s: String) -> Result<SetList, String> {
        match peal(s) {
            Ok(pealed) => {
                if pealed == "" {
                    return Ok(SetList {content: Vec::new()});
                };
                let mut slv: Vec<SetList> = Vec::new();
                for t in pealed.split(',') {
                    match SetList::create(t.to_string()) {
                        Err(e) => return Err(e.to_string()),
                        Ok(sl) => slv.push(sl),
                    }
                };
                Ok(SetList {content: slv})
            },
            Err(e) => Err(e),
        }
    }
    
    fn copy(self: &Self) -> SetList {
        return SetList {content: self.content.iter().map(|x| x.copy()).collect()};
    }

    // 一意化及びソートを行う
    fn uniquify(self: &Self) -> Set {
        if self.content.is_empty() {
            return Set {content: Vec::new()};
        }

        // まず，各要素をuniquify
        let mut sv: Vec<Set> = self.content.iter().map(|x| x.uniquify()).collect();

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
        if self.content.is_empty() {
            return "{}".to_string();
        }
        let str_ls: Vec<String> = self.content.iter().map(|x| x.to_string()).collect();
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

    fn copy(self: &Self) -> Set {
        return Set {content: self.content.iter().map(|x| x.copy()).collect()};
    }

    fn to_string(self: &Self) -> String {
        if self.content.is_empty() {
            return "{}".to_string();
        }
        let str_ls: Vec<String> = self.content.iter().map(|x| x.to_string()).collect();
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
    if a.content.is_empty() {
        if b.content.is_empty() {
            return Ordering::Equal;
        } else {
            return Ordering::Less;
        }
    } else if b.content.is_empty() {
        return Ordering::Greater;
    }
    let n = std::cmp::min(a.content.len(), b.content.len());
    for i in 0..n {
        let tmp = set_cmp(&a.content[i], &b.content[i]);
        if tmp != Ordering::Equal {
            return tmp;
        }
    }

    if a.content.len() < b.content.len() {
        return Ordering::Less;
    } else if a.content.len() > b.content.len() {
        return Ordering::Greater;
    } else {
        return Ordering::Equal;
    }
}

