use std::cmp::Ordering;

pub trait SetLike<T> where T: std::clone::Clone, T: SetLike<T> {
    fn contents(self: &Self) -> &Vec<T>;
    fn new(contents: Vec<T>) -> Self;

    fn len(self: &Self) -> usize {
        return self.contents().len();
    }

    fn is_empty(self: &Self) -> bool {
        return self.contents().is_empty();
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
        return self.contents().iter();
    }
}

#[derive(Clone)]
pub struct SetList {
    contents: Vec<Set>,
}

impl SetLike<Set> for SetList {
    fn new(contents: Vec<Set>) -> Self {
        return Self {contents: contents};
    }

    fn contents(self: &Self) -> &Vec<Set> {
        return &self.contents;
    }
}

impl SetList {
    // 一意化及びソートを行う
    pub fn uniquify(self: &Self) -> Set {
        if self.is_empty() {
            return Set::new(Vec::new());
        }

        let mut sv: Vec<Set> = self.contents.clone();

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

        return Set {contents: sv_ret};
    }
}

#[derive(Clone, Debug)]
pub struct Set {
    contents: Vec<Set>,
}

impl SetLike<Self> for Set {
    fn new(contents: Vec<Self>) -> Self {
        return Self {contents: contents};
    }

    fn contents(self: &Self) -> &Vec<Self> {
        return &self.contents;
    }
}

impl Set {
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
            let tmp = Self::cmp(&a.contents[i], &b.contents[i]);
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

    pub fn is_in(&self, set: &Self) -> bool {
        for e in set.contents() {
            if self == e {
                return true;
            }
        }
        return false;
    }

    fn to_setlist(&self) -> SetList {
        return SetList { contents: self.contents.clone() };
    }

    pub fn set_union(set1: &Set, set2: &Set) -> Set {
        let mut contents1 = set1.to_setlist().contents;
        let mut contents2 = set2.to_setlist().contents;
        contents1.append(&mut contents2);
        let sl = SetList { contents: contents1 };
        return sl.uniquify();
    }

    pub fn set_intersec(set1: &Set, set2: &Set) -> Set {
        let mut contents: Vec<Set> = Vec::new();

        for s in set1.iter() {
            if s.is_in(set2) {
                contents.push(s.clone());
            }
        }

        return Set {contents: contents};
    }

    pub fn set_diff(set1: &Set, set2: &Set) -> Set {
        let mut contents: Vec<Set> = Vec::new();

        for s in set1.iter() {
            if !s.is_in(set2) {
                contents.push(s.clone());
            }
        }

        return Set {contents: contents};
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
