print({})
{}
print({{}})

let s = {}
print f(f(s)) % {{},{{}}}
print g(s, f(f(s))) % {{}}

% いっこ前を得る関数
def g: Set, Set -> Set; s, t -> if f(s) == t then s else g(f(s), t)

% 後続数を得る関数
def f: Set -> Set; s -> s + {s}

% 要素の中で一番ネストが深い要素を返す関数