%print({{{}}, {}})

%let s = {}
%print succ(succ(s)) % {{},{{}}}
%print prev(succ(succ(s))) % {{}}

% いっこ前を得る関数(安全でない)
def g: Set, Set -> Set; s, t -> if succ(s) == t then s else g(succ(s), t)
def prev: Set -> Set; s -> g({}, s)

% ノイマン表示の後続数を得る関数
def succ: Set -> Set; s -> s + {s}

% ノイマン表示から{{...}}表示に変換する関数
def from_n: Set -> Set; s -> if s == {} then {} else {from_n(prev(s))}
%print from_n(succ(succ({}))) %{{{}}}

% ノイマン集合かどうか判定する関数
def is_neumann: Set -> Bool; s -> if is_empty(s) then true else (let m = get_max(s) | is_neumann(m) && succ(m) == s)

% 集合を順番に並べたときの次の集合を得る関数
% sがノイマン集合なら{{...}}表示に直して{}で囲う．
% そうでないなら，t=get_target(s)とする．tが空でないならsのtをnext(t)で置換する
% tが空なら，(t in sならtをnext(t)で置換，そうでないならsにtを追加)
def next: Set -> Set; s -> if is_neumann(s) then {from_n(s)} else (let t = get_target(s) | if t in s then s - {t} + {next(t)} else s + {t})

% 要素の中で，次に更新すべき要素を得る関数
% t=sとする
% 過去を遡り，sの要素の中で一番大きな集合mを見つける
% mがノイマン集合でないならばmを返す．そうでないならsからmを削除し，t=mとして次へ
% (*)sの要素の中で一番大きな集合mを見つける．mがノイマン集合かつsucc(m)がtに一致するならば，sからmを削除し，t=mとして(*)へ
% そうでないならmを返す
% 注:sがノイマン集合なら{}が返る．{4,3}なら{}が返る．{4,3,2,1}でも{}が返る．{4,3,{}}でも{}が返る
def get_target: Set -> Set; s -> if (let m = get_max(s) | !is_neumann(m)) then m else _get_target(s - {m}, m)
def _get_target: Set, Set -> Set; s, t -> if (let m = get_max(s) | is_neumann(m) && succ(m) == t) then _get_target(s - {m}, m) else m

% sの要素の中で最大の集合を見つける s: ターゲット集合, t, u: 探索集合
% sが空なら{}が返る．sが{{}}でも{}が返る．
def get_max: Set -> Set; s -> _get_max(s, {}, {})
def _get_max: Set, Set, Set -> Set; s, t, u -> if t == s then u else (let w = if t in s then t else u | _get_max(s, next(t), w))

print is_neumann({{}})
print is_neumann({{{}}})

let s = succ(succ({}))
%let s = succ(succ(succ({})))
print s
print is_neumann(s)
print get_max(s)
print next(s)


def get_nth: Number -> Set; n -> if n == 0 then {} else next(get_nth(n - 1))
print get_nth(0)
print get_nth(1)
print get_nth(2)
print get_nth(3)
print get_nth(4)
print get_nth(5)
