print (if (let s = {}) in {{}} then s else {s})
let s = {{}}
print is_empty(s)

def f: Set -> Set; s -> (let t={s} | t + s)
print f({})

def h: Bool -> Set; b -> if b then {} else {{}}
print h(false)

def fact: Number -> Number; n -> if n == 0 then 1 else n * fact(n - 1)
print fact(5)

$(
    let t = f(s) |
    print t
)

let t = 2
print t + 1
