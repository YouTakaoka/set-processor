def f: Set -> Set; s -> (let t={s} | t + s)
print f({})

def h: Bool -> Set; b -> if b then {} else {{}}
print h(false)
