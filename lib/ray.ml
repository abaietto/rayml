open Base

type t = {a : Vector.t; b : Vector.t}

let of_tuple (a, b) = {a = a; b = b}

let eval r t = Vector.(r.a + r.b * t)


