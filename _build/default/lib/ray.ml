type t = {origin : Vector.t; direction : Vector.t}

let of_tuple (a, b) = {origin = a; direction = b}

let eval r t = Vector.(r.origin + r.direction * t)

