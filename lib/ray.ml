type t = {origin : Vector.t; direction : Vector.t}

let create origin direction = {origin; direction}

let eval r t = Vector.(r.origin + r.direction * t)

