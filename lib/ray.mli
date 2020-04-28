open Base

type t = {origin : Vector.t; direction : Vector.t}

val create : Vector.t -> Vector.t -> t

val eval : t -> float -> Vector.t
                           
