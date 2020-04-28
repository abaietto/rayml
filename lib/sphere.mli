open Base

type t

val create : center:Vector.t -> radius:float -> Material.t -> t

val hit : t -> Ray.t -> float -> float -> Material.hitmark option

