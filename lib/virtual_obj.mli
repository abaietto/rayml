open Base

type t = 
  | Sphere of Sphere.t

val hit : t -> Ray.t -> float -> float -> Material.hitmark option

