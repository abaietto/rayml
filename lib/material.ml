open Base

type t = 
  | Lambertian of Vector.t 
  | Metal of Vector.t * float
  | Dielectric of float

let schlick cosine refract_idx = 
  let r0 = (1. -. refract_idx) /. (1. +. refract_idx) in
  let r0_sq = r0 **. 2. in
  r0_sq +. (1. -. r0_sq) *. (1. -. cosine) **. 5.

