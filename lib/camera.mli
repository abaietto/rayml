open Base

type t

val create : look_from:Vector.t -> look_at:Vector.t -> vup:Vector.t -> 
  vfov:float -> aspect:float -> aperture:float -> focus_dist:float -> t

val get_ray : t -> float -> float -> Ray.t

