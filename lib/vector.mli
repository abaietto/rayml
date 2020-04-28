open Base

type t = {x : float; y : float; z : float}

val create : float -> float -> float -> t

val zero : t
val i : t
val j : t
val k : t

val (~-) : t -> t
val (+) : t -> t -> t
val (-) : t -> t -> t
val ( * ) : t -> float -> t
val (/) : t -> float -> t
val mult : t -> t -> t
val dot : t -> t -> float
val cross : t -> t -> t
val norm_sq : t -> float
val norm : t -> float
val unit_vector : t -> t
val reflect : t -> t -> t
val refract : t -> t -> float -> t
val random : float -> t
val random_range : float -> float -> t
val random_in_unit_sphere : unit -> t
val random_unit_vector : unit -> t
val random_in_unit_disk : unit -> t
val write : t -> unit
val write_color : t -> int -> unit
