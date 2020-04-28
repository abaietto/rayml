open Base
open Stdio

type t = {x : float; y : float; z : float}

let create x y z = {x; y; z}

let zero = create 0. 0. 0.

let i = {zero with x = 1.}
let j = {zero with y = 1.}
let k = {zero with z = 1.}

let (~-) {x; y; z} = {x = ~-.x; y = ~-.y; z = ~-.z}

let (+) u v = {x = u.x +. v.x; y = u.y +. v.y; z = u.z +. v.z}

let (-) u v = {x = u.x -. v.x; y = u.y -. v.y; z = u.z -. v.z}

let ( * ) {x; y; z} c = {x = c *. x; y = c *. y; z = c *. z}

let (/) {x; y; z} c = {x = x /. c; y = y /. c; z = z /. c}

let mult u v = {x = u.x *. v.x; y = u.y *. v.y; z = u.z *. v.z}

let dot u v = u.x *. v.x +. u.y *. v.y +. u.z *. v.z

let cross u v = {x = u.y *. v.z -. u.z *. v.y; 
                 y = u.z *. v.x -. u.x *. v.z; 
                 z = u.x *. v.y -. u.y *. v.x}

let norm_sq v = dot v v

let norm v = Float.sqrt (norm_sq v)

let unit_vector v = v / (norm v)

let reflect v n = v - n * dot v n * 2.

let refract v n etai_over_etat = 
  let cos_theta = dot ~-v n in
  let r_out_parallel = (v + n * cos_theta) * etai_over_etat in
  let r_out_perp = ~-n * Float.sqrt (1. -. dot r_out_parallel r_out_parallel) in
  r_out_parallel + r_out_perp

let random range = {x = Random.float range;
                    y = Random.float range;
                    z = Random.float range
                   }

let random_range min max = 
  let x = Random.float_range min max in
  let y = Random.float_range min max in
  let z = Random.float_range min max in
  {x; y; z}

let rec random_in_unit_sphere () = 
  let p = random_range (-1.) 1. in
  if Float.(norm_sq p >= 1.) then random_in_unit_sphere () else p

let random_unit_vector () = 
  let a = Random.float_range 0. (2. *. Float.pi) in
  let z = Random.float_range (-1.) 1. in
  let r = Float.sqrt (1. -. z **. 2.) in
  {x = r *. Float.cos a; y = r *. Float.sin a; z = z}

let rec random_in_unit_disk () = 
  let p = {x = Random.float_range (-1.) 1.;
           y = Random.float_range (-1.) 1.;
           z = 0.} in
  if Float.(norm_sq p >= 1.) then random_in_unit_disk () else p

let write {x; y; z} = printf "%i %i %i\n" 
                        (Int.of_float x) (Int.of_float y) (Int.of_float z)

let write_color {x; y; z} samples_per_pixel =
  let scale = 1. /. (Float.of_int samples_per_pixel) in
  let r = Float.sqrt (scale *. x) in
  let g = Float.sqrt (scale *. y) in
  let b = Float.sqrt (scale *. z) in
  let get_color col = match Float.clamp col ~min:0. ~max:0.999 with 
    | Ok n -> Int.of_float (256. *. n)
    | Error _ -> 255 in
  printf "%i %i %i\n" (get_color r) (get_color g) (get_color b)

