open Base
open Stdio

type t = {x : float; y : float; z : float}

let of_tuple (x, y, z) = {x = x; y = y; z = z}

let (~-) {x; y; z} = {x = ~-.x; y = ~-.y; z = ~-.z}

let (+) u v = {x = u.x +. v.x; y = u.y +. v.y; z = u.z +. v.z}

let (-) u v = {x = u.x -. v.x; y = u.y -. v.y; z = u.z -. v.z}

let ( * ) {x; y; z} c = {x = c *. x; y = c *. y; z = c *. z}

let (/) {x; y; z} c = {x = x /. c; y = y /. c; z = z /. c}

let mult u v = {x = u.x *. v.x; y = u.y *. v.y; z = u.z *. v.z}

let dot u v = u.x *. v.x +. u.y *. v.y +. u.z *. v.z

let cross u v = {x = u.y *. v.z -. u.z *. v.y; y = u.z *. v.x -. u.x *. v.z; z = u.x *. v.y -. u.y *. v.x}

let length u = Float.sqrt (dot u u)

let unit_v v = v / (length v)

let write {x; y; z} = printf "%i %i %i\n" (Int.of_float x) (Int.of_float y) (Int.of_float z)

let write_color {x; y; z} = printf "%i %i %i\n" (Int.of_float (255.999 *. x)) (Int.of_float (255.999 *. y)) (Int.of_float (255.999 *. z))


