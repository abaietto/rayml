open Base

type t = {origin : Vector.t;
          lower_left : Vector.t;
          vertical : Vector.t;
          horizontal : Vector.t
         }

let default = {origin = Vector.of_tuple (0., 0., 0.); 
               lower_left = Vector.of_tuple (-2., -1., -1.);
               vertical = Vector.of_tuple (0., 2., 0.);
               horizontal = Vector.of_tuple (4., 0., 0.)
              }

let of_tuple (look_from, look_at, vup, vfov, aspect) = 
  let origin = look_from in
  let degrees_to_radians degrees = degrees /. 180. *. Float.pi in
  let theta = degrees_to_radians vfov in
  let half_height = Float.tan (theta /. 2.) in
  let half_width = aspect *. half_height in
  let w = Vector.(unit_vector (look_from - look_at)) in
  let u = Vector.(unit_vector (cross vup w)) in
  let v = Vector.cross w u in
  let lower_left = Vector.(origin - u * half_width - v * half_height - w) in
  let horizontal = Vector.(u * (2. *. half_width)) in
  let vertical = Vector.(v * (2. *. half_height)) in
  {origin; lower_left; vertical; horizontal}
  
let get_ray {origin; lower_left; vertical; horizontal} u v = 
  let direction = Vector.(lower_left + horizontal * u + vertical * v - origin) in
  Ray.of_tuple (origin, direction)
