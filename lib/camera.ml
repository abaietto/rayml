open Base

type t = {origin : Vector.t;
          lower_left : Vector.t;
          vertical : Vector.t;
          horizontal : Vector.t;
          u : Vector.t;
          v : Vector.t;
          w : Vector.t;
          lens_radius : float
         }

let create ~look_from ~look_at ~vup ~vfov 
      ~aspect ~aperture ~focus_dist = 
  let origin = look_from in
  let lens_radius = aperture /. 2. in
  let degrees_to_radians degrees = degrees /. 180. *. Float.pi in

  let theta = degrees_to_radians vfov in
  let half_height = Float.tan (theta /. 2.) in
  let half_width = aspect *. half_height in

  let w = Vector.(unit_vector (look_from - look_at)) in
  let u = Vector.(unit_vector (cross vup w)) in
  let v = Vector.cross w u in
  let lower_left = Vector.(origin - 
                           u * (focus_dist *. half_width) - 
                           v * (focus_dist *. half_height) - 
                           w * focus_dist) in

  let horizontal = Vector.(u * (2. *. half_width *. focus_dist)) in
  let vertical = Vector.(v * (2. *. half_height *. focus_dist)) in
  {origin; lower_left; vertical; horizontal; u; v; w; lens_radius}

let get_ray cam s t =
  let rd = Vector.(random_in_unit_disk () * cam.lens_radius) in
  let offset = Vector.(cam.u * rd.x + cam.v * rd.y) in
  let start = Vector.(cam.origin + offset) in
  let direction = Vector.(cam.lower_left + 
                          cam.horizontal * s + 
                          cam.vertical * t - 
                          cam.origin - offset) in
  Ray.create start direction

