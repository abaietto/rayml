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

let get_ray {origin; lower_left; vertical; horizontal} u v = 
  let direction = Vector.(lower_left + horizontal * u + vertical * v - origin) in
  Ray.of_tuple (origin, direction)
