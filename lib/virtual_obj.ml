type t = 
  | Sphere of Sphere.t

let hit = function
  | Sphere s -> Sphere.hit s


