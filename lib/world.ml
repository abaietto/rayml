open Base

type t = Sphere.t list

let hit lst r t_min t_max =
  let first_hit first_so_far obj = match first_so_far with
    | Some h -> (match Sphere.hit obj r t_min h.Sphere.t with
      | Some h' -> Some h'
      | None -> Some h)
    | None -> Sphere.hit obj r t_min t_max in
  List.fold lst ~init:None ~f:first_hit

let random_scene () = 
  let generate_sphere a b = 
    let choose_mat = Random.float 1. in
    let x = a +. 0.9 *. Random.float 1. in
    let y = 0.2 in
    let z = b +. 0.9 *. Random.float 1. in
    let center = Vector.of_tuple (x, y, z) in
    if Float.(Vector.(length (center - of_tuple (4., 0.2, 0.))) > 0.9) then
      if Float.(choose_mat < 0.8) then
        let albedo = Vector.(mult (random 1.) (random 1.)) in
        Some (Sphere.of_tuple (center, 0.2, Material.Lambertian albedo))
      else if Float.(choose_mat < 0.95) then
        let albedo = Vector.random_range 0.5 1. in
        let fuzz = Random.float_range 0. 0.5 in
        Some (Sphere.of_tuple (center, 0.2, Material.Metal (albedo, fuzz))) else
        Some (Sphere.of_tuple (center, 0.2, Material.Dielectric 1.5))
    else None in

  let balls_opt = List.init 22 ~f:(fun a -> List.init 22 ~f:(fun b -> generate_sphere (Float.of_int (a - 11)) (Float.of_int (b - 11)))) in
  let balls = balls_opt |> List.join |> List.filter_opt in

  let big1 = Sphere.of_tuple 
               (Vector.of_tuple (0., 1., 0.), 1., Material.Dielectric 1.5) in
  let big2 = Sphere.of_tuple 
               (Vector.of_tuple (-4., 1., 0.), 1., 
                Material.Lambertian (Vector.of_tuple (0.4, 0.2, 0.1))) in
  let big3 = Sphere.of_tuple 
               (Vector.of_tuple (4., 1., 0.), 1., 
                Material.Metal (Vector.of_tuple (0.7, 0.6, 0.5), 0.)) in
  let ground = Sphere.of_tuple 
                 (Vector.of_tuple (0., -1000., 0.), 1000., 
                  Material.Lambertian (Vector.of_tuple (0.5, 0.5, 0.5))) in

  let world = big1 :: big2 :: big3 :: ground :: balls in world
