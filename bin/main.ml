open Base
open Stdio
open Rayml

let hit lst r t_min t_max =
    let first_hit first_so_far obj = match first_so_far with
      | Some h -> (match Sphere.hit obj r t_min h.Sphere.t with
        | Some h' -> Some h'
        | None -> Some h)
      | None -> Sphere.hit obj r t_min t_max in
    List.fold lst ~init:None ~f:first_hit

let rec ray_color r world n = if n = 0 then Vector.zero else 
    match hit world r 0.001 Float.infinity with
    | Some closest_hit -> 
      let material = closest_hit.material in
      (match Sphere.scatter material r closest_hit with
       | Some (scattered, attenuation) -> 
         let col = ray_color scattered world (n - 1) in
         Vector.mult col attenuation
       | None -> Vector.zero)
    | None -> let unit_direction = Vector.unit_vector r.direction in
      let t = 0.5 *. (unit_direction.y +. 1.) in
      Vector.((of_tuple (1., 1., 1.) * (1. -. t)) + (of_tuple (0.5, 0.7, 1.) * t))

let main () =
  let im_width = 200 in
  let im_height = 100 in
  let samples_per_pixel = 100 in
  let max_depth = 50 in
  print_string ("P3\n" ^ (Int.to_string im_width) ^ " " ^ 
                (Int.to_string im_height) ^ "\n255\n");

  let world = [
    Sphere.of_tuple (Vector.of_tuple (0., 0., -1.), 0.5, Material.Lambertian (Vector.of_tuple (0.1, 0.2, 0.5)));
    Sphere.of_tuple (Vector.of_tuple (0., -100.5, -1.), 100., Material.Lambertian (Vector.of_tuple (0.8, 0.8, 0.0)));
    Sphere.of_tuple (Vector.of_tuple (1., 0., -1.), 0.5, Material.Metal (Vector.of_tuple (0.8, 0.6, 0.2), 0.3));
    Sphere.of_tuple (Vector.of_tuple (-1., 0., -1.), 0.5, Material.Dielectric 1.5);
    Sphere.of_tuple (Vector.of_tuple (-1., 0., -1.), -0.45, Material.Dielectric 1.5);
  ] in

  let look_from = Vector.of_tuple (-2., 2., 1.) in
  let look_at = Vector.of_tuple (0., 0., -1.) in
  let vup = Vector.of_tuple (0., 1., 0.) in
  let fov = 30. in
  let aspect = (Float.of_int im_width) /. (Float.of_int im_height) in
  let camera = Camera.of_tuple (look_from, look_at, vup, fov, aspect) in

  for i = (im_height - 1) downto 0 do
    eprintf "\rScanlines remaining: %i" i; Out_channel.flush stderr;
    for j = 0 to (im_width - 1) do
      let eval_color _ =
        let u = Float.((of_int j + Random.float 1.) /. of_int im_width) in
        let v = Float.((of_int i + Random.float 1.) /. of_int im_height) in
        let r = Camera.get_ray camera u v in
        ray_color r world max_depth in        
      let colors = List.init samples_per_pixel ~f:eval_color in
      let cum_color = List.fold colors ~init:Vector.zero ~f:Vector.(+) in
      Vector.write_color cum_color samples_per_pixel
    done;
  done;
  prerr_endline "\nDone"

let () = main ()
