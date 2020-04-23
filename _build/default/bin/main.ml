open Base
open Stdio
open Rayml

module Sphere_lst = Hittable_list.Make(Sphere)

let rec ray_color r world n = if n = 0 then Vector.zero else 
    match Sphere_lst.hit world r 0.001 Float.infinity with
    | Some closest_hit -> let p = closest_hit.p in
      let target = Vector.(p + closest_hit.normal + random_unit_vector ()) in
      let r' = Ray.of_tuple (p, Vector.(target - p)) in
      let col = ray_color r' world (n - 1) in
      Vector.(col * 0.5)
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
  let world = Sphere.([
    of_tuple (Vector.of_tuple (0., 0., -1.), 0.5);
    of_tuple (Vector.of_tuple (0., -100.5, -1.), 100.)
  ]) in
  let camera = Camera.default in
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
