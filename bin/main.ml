open Base
open Stdio
open Rayml

let rec ray_color r world n = if n = 0 then Vector.zero else 
    match World.hit world r 0.001 Float.infinity with
    | Some closest_hit -> 
      let material = closest_hit.material in
      (match Material.scatter material r closest_hit with
       | Some (scattered, attenuation) -> 
         let col = ray_color scattered world (n - 1) in
         Vector.mult col attenuation
       | None -> Vector.zero)
    | None -> let unit_direction = Vector.unit_vector r.direction in
      let t = 0.5 *. (unit_direction.y +. 1.) in
      Vector.((create 1. 1. 1. * (1. -. t)) + 
              (create 0.5 0.7 1. * t))

let main () =
  let im_width = 1200 in
  let im_height = 800 in
  let samples_per_pixel = 10 in
  let max_depth = 50 in
  print_string ("P3\n" ^ (Int.to_string im_width) ^ " " ^ 
                (Int.to_string im_height) ^ "\n255\n");

  let aspect = Float.of_int im_width /. Float.of_int im_height in
  let look_from = Vector.create 13. 2. 3. in
  let look_at = Vector.zero in
  let vup = Vector.j in
  let focus_dist = 10. in
  let aperture = 0.1 in
  let vfov = 20. in
  let camera = Camera.create ~look_from ~look_at ~vup ~vfov ~aspect ~aperture ~focus_dist in

  let world = World.random_scene () in

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
