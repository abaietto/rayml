open Base
open Stdio
open Rayml

let main () =
        let im_width = 2000 in
        let im_height = 1000 in
        print_string ("P3\n" ^ (Int.to_string im_width) ^ " " ^ (Int.to_string im_height) ^ "\n255\n");
        let init = List.init im_height
        ~f:(fun i -> List.init im_width ~f:(fun j -> (im_height - 1 - i, j))) in
        let print_colors (m, n) =
                let r = (Float.of_int n) /. (Float.of_int im_width) in
                let g = (Float.of_int m) /. (Float.of_int im_height) in
                let b = 0.2 in
                Vector.(write_color (of_tuple (r, g, b))) in
        List.iteri init ~f:(fun i -> List.iter ~f:(eprintf "\rScanlines remaining: %i" (im_height - 1 - i); Out_channel.flush stderr; print_colors));
        prerr_endline "\nDone"

let _ = main ()
