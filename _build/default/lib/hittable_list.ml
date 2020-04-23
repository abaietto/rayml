open Base

module type Hittable = sig
  type t
  type hitmark = {t : float;
                  p : Vector.t; 
                  normal : Vector.t; 
                  front_face : bool}

  val hit : t -> Ray.t -> float -> float -> hitmark option
end

module Make (Obj : Hittable) = struct
  type t = Obj.t list

  let hit lst r t_min t_max = 
    let first_hit first_so_far obj = match first_so_far with
      | Some h -> (match Obj.hit obj r t_min h.Obj.t with
        | Some h' -> Some h'
        | None -> Some h) 
      | None -> Obj.hit obj r t_min t_max in
    List.fold lst ~init:None ~f:first_hit 
end

