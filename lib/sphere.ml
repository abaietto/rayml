open Base

type t = {center : Vector.t; radius : float; material : Material.t}

let create ~center ~radius material = {center; radius; material}

let hit sphere (r : Ray.t) t_min t_max =
  let oc = Vector.(r.origin - sphere.center) in
  let a = Vector.norm_sq r.direction in
  let half_b = Vector.dot oc r.direction in
  let c = Vector.(norm_sq oc -. sphere.radius **. 2.) in
  let discriminant = half_b **. 2. -. a *. c in
  if Float.(discriminant > 0.) then
    let root = Float.sqrt discriminant in
    let hit_record t =
      let material = sphere.material in
      let p = Ray.eval r t in
      let outward_normal = Vector.((p - sphere.center) / sphere.radius) in
      let front_face = Float.(Vector.dot r.direction outward_normal < 0.) in
      let normal =
        if front_face then outward_normal else Vector.(~-outward_normal) in
      Some Material.{t; p; normal; front_face; material} in 
    let t1 = (-.half_b -. root) /. a in
    let t2 = (-.half_b +. root) /. a in
    if Float.(t1 < t_max && t1 > t_min) then hit_record t1 
    else if Float.(t2 < t_max && t2 > t_min) then hit_record t2
    else None
  else None 

