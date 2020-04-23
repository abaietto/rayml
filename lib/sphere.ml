open Base

type t = {center : Vector.t; radius : float; material : Material.t}

let of_tuple (center, radius, material) = {center; radius; material}

type hitmark = {t : float;
                p : Vector.t; 
                normal : Vector.t; 
                material : Material.t;
                front_face: bool}

let hit sphere (r : Ray.t) t_min t_max =
  let oc = Vector.(r.origin - sphere.center) in
  let a = Vector.dot r.direction r.direction in
  let half_b = Vector.dot oc r.direction in
  let c = Vector.(dot oc oc -. sphere.radius **. 2.) in
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
      Some {t; p; normal; front_face; material} in 
    let t1 = (-.half_b -. root) /. a in
    let t2 = (-.half_b +. root) /. a in
    if Float.(t1 < t_max && t1 > t_min) then hit_record t1 
    else if Float.(t2 < t_max && t2 > t_min) then hit_record t2
    else None
  else None 

let scatter material (r : Ray.t) hitmark =
  match material with
  | Material.Lambertian v ->
    let scatter_direction = Vector.(hitmark.normal + random_unit_vector ()) in
    let scattered = Ray.of_tuple (hitmark.p, scatter_direction) in
    Some (scattered, v)
  | Material.Metal (v, f) -> 
    let fuzz = if Float.(f < 1.) then f else 1. in
    let reflected = Vector.(reflect (unit_vector r.direction) hitmark.normal) in
    let scattered = 
      Ray.of_tuple 
        (hitmark.p, Vector.(reflected + random_unit_sphere () * fuzz)) in
    let projection = Vector.dot scattered.direction hitmark.normal in
    if Float.(projection > 0.) then Some (scattered, v) else None
  | Material.Dielectric refract_idx -> 
    let attenuation = Vector.of_tuple (1., 1., 1.) in
    let etai_over_etat = if hitmark.front_face then 1. /. refract_idx 
      else refract_idx in
    let unit_direction = Vector.unit_vector r.direction in
    let cos_theta = Float.min Vector.(dot ~-unit_direction hitmark.normal) 1. in
    let sin_theta = Float.sqrt (1. -. cos_theta **. 2.) in
    if Float.(etai_over_etat *. sin_theta > 1.) then 
      let reflected = Vector.reflect unit_direction hitmark.normal in
      let scattered = Ray.of_tuple (hitmark.p, reflected) in
      Some (scattered, attenuation) else
      let reflect_prob = Material.schlick cos_theta etai_over_etat in
      if Float.(Random.float 1. < reflect_prob) then
        let reflected = Vector.reflect unit_direction hitmark.normal in
        let scattered = Ray.of_tuple (hitmark.p, reflected) in
        Some (scattered, attenuation) else
        let refracted = 
          Vector.refract unit_direction hitmark.normal etai_over_etat in
        let scattered = Ray.of_tuple (hitmark.p, refracted) in
        Some (scattered, attenuation) 

