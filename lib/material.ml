open Base

type t = 
  | Lambertian of Vector.t 
  | Metal of Vector.t * float
  | Dielectric of float

type hitmark = {t : float;
                p : Vector.t;
                normal : Vector.t;
                front_face : bool;
                material : t
               }

let scatter material (r : Ray.t) hitmark = 
  match material with
  | Lambertian v ->
    let scatter_direction = Vector.(hitmark.normal + random_unit_vector ()) in
    let scattered = Ray.create hitmark.p scatter_direction in
    Some (scattered, v)
  | Metal (v, f) -> 
    let fuzz = if Float.(f < 1.) then f else 1. in
    let reflected = Vector.(reflect (unit_vector r.direction) hitmark.normal) in
    let scattered = Ray.create hitmark.p 
                      Vector.(reflected + random_in_unit_sphere () * fuzz) in
    let projection = Vector.dot scattered.direction hitmark.normal in
    if Float.(projection > 0.) then Some (scattered, v) else None
  | Dielectric refract_idx -> 
    let attenuation = Vector.create 1. 1. 1. in
    let etai_over_etat = if hitmark.front_face then 1. /. refract_idx 
      else refract_idx in
    let unit_direction = Vector.unit_vector r.direction in
    let cos_theta = Float.min Vector.(dot ~-unit_direction hitmark.normal) 1. in
    let sin_theta = Float.sqrt (1. -. cos_theta **. 2.) in
    if Float.(etai_over_etat *. sin_theta > 1.) then 
      let reflected = Vector.reflect unit_direction hitmark.normal in
      let scattered = Ray.create hitmark.p reflected in
      Some (scattered, attenuation) else
      let schlick cosine refract_idx = 
        let r0 = (1. -. refract_idx) /. (1. +. refract_idx) in
        let r0_sq = r0 **. 2. in
        r0_sq +. (1. -. r0_sq) *. (1. -. cosine) **. 5. in
      let reflect_prob = schlick cos_theta etai_over_etat in
      if Float.(Random.float 1. < reflect_prob) then
        let reflected = Vector.reflect unit_direction hitmark.normal in
        let scattered = Ray.create hitmark.p reflected in
        Some (scattered, attenuation) else
        let refracted = 
          Vector.refract unit_direction hitmark.normal etai_over_etat in
        let scattered = Ray.create hitmark.p refracted in
        Some (scattered, attenuation) 

