open Core;;

let pi = 4.0 *. atan 1.0

let bm_r u1 =
  sqrt (-. 2.0 *. log u1)

let bm_theta u2 =
  2.0 *. pi *. u2

let bm_z0 r theta =
  r *. cos theta

let bm_z1 r theta =
  r *. sin theta

let scaled_gauss mu sigma z =
  mu +. sigma *. z

let box_muller_step mu sigma =
  let u1 = Random.float 1.0 in
  let u2 = Random.float 1.0 in
  let r = bm_r u1 in
  let theta = bm_theta u2 in
  let z0 = bm_z0 r theta in
  let z1 = bm_z1 r theta in
  (scaled_gauss mu sigma z0, scaled_gauss mu sigma z1)
      

let generate_path ?mean:(mu=0.0) ?st_dev:(sigma=1.0) n_val =
  let () =
    Random.self_init () in
  let rec aux acc n =
    if n == 0 then
      acc
    else
      let z0_and_z1 = box_muller_step mu sigma in
      match z0_and_z1 with
      | (z0,z1) ->
        if n == 1 then
          z0 :: acc
        else
          aux (z0 :: z1 :: acc) (n - 2) in
  aux [] n_val

let black_scholes_process s0 mu sigma n_val delta_time =
  let rand_val = generate_path ~mean:0.0 ~st_dev:(sqrt(delta_time)) (n_val - 1) in
  let rec aux acc w tme = function
    | [] -> acc
    | h :: t ->
      let new_w = w +. h in
      let new_tme = tme +. delta_time in
      let new_s = s0 *.
                  exp (sigma *. new_w -. (0.5 *. sigma ** 2.0) *. new_tme +. mu *. new_tme ) in
aux (new_s :: acc) new_w new_tme t in
  List.rev (aux [s0] 0.0 0.0 rand_val)
  
