open Core;;
open Make_path;;

let get_paths s0 mu sigma n_val n_paths delta_time=
  let rec aux acc n =
    if n == 0 then
      acc
    else
      let new_p = black_scholes_process s0 mu sigma n_val delta_time in
      aux (new_p :: acc) (n - 1) in
  aux [] n_paths


let mean li =
  let rec aux acc n = function
    | [] -> acc /. float_of_int n
    | h :: t ->
      aux (h +. acc) (n + 1) t in
  aux 0.0 0 li

let discount rate time value =
  value *. exp (-. rate *. time)

let price exp_year s0 mu sigma n_val n_paths payoff =
  let delta_time = exp_year /. (float_of_int n_val) in
  let paths = get_paths s0 mu sigma n_val n_paths delta_time in
  let poffs = List.map payoff paths in
  mean poffs
  |> discount mu exp_year
