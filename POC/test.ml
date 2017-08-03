open Core;;
open Make_path;;
open Payoff;;
open Price;;


let print_float_list li =
  let rec aux = function
    | [] -> print_endline "Done";
    | h :: t -> print_float h; print_string " "; aux t in
  aux li


let () =
  (*
  let exp_year = 1.0 in
  let n_val = 100 in
  let mu = 0.1 in
  let s0 = 100.0 in
  let sigma = 0.1 in
  let n_paths = 100 in
  let delta_time = exp_year /. (float_of_int n_val) in
  let mu_act = mu *. delta_time in
  let sigma_act = sigma *. sqrt (delta_time) in
  let paths = get_paths s0 mu_act sigma_act n_val n_paths in
  match paths with
  | [] -> failwith "OSEF"
  | h :: t ->
    print_float_list h *)
  let price =
    call_payoff 90.0
    |> price 2.0 100.0 0.01 0.10 10000 100 in
  Std.printf "call price: %f\n" price

  
