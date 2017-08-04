open Core;;
open Payoff_function;;
open Model;;


print_endline "First part: test payoffs"
let () =
  let p = [(Time.now (),100.0);(Time.now (),105.0);(Time.now (),110.0)] in
  let call = make_call (Time.now ()) 100.0 in
  let barrier = make_knock_out_call 105.0 (Time.now ()) 100.0 in
  let barrier2 = make_knock_out_call 120.0 (Time.now ()) 95.0 in
  let path = Path.t_of_time_value_list p in
  Std.printf "value of the call = %f
value of the barrier 1 = %f
value of the barrier 2 = %f\n" (evaluate call path) (evaluate barrier path) (evaluate barrier2 path);;


print_endline "\nSecond part: test models"
let () =
  let call = make_call (Time.now ()) 100.0 in
  let rec aux = function
    | [] -> ()
    | h :: t ->
      let poff = (evaluate call h) |> string_of_float in
      (h |> Path.string_of_t) ^ " - call payooff = " ^ poff |> print_endline;
      aux t in
  let m = Binomial_tree.make_test () in
  m |> Binomial_tree.paths_of_model |> aux;;
  


