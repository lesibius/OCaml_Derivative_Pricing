open Core;;
open Payoff_function;;
open Pricing_logic;;
open Model;;
open Motion;;

let time1 = Time.of_filename_string "2017-08-04_13-19-00.000";;
let time2 = Time.of_filename_string "2017-08-05_13-19-00.000";;
let time3 = Time.of_filename_string "2017-08-06_13-19-00.000";;


print_endline "First part: test payoffs"
let () =
  let p = [(time1,100.0);(time2,105.0);(time3,110.0)] in
  let call = make_call time2 100.0 in
  let barrier = make_knock_out_call 105.0 time3 100.0 in
  let barrier2 = make_knock_out_call 120.0 time3 95.0 in
  let path = Path.t_of_time_value_list p in
  Std.printf "value of the call = %f
value of the barrier 1 = %f
value of the barrier 2 = %f\n" (evaluate call path) (evaluate barrier path) (evaluate barrier2 path);;


print_endline "\nSecond part: test Logic module"
let () =
  let call = make_call time2 100.0 in
  let rec aux = function
    | [] -> ()
    | h :: t ->
      let poff = (evaluate call h) |> string_of_float in
      (h |> Path.string_of_t) ^ " - call payooff = " ^ poff |> print_endline;
      aux t in
  let m = Binomial_tree.make_test () in
  m |> Binomial_tree.paths_of_logic |> aux;;


print_endline"\nTest Motion module";;
let () =
  let bparam = Motion.Bachelier.parameter_of_list [0.01;0.15] in
  let bstate = Motion.Bachelier.state_of_list [100.0] in
  let () =
      match bparam,bstate with
      | None,_ |_,None -> print_endline "You failed miserably"
      | Some bparam, Some bstate ->
        let ds = Motion.Bachelier.ds bparam bstate 0.01 0.01 in
        Std.printf "ds = %f\n" ds in
  ();;


print_endline"\nTest Model module";;
let () =
  let bachelier = (module Bachelier:Motion_intf) in
  let tree = (module Binomial_tree:Pricing_logic_intf) in
  let module Tmp_motion = (val bachelier) in
  let module Tmp_logic = (val tree) in
  let module M = Make_model (Tmp_motion) (Tmp_logic) in
  let param = M.parameter_of_list [0.01;10.0] in
  let state = M.state_of_list [100.0] in
  let logic = M.make_test () in
  let () =
    match param, state with
    | None, _ | _, None -> failwith "You failed miserably"
    | Some p, Some s ->  let value = M.evaluate p s logic in
      print_endline (string_of_float value) in
  ()
(*module M = Make_model (Bachelier) (Binomial_tree);;*)
