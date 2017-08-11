open Core;;
open Printf;;

open Payoff_function;;
open Pricing_logic;;
open Model;;
open Motion;;


let () =
  print_endline "Starting\n"


let time1 = Span.of_day 180.0 |> (Time.add (Time.now ()))
let time2 = Span.of_day 180.0 |> (Time.add time1)
let time3 = Span.of_day 180.0 |> (Time.add time2);;



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
  let p = Binomial_tree.make (BINOMIAL_TREE {start_date=time1;end_date = time3;depth = 12}) in
  match p with
  | None -> failwith "You failed miserably"
  | Some p ->
    Binomial_tree.string_of_t p |> print_endline;;



print_endline"\nTest Motion module";;
let () =
  let bparam = Motion.Bachelier.make_parameter (P_BACHELIER {mean=0.00;st_dev=10.0}) in
  let bstate = Motion.Bachelier.make_state (S_BACHELIER 100.0) in
  let () =
      match bparam,bstate with
      | None,_ |_,None -> print_endline "You failed miserably"
      | Some bparam, Some bstate ->
        let ds = Motion.Bachelier.ds bparam bstate 0.01 0.01 in
        let call = make_european_call time2 110.0 in
        let cprice_fun = Motion.Bachelier.get_closed_form call in
        match cprice_fun with
        | None -> failwith "You failed miserably"
        | Some cp_fun ->
          let cprice = cp_fun bparam bstate time1 in
          Std.printf "ds = %f\n" ds;
          Std.printf "call price = %f\n" cprice in
  ();;


print_endline"\nTest Model module";;
(*
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
    | Some p, Some s ->  let value = M.evaluate logic p s in
      print_endline (string_of_float value) in
  ()
(*module M = Make_model (Bachelier) (Binomial_tree);;*)

*)


print_endline "Test Math module";;

let () =
  let test_val = 2.0 in
  let test_val2 = 0.01 in
  let test_val3 = -0.5 in
  let test_val4 = 3.0 in
  Std.printf "Pi = %f\n" Math_util.pi;
  Std.printf "erf %f = %f\n" test_val (Math_util.erf test_val);
  Std.printf "erf %f = %f\n" test_val2 (Math_util.erf test_val2);
  Std.printf "erf %f = %f\n" test_val3 (Math_util.erf test_val3);
    Std.printf "erf %f = %f\n" test_val4 (Math_util.erf test_val4)
