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

let p = [(time1,100.0);(time2,105.0);(time3,110.0)] 
let call = make_call time2 100.0 
let call_european = make_european_call time3 100.0 
let put = make_put time3 100.0 
let european_put = make_european_put time3 100.0
let forward1 = make_forward time2 None
let forward2 = make_forward time2 (Some 100.0)
let barrier = make_knock_out_call 105.0 time3 100.0
let barrier2 = make_knock_out_call 120.0 time3 95.0
let path = Path.t_of_time_value_list p;;


print_endline "First part: test payoffs"
let () =
  Std.printf "value of the call = %f\n" (evaluate call path);
  Std.printf "value of the european call = %f\n" (evaluate call_european path);
  Std.printf "value of the put = %f\n" (evaluate put path);
  Std.printf "value of the european put = %f\n" (evaluate european_put path);
  Std.printf "value of the forward (None) = %f\n" (evaluate forward1 path);
  Std.printf "value of the forward (Some) = %f\n" (evaluate forward2 path);
  Std.printf "value of the barrier1 = %f\n" (evaluate barrier path);
  Std.printf "value of the barrier2 = %f\n" (evaluate barrier2 path);;


print_endline "\nSecond part: test Logic module"
let () =
  let p = Binomial_tree.make (BINOMIAL_TREE {start_date=time1;end_date = time3;depth = 12}) in
  match p with
  | None -> failwith "You failed miserably"
  | Some p ->
    Binomial_tree.string_of_t p |> print_endline;;



print_endline"\nTest Motion module";;
let () =
  let bparam = Motion.Bachelier.make_parameter (P_BACHELIER {mean=0.01;st_dev=10.0}) in
  let bstate = Motion.Bachelier.make_state (S_BACHELIER 100.0) in
  let () =
      match bparam,bstate with
      | None,_ |_,None -> print_endline "You failed miserably"
      | Some bparam, Some bstate ->
        let ds = Motion.Bachelier.ds bparam bstate 0.01 0.01 in
        let cprice_fun = Motion.Bachelier.get_closed_form call in
        let ecprice_fun = Motion.Bachelier.get_closed_form call_european in
        let cput_fun = Motion.Bachelier.get_closed_form put in
        let ecput_fun = Motion.Bachelier.get_closed_form european_put in
        let f1_fun = Motion.Bachelier.get_closed_form forward1 in
        let f2_fun = Motion.Bachelier.get_closed_form forward2 in
        let b1_fun = Motion.Bachelier.get_closed_form barrier in
        let b2_fun = Motion.Bachelier.get_closed_form barrier2 in
        match cprice_fun, cput_fun, b1_fun, b2_fun with
        | Some _,_,_,_|_,Some _,_,_|_,_,Some _,_|_,_,_,Some _ -> failwith "Stupid!"
        | None, None, None, None ->
          print_endline "Ok with no closed form";
          match ecprice_fun, ecput_fun, f1_fun, f2_fun with
          | None,_,_,_ |_,None,_,_|_,_,None,_|_,_,_,None -> failwith "You failed miserably"
          | Some cp_fun,Some ep_fun,Some ff1,Some ff2 ->
            let cprice = cp_fun bparam bstate time1 in
            let eprice = ep_fun bparam bstate time1 in
            let f1price = ff1 bparam bstate time1 in
            let f2price = ff2 bparam bstate time1 in
            Std.printf "ds = %f\n" ds;
            Std.printf "call price = %f\n" cprice;
            Std.printf "put price = %f\n" eprice;
            Std.printf "forward (None) price = %f\n" f1price;
            Std.printf "forward (Some) price = %f\n" f2price in
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


