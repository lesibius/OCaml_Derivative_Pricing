open Core;;
open Math_util;;


type bachelier_parameter =
  {
    mean:float;
    st_dev:float;
  }

type black_scholes_parameter =
  {
    mean:float;
    st_dev:float;
  }



type motion_parameter =
  | P_BACHELIER of bachelier_parameter
  | P_BLACK_SCHOLES of black_scholes_parameter


type motion_state =
  | S_BACHELIER of float
  | S_BLACK_SCHOLES of float



(******************************************************)
(*                    INTERFACES                      *)
(******************************************************)

module type Motion_intf = sig

  type parameter
  type state

  val ds: parameter -> state -> float -> float -> float

  val make_parameter: motion_parameter -> parameter option
  val make_state: motion_state -> state option

  val get_closed_form: Payoff_function.t -> (parameter -> state -> Time.t -> float) option

end



(******************************************************)
(*                      BACHELIER                     *)
(******************************************************)

module Bachelier : Motion_intf = struct

  type parameter = bachelier_parameter
  type state = float

  let make_parameter = function
    | P_BACHELIER x -> Some (x:parameter)
    | _ -> None
          
  let make_state = function
    | S_BACHELIER x -> Some (x:state)
    | _ -> None

  let ds (param:parameter) state dt dw =
    match param with
    |{mean=mu;st_dev=sigma} ->
      state *. mu *. dt +. sigma *. dw *. (sqrt dt)

  let d0 s0 sigma_bar strike rf deltat =
    ((s0 *. exp (rf *. deltat) -. strike)) /. (s0 *. sigma_bar *. exp (rf *. deltat))

<<<<<<< HEAD
  let n0 x = (1.0 +. erf (x /. sqrt 2.0)) /. 2.0
  
=======
  (*  external bachelier_call: float -> float -> float -> float -> float -> int = "caml_bachelier_call"*)
    
>>>>>>> math
  let call_price expiry strike param (state:state) date0 =
    match param with
    | ({mean=rf;st_dev=sigma}:parameter) ->
      let t = Time.diff expiry date0 |> Span.to_day in 
      match state with
<<<<<<< HEAD
      | s0 ->
        let time_span = (Time.diff expiry date0 |> Span.to_day) /. 365.0 in
        let sigma_bar = sigma *. sqrt (time_span) /. s0 in
        let dd0 = d0 s0 sigma_bar strike mu time_span in
        let nn0 = dd0 |> n0 in
        print_endline ("d0 = " ^ string_of_float dd0);
        print_endline ("n0[d0] = " ^ string_of_float nn0);
        print_endline ("sigma_bar = " ^ string_of_float sigma_bar);
        let a = (s0 *. exp (time_span *. mu) -. strike) *. nn0 in
        let b = s0 *. exp (time_span *. mu) *. sigma_bar /. (sqrt (2.0 *. pi)) *. exp ((-. dd0 ** 2.0) /. 2.0) in
        (a +. b) *. exp (-. time_span *. mu)
        
=======
      | (s:float) -> s(*bachelier_call rf t s sigma strike*)
>>>>>>> math
  

    
  let get_closed_form payoff =
    let open Payoff_function in
    match payoff with
    | EUROPEAN_CALL (expiry,strike,_) -> Some (call_price expiry strike)
    | _ -> None


end
