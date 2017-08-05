open Core;;


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

  (*******************************************)

  (*REPLACE THIS SHIT WHENEVER YOU CAN*)
  let pi = 4.0 *. atan 1.0
  let n0 z =
    let a = 0.78766 in
    let b = 0.2605 in
    let c = 0.6827 in
    let f = z *. a *. ((1.0 +. (z ** 2.0) *. (b ** 2.0)) ** c) in
    (exp f) /. (exp f +. exp (-. f))

  let d0 param state time_span =
    match param with
    | ({st_dev=sigma;_}:bachelier_parameter) -> sigma *. sqrt(time_span)
  
  let call_price expiry strike param (state:state) date0 =
    let open Payoff_function in
    match param with
    | ({mean=mu;st_dev=sigma}:parameter) ->
      match state with
      | s0 ->
        let time_span = (Time.diff expiry date0 |> Span.to_day) /. 365.0 in
        let dd0 = (d0 param state time_span |> n0) in
        let a = (s0 -. strike) *. dd0 in
        let b = s0 *. sigma /. sqrt (2.0 *. pi) *. exp ((-. dd0 ** 2.0) /. 2.0) in
        (a +. b) *. exp (time_span *. mu)
        
        


  
  let get_closed_form payoff =
    let open Payoff_function in
    match payoff with
    | EUROPEAN_CALL (expiry,strike,_) -> Some (call_price expiry strike)
    | _ -> None


end
