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

  external bachelier_call: float -> float -> float -> float -> float -> float = "caml_bachelier_call"
  external bachelier_put: float -> float -> float -> float -> float -> float = "caml_bachelier_put"
    external bachelier_forward: float -> float -> float -> float = "caml_bachelier_forward"
    

  let vanilla_price expiry strike f param (state:state) date0=
    match param with
    | ({mean=rf;st_dev=sigma}:parameter) ->
      let t = (Time.diff expiry date0 |> Span.to_day) /. 360.0 in 
      match state with
            | (s:float) -> f rf s sigma t strike
    
  let get_closed_form payoff =
    let open Payoff_function in
    match payoff with
    | EUROPEAN_CALL (expiry,strike,_) -> Some (vanilla_price expiry strike bachelier_call)
    | EUROPEAN_PUT (expiry,strike,_) -> Some (vanilla_price expiry strike bachelier_put)
    | FORWARD (expiry,_,_) ->
      Some (fun param state date0 ->
          match param with
          |({mean=rf;st_dev=sigma}:parameter) ->
            let t = (Time.diff expiry date0 |> Span.to_day) /. 360.0 in
            match state with
            | (s:float) -> bachelier_forward rf s t)
    | _ -> None


end
