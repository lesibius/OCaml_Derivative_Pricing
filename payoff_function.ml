open Core;;

(*NB: barrier = test, unbreached payoff, breached payoff*)
type t =
  | BASIC of (Path.t -> float)
  | CONSTANT of (unit -> float)
  | BARRIER of (Path.t -> bool) * t * t
  | EUROPEAN_PUT of Time.t * float * t
  | EUROPEAN_CALL of Time.t * float * t
  | FORWARD of Time.t * float * t  

let rec evaluate t path =
  match t with
  | BASIC f -> f path
  | BARRIER (f , f_unbreached , f_breached ) ->
    if f path then
      evaluate f_breached path
    else
      evaluate f_unbreached path
  | CONSTANT f -> f ()
  (* Thereafter: payoffs of derivatives known for having closed form solution - this is used in pattern matching*)
  | EUROPEAN_CALL (_,_,f) -> evaluate f path
  | EUROPEAN_PUT (_,_,f) -> evaluate f path
  | FORWARD (_,_,f) -> evaluate f path


let call_payoff strike = function
  | expiry_value when expiry_value <= strike -> 0.0
  | expiry_value -> expiry_value -. strike
  
let make_call expiry_date strike =
  BASIC
    (fun path ->
    expiry_date |>
    Path.get_val_by_date path
    |> call_payoff strike)


let make_european_call expiry_date strike =
  EUROPEAN_CALL (expiry_date, strike,
    (BASIC
    (fun path ->
    expiry_date |>
    Path.get_val_by_date path
    |> call_payoff strike)))

let knocked_out () =
  0.0

let make_knock_out_call barrier expiry_date strike =
  BARRIER
    ((fun path -> Path.is_breached_up path barrier),
     make_call expiry_date strike,
     CONSTANT knocked_out)


    
