open Core;;

type t =
  | SINGLE_VALUE of (Date.t -> float -> (Date.t * float) list -> float)
  | BARRIER of
      (Date.t * float -> bool) * (* barrier condition *)
      t * (* payoff if no barrier break *)
      t (* payoff if breaks barrier *)
  | ASIAN of (Date.t list -> float -> (Date.t * float) -> float)



let call_payoff expiry_date strike path =
  
  
let make_call expiry_date strike =
  SINGLE_VALUE (
  
  
