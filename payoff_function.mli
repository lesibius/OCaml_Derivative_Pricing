open Core;;

type t

val evaluate: t -> Path.t -> float
  
val make_call: Time.t -> float -> t
val make_knock_out_call: float -> Time.t -> float -> t
