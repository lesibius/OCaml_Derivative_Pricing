open Core;;

type t =
  | BASIC of (Path.t -> float)
  | CONSTANT of (unit -> float)
  | BARRIER of (Path.t -> bool) * t * t
  | EUROPEAN_PUT of Time.t * float * t
  | EUROPEAN_CALL of Time.t * float * t
  | FORWARD of Time.t * float option * t


val evaluate: t -> Path.t -> float

val make_forward: Time.t -> float option -> t
val make_call: Time.t -> float -> t
val make_put: Time.t -> float -> t
val make_european_call: Time.t -> float -> t
val make_european_put: Time.t -> float -> t
val make_knock_out_call: float -> Time.t -> float -> t
