open Core;;

type t

val t_of_time_value_list: (Time.t * float) list -> t

val get_val_by_date: t -> Time.t -> float

val is_breached_up: t -> float -> bool

val is_breached_down: t -> float -> bool

val string_of_t: t -> string
