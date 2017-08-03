open Core;;

val generate_path: ?mean:float -> ?st_dev:float -> int -> float list

val black_scholes_process: float -> float -> float -> int -> float -> float list


