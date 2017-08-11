open Core;;
open Ctypes;;
open Foreign;;

let pi = 4.0 *. atan 1.0

let erf =
  foreign "erf"
    (double @-> returning double)

