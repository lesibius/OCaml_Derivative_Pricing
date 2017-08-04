open Core;;


(******************************************************)
(*                    INTERFACES                      *)
(******************************************************)

module type Motion_intf = sig

  type parameter
  type state

  val ds: parameter -> state -> float -> float -> float

  (* THESE ONES SHOULD NOT BE USED!*)
  val parameter_of_list: float list -> parameter option
  val state_of_list: float list -> state option

end



(******************************************************)
(*                      BACHELIER                     *)
(******************************************************)

module Bachelier : Motion_intf = struct

  type parameter =
    {
      mean:float;
      st_dev:float;
    }

  type state = float

  let parameter_of_list = function
    | [] -> None
    | h :: [] -> None
    | mu :: sigma :: t -> Some {mean=mu;st_dev=sigma}
        
          
  let state_of_list = function
    | [] -> None
    | h :: t -> Some h

  let ds param state dt dw =
    match param with
    |{mean=mu;st_dev=sigma} ->
      state *. mu *. dt +. sigma *. dw *. (sqrt dt)
    

end
