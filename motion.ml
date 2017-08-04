open Core;;


(******************************************************)
(*                    INTERFACES                      *)
(******************************************************)


module type Motion_intf = sig

  type parameter
  type state

  val ds: parameter -> state -> float -> float -> float
  val make_parameter: 'a -> parameter
  val make_state: 'a -> state

end



(******************************************************)
(*                      BACHELIER                     *)
(******************************************************)

module type Bachelier_intf = sig

  include Motion_intf
  val make_parameter: float -> float -> float -> parameter
  val make_state: float -> state
  
end


module Bachelier : Bachelier_intf = struct

  type parameter =
    {
      mean:float;
      st_dev:float;
      s0:float
    }

  type state = float

  let make_parameter s0 mu sigma = {mean=mu;st_dev=sigma;s0=s0}
  let make_state s = s

  let ds param state dt dw =
    match param with
    |{mean=mu;st_dev=sigma;s0=s0} ->
      state *. mu *. dt +. s0 *. sigma *. dw *. (sqrt dt)
    

end
