open Core;;
open Motion;;
open Pricing_logic;;


module type Model_intf = sig
  include Motion_intf
  include Pricing_logic_intf
  val evaluate: parameter -> state -> t -> float
end

module Make_model(Motion:Motion_intf) (Logic:Pricing_logic_intf):Model_intf = struct

  include Motion
  include Logic
      
  
  let evaluate parameter state logic = 42.0
  
end
