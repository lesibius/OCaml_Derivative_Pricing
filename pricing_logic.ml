open Core;;

module type Pricing_logic_intf = sig

  type t
  val make_test: unit -> t
  val paths_of_logic: t -> Path.t list

end


module Binomial_tree : Pricing_logic_intf = struct
  
  

  (* TO DELETE WHEN TEST ENDS!*)
  let time1 = Time.of_filename_string "2017-08-04_13-19-00.000";;
  let time2 = Time.of_filename_string "2017-08-05_13-19-00.000";;
  let time3 = Time.of_filename_string "2017-08-06_13-19-00.000";;

  type t =
    | NODE of (Time.t * float) * t * t
    | EMPTY


  let make_test () =
    let up = NODE ((time2,120.0),EMPTY,EMPTY) in
    let down = NODE ((time2,90.0),EMPTY,EMPTY) in
    NODE ((time1,100.0),up,down)

  let paths_of_logic logic =
    let rec aux current_list list_of_list = function
      | NODE ((date,value) as data,EMPTY,EMPTY) ->
        (List.rev (data :: current_list)) :: list_of_list
      | NODE ((d,v),(NODE (_,_,_) as n1),(NODE (_,_,_) as n2)) ->
        let l_of_l = aux ((d,v) :: current_list) list_of_list n1 in
        aux ((d,v) :: current_list) l_of_l n2
      | _ -> failwith "Should not happen" in
    let dv_list_list = aux [] [] logic in
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux ((h |> Path.t_of_time_value_list) :: acc) t in
    aux [] dv_list_list
        
      

end
