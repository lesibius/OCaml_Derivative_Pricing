open Core;;


module type Model_int = sig

  type t
  val make_test: unit -> t
  val paths_of_model: t -> Path.t list

end


module Binomial_tree : Model_int = struct

  type t =
    | NODE of (Time.t * float) * t * t
    | EMPTY

  let make_test () =
    let up = NODE ((Time.now (),120.0),EMPTY,EMPTY) in
    let down = NODE ((Time.now (),90.0),EMPTY,EMPTY) in
    NODE ((Time.now (),100.0),up,down)

  let paths_of_model model =
    let rec aux current_list list_of_list = function
      | NODE ((date,value) as data,EMPTY,EMPTY) ->
        (List.rev (data :: current_list)) :: list_of_list
      | NODE ((d,v),(NODE (_,_,_) as n1),(NODE (_,_,_) as n2)) ->
        let l_of_l = aux ((d,v) :: current_list) list_of_list n1 in
        aux ((d,v) :: current_list) l_of_l n2
      | _ -> failwith "Should not happen" in
    let dv_list_list = aux [] [] model in
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux ((h |> Path.t_of_time_value_list) :: acc) t in
    aux [] dv_list_list
        
      

end
