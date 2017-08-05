open Core;;

type binomial_tree = {
  start_date:Time.t;
  end_date:Time.t;
  depth:int}
type basic_monte_carlo = {
  start_date:Time.t;
  end_date:Time.t;
  n_div:int
}
type closed_form = |CLOSED_FORM

type pricing_logic_parameter =
  | BINOMIAL_TREE of binomial_tree
  | BASIC_MONTE_CARLO of basic_monte_carlo
  | CLOSED_FORM of closed_form



module type Pricing_logic_intf = sig

  type t

  val make: pricing_logic_parameter -> t option

  val string_of_t: t -> string

  (*  val paths_of_logic: t -> Path.t list *)

end


module Binomial_tree : Pricing_logic_intf = struct

  (* TO DELETE WHEN TEST ENDS!*)
  let time1 = Time.of_filename_string "2017-08-04_13-19-00.000";;
  let time2 = Time.of_filename_string "2017-08-05_13-19-00.000";;
  let time3 = Time.of_filename_string "2017-08-06_13-19-00.000";;

  type t = binomial_tree

  let make param =
    match param with
    | BINOMIAL_TREE param -> Some param
    | _ -> None

  let string_of_t (param:t) =
    match param with
    | {start_date=s;end_date=e;depth=d} ->
      let s_format = Time.format s "%d/%m/%y" in
      let e_format = Time.format e "%d/%m/%y" in
      Std.sprintf "Binomial model:\n -start date: %s\n -end date: %s\n -depth: %i" s_format e_format d
      

  (* This type is not accessible outside the module *)
  type tree

  (*
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
*)
        
      

end
