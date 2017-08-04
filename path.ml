open Core;;

type t = (Time.t * float) list


let t_of_time_value_list date_value_list = date_value_list

let get_val_by_date path date =
  match path |> List.rev with
  | [] -> failwith "Should not happen"
  | (d,v) :: t -> v

let is_breached_up path value =
  let rec aux = function
    | [] -> false
    | h :: t ->
      match h with
      | (d,v) -> if v >= value then true else aux t in
  aux path

let is_breached_down path value =
  false



let string_of_t path =
  let rec aux acc = function
    | [] -> acc
    | h :: t ->
      match h with
      | (d,v) -> aux (acc ^ " " ^ string_of_float v) t in
  aux "" path 
