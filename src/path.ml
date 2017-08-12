open Core;;

type t = (Time.t * float) list


let t_of_time_value_list date_value_list = date_value_list

let get_val_by_date path date =
  (* Current implementation assumes list ordered by time *)
  let rec aux current_val current_diff = function
    | [] ->
      let ret_val =
        match current_val with
        | None -> None
        | Some v -> Some v in
      ret_val
    | (d,v) :: t ->
      let new_diff = (Time.abs_diff date d) in
      if new_diff < current_diff then
        aux (Some v) new_diff t 
      else
        current_val in
  let option_v = aux None (Span.of_day 365.0) path in
  match option_v with
  | None -> failwith "Not found!!"
  | Some v -> v


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
