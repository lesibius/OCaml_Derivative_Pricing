open Core;;

let verbose name end_val poff =
  Std.printf "%s: end val = %f - poff = %f" name end_val poff

let call_payoff strike path =
  let li = List.rev path in
  let last =
    match li with
    | [] -> failwith "Error"
    | x :: tl -> x in
  max (last -. strike) 0.0
