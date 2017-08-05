open Core;;

let pi = 4.0 *. atan 1.0

let erf ?depth:(d=1000) z =
  let z_square = z ** 2.0 in
  let prod_init = -. (z ** 2.0) in
  let init = z -. (z ** 3.0) /. 3.0  in
  let rec aux acc acc_p n =
    if n > d then
      acc
    else
      let new_prod = acc_p *. ((-. z_square) /. (float_of_int n +. 1.0)) in
      let new_val = acc +. (z /. (2.0 *. float_of_int n +. 1.0)) *. new_prod in
      aux new_val new_prod (n + 1) in
  (2.0 /. sqrt pi) *. (aux init prod_init 2)
      
