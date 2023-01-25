(* Warmup Functions *)
let add x y = x + y

let six = add 3 3

let add3 x = x + 3

let is_dividing x y = y mod x = 0

let rec check_factors x d = 
  if d = 1 then false
  else 
    if is_dividing d x then true
    else check_factors x (d - 1)

let is_prime x = 
  if check_factors x (x - 1) then false
  else true


(* Main Functions *)
let rec power_pos x y = 
  if y >= 0 then
    if y = 0 then 1.
    else power_pos x (y - 1) *. x
  else 0.

let rec power x y = 
  if y >= 0 then
    if y = 0 then 1.
    else power x (y - 1) *. x
  else 1. /. power x (-1 * y)

let rec count_factors_helper x y = 
  if y = 0 then 0
  else 
    if is_dividing y x then (count_factors_helper x (y - 1)) + 1
    else count_factors_helper x (y - 1)

let count_factors x = let y = x in count_factors_helper x y 

let test = count_factors 3