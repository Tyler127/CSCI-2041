
(* 6 *)
(* (let rec foo x y = x in 
(let rec foo x y = y in 
(let rec y = foo 1 2 in foo (y+2) (y+4)))) *)


(* 8 *)
(* (let x = 5 in (let foo x y = x in foo 3 6 + x))  *)

(* 3 correct *)
(* (let x = 5 in (let foo x y = x in foo 3 6)) *)

(* maximum: 26/30 *)
let score = 26.0 /. 30.0

let mostlikelyscore = 22.0 /. 30.0

let x y = [Some (Some 3); Some (None); Some(Some y)]