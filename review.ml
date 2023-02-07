(* LET EXPRESSIONS: *)
(* Question 1: *)
(let x = (let y = 3 in y + 1) 
in (let x = (let y = x + 1 in x + 1)
in x + 1))

(* (let x = 4 in (let x = (let y = x + 1 in x + 1)
in x + 1))

(let x = 4 in (x + 1) + 1) *)

(* Compiles *)
(* Yes, gives value. *)
(* = 6 *)


(* Question 2 *)
(* (let rec x = (let rec y = 3 in y + 1)
in (let rec x = (let rec y = x + 1 in x + 1)
in x + 1)) *)

(* Doesn't compile, because rec can't be used for ______ expressions. *)


(* Question 3 *)
(* let x = (let y = 2) 
in (let x = (let y = y + 1 in x + 1)
in x + 1) *)

(* (let x = (let y = 2) 
in (let x = x + 1)
in x + 1)

(let x = (let y = 2)
in x + 1 + 1) *)

(* Doesn't compile. There is no in for (let y = 2) *)


(* TYPES: *)
type trainstop = { platform : int; station : string; }
type 'gridpoint square = { sq_corner1 : 'gridpoint; sq_corner2 : 'gridpoint; }
type 'gridpoint shape = Square of 'gridpoint square 
                        | Point of 'gridpoint

(* type point = (int * int)
type square = { sq_corner1 : point; sq_corner2 : point }
type shape = Square of square | Point of point *)


(* Question 4 *)
let x = 1. +. 2.
(* float *)

(* Question 5 *)
(* let y = { platform = 9.75; station = "King's Cross" } *)
(* Type Error: platform is float but requires int *)

(* Question 6 *)
let z = { sq_corner1 = (3,4); sq_corner2 = (4,4) }
(* (int * int) square *)

(* Question 7 *)
let a = Point (3,5)
(* (int * int) shape *)

(* Question 8 *)
let b = [Square {sq_corner1 = (3.3, 4.4); sq_corner2 = (4.9, 4.4)}; Point (3.4, 5.3)]
(* (float * float) shape list *)


(* REDOING A LAB: *)
(* Question 9: *)
type date = (int * int * int) (* year month day *)

let is_before (date1 : date) (date2 : date) = 
  match date1 with
  | (year1, month1, day1) ->
    match date2 with 
    | (year2, month2, day2) ->
      if year1 < year2 then true
      else 
        if year1 = year2 then
          if month1 < month2 then true
          else
              if month1 = month2 then
                if day1 < day2 then true
                else false
              else false
        else false
          
let date1 : date = (2000, 2, 3)
let date2 : date = (2000, 2, 3)
let x = is_before date1 date2









