(* A *)
(** [increment list] is the given [list] of ints with each element increased by 1. *)
let increment list = 
  List.map (fun x -> x + 1) list

(** [lengths list] is a list of the lengths of each list in [list]. *)
let lengths list =
  List.map (fun x -> List.length x) list
 
(** [times_lists x list] is a list of each element of [list] multiplied by [num]. *)
let times_list num list =
  List.map (fun x -> x * num) list
 
(** [product list] is the product of all elements in [list]. *)
let product list =
  List.fold_left (fun x y -> x * y) 1 list 

(** [print_it list] prints each element of [list] to a new line. *)
let print_it list = 
  List.map (fun x -> print_endline (x ^ " it")) list

(** [intercalate sep list] separates each element of [list] with [sep].
    Ignores empty strings.  *)
let intercalate sep list = 
  match (List.filter (fun x -> if x = "" then false else true) list) with
  | [] -> ""
  | h :: t -> List.fold_left (fun x y -> x ^ sep ^ y) h t

(** [nonempty list] is all nonempty lists in [list] in the same order. *)
let nonempty list = 
  List.filter (fun x -> if x = [] then false else true) list

(** [odds list] is a list of all odd numbers in [list]. *)
let rec odds list = 
  List.filter (fun x -> if x mod 2 == 0 then false else true) list

(** [skip_skips list] is all strings in [list] excluding any strings
    that equal "skip". *)
let rec skip_skips list = 
  List.filter (fun x -> if x = "skip" then false else true) list

(* B *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(* C *)
let rec flip tree  = 
  match tree with 
  | Leaf -> Leaf
  | Node (element, left, right) -> Node (element, flip right, flip left)

(* D.1 *)
let rec rightmost tree = 
  match tree with
  | Leaf -> None 
  | Node (element, left, right) -> 
    if right == Leaf then Some element
    else rightmost right

(* D.2 *)
let rec exists_tree f tree = 
  match tree with 
  | Leaf -> false
  | Node (element, left, right) ->
    if f element == true then true
    else exists_tree f left || exists_tree f right

(* E *)
let rec fold_tree init f tree = 
  match tree with 
  | Leaf -> init
  | Node (element, left, right) -> f element (fold_tree init f left) (fold_tree init f right)

(* F *)
let rec map_tree f tree =
  match tree with
  | Leaf -> Leaf
  | Node (element, left, right) -> Node (f element, (map_tree f left), (map_tree f right))

(* G *)
let for_all_tree f tree = 
  match tree with
  | Leaf -> false
  | Node (element, left, right) -> fold_tree true (fun _ left right -> (f left) && (f right)) tree

let test_for_all_tree = run_fold (fun x -> x > 0) (Node (100, Leaf, Leaf))



(* B to G Tests*)
(* let test_flip = flip (Node (3, Node (2, Node (1, Leaf, Leaf), Leaf), Node (4, Leaf, Leaf)))
let test_flip2 = flip Leaf
let test_flip3 = flip (Node ("hi", Leaf, Leaf))

let test_rightmost = rightmost (Node ("hello", Leaf, Node ("world", Leaf, Leaf)))
let test_rightmost2 = rightmost (Node (3, Leaf, Leaf))
let test_rightmost3 = rightmost Leaf

let test_exists_tree = exists_tree (fun x -> x > 0) (Node (3, Leaf, Leaf))
let test_exists_tree2 = exists_tree (fun x -> x mod 2 = 0) Leaf 
let test_exists_tree3 = exists_tree (function Some _ -> true | None -> false) (Node (None, Node (None, Leaf, Leaf), Leaf))

let test_fold_tree = fold_tree 0 (fun x left right -> x + left + right) (Node (3, Node (2, Node (1, Leaf, Leaf), Leaf), Node (4, Leaf, Leaf)))
let test_fold_tree2 = fold_tree 1 (fun _ left right -> left + right) (Node (3, Node (2, Node (1, Leaf, Leaf), Leaf), Node (4, Leaf, Leaf))) 

let test_map_tree = 2*)


(* A Tests *)
(* let test_increment = increment [1;2;3;4]
let test_lengths = lengths [[1;3;4];[2;3;];[1]]
let test_timeslist = times_list 2 [3;2;3;4]
let test_prod = product [4;2;1;8]
let test_print_it = print_it ["work"; "make"; "do"]
let test_intercalate = intercalate ", " ["";"hello";"";"";"world";"!"]
let test_nonempty = nonempty [[];[1;2;3]]
let test_odds = odds [3;4;5;6;6;5;4;3;2] 
let test_skipskips = skip_skips ["hello";"skip";"world";"!";"don't skip this!"] *)