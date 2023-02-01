(* In-Lab Tasks *)
(** [increment list] is the given [list] of ints with each element increased by 1. *)
let rec increment list = 
  match list with
  | [] -> []
  | h :: t -> h + 1 :: increment t

(** [lengths list] is a list of the lengths of each list in [list]. *)
let rec lengths list =
  match list with
  | [] -> []
  | h :: t -> List.length h :: lengths t
 
(** [times_lists x list] is a list of each element of [list] multiplied by [x]. *)
let rec times_lists x list =
  match list with
  | [] -> []
  | h :: t -> h * x :: times_lists x t

(** [product list] is the product of all elements in [list]. *)
let rec product list =
  match list with
  | [] -> 1
  | h :: t -> h * product t

(** [print_it list] prints each element of [list] to a new line. *)
let rec print_it list = 
  match list with
  | [] -> ()
  | h :: t -> print_endline (h ^ " it"); print_it t

(** [intercalate sep list] separates each element of [list] with [sep].
    Ignores empty strings.  *)
let rec intercalate sep list = 
  match list with
  | [] -> ""
  | h :: t -> 
    match h with 
    | "" -> intercalate sep t
    | _ -> 
      match t with
      | [] -> h
      | _ -> h ^ sep ^ intercalate sep t

(** [nonempty list] is all nonempty lists in [list] in the same order. *)
let rec nonempty list =
  match list with 
  | [] -> []
  | h :: t ->
    match h with
    | [] -> nonempty t
    | _ -> h :: nonempty t

(** [odds list] is a list of all odd numbers in [list]. *)
let rec odds list =
  match list with
  | [] -> []
  | h :: t ->
    match h mod 2 with
    | 0 -> odds t
    | _ -> h :: odds t

(** [skip_skips list] is all strings in [list] excluding any strings
    that equal "skip". *)
let rec skip_skips list = 
  match list with
  | [] -> []
  | h :: t ->
    match h with
    | "skip" -> skip_skips t
    | _ -> h :: skip_skips t

(* Individual Tasks: *)
(** [get_prefix_eq_int num list] is a list that contains only [num] 
    for the amount of times it occurs consecutively at the beginning of [list]. *)
let rec get_prefix_eq_int num list =
  match list with 
    | [] -> []
    | h :: t ->
      match (h == num) with 
      | true -> h :: get_prefix_eq_int num t
      | false -> []


(** [drop_prefix_eq_int num list] is a list that contains all remaining elements
    after removing all consecutive occurances of [num] from the beginning of [list]. *)
let rec drop_prefix_eq_int num list = 
  match list with
  | [] -> []
  | h :: t ->
    match (h == num) with
    | true -> drop_prefix_eq_int num t
    | false -> h :: t

(** [make_list_x_ntimes x n] creates a list of [n] elements of [x]. *)
let rec make_list_x_ntimes x n =
  if (n >= 1) then
    x :: make_list_x_ntimes x (n-1)
  else
    []

(** [group_eq_helper acc holder list] counts the amount of consecutive occurances of equal elements
  using [acc] then when a differing element occurs, creates a list of [acc] elements of [holder]. 
  Recursively calls until [list] is empty. *)
let rec group_eq_helper acc holder list =
  match list with
  | [] -> []
  | h :: t -> 
    if t = [] then 
      if h = holder then [make_list_x_ntimes holder (acc+1)]
      else make_list_x_ntimes holder acc :: [make_list_x_ntimes h 1]
    else
      if h = holder then group_eq_helper (acc + 1) holder t
      else make_list_x_ntimes holder acc :: group_eq_helper 1 h t

(** [group_eq list] creates a list in which equal consecutive items of [list] 
    are group togethered in sublists. *)
let rec group_eq list = 
  match list with
  | [] -> []
  | h :: t -> group_eq_helper 1 h t  

(* Tests: *)
(* let k = get_prefix_eq_int 1 [1;1;3;4;1]
let l = drop_prefix_eq_int 1 [1;1;3;4;1]
let test_list = make_list_x_ntimes 5 3
let test_list2 = make_list_x_ntimes 10 2
let m1 = group_eq [1;1;3;4;4;5;4;1] 
let m2 = group_eq [1;2;3;3;3;3;8;9;9;9] 
let m3 = group_eq ["baby"; "baby"; "shark"; "doo";"doo";"doo";"doo";"sus"]
let m4 = group_eq ["baby"; "doo";"shark"; "doo";"doo";"doo";"doo"]  *)