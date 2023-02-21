module type TreeSignature =
sig
  (** The type of binary trees. *)
  type 'a t = Leaf | Node of 'a * 'a t * 'a t

  (** [empty] is [Leaf]. *)
  val empty : 'a t

  (** [singleton] is a tree with one node containing 'a as the element. *)
  val singleton : 'a -> 'a t

  (** [size t] returns the size of [t] (leaves don't count for this lab). *)
  val size : 'a t -> int

  (** [leftmost] returns element in the farthest left node in [t]. *)
  val leftmost : 'a t -> 'a option
 
  val reverse : 'a t -> 'a t

  (** [map] is the given function applied to each element of [t]. *)
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(* 👉 Define a module [Tree] of type [TreeSignature]

   Put the top-level comments about the interface (public-facing comments)
   in the module type (signature), not in the module (structure). *)

module Tree : TreeSignature = 
struct
  type 'a t = Leaf | Node of 'a * 'a t * 'a t

  let empty = Leaf

  let singleton e = Node (e, Leaf, Leaf)

  let rec size = function
    | Leaf -> 0
    | Node (e, l, r) -> 1 + size l + size r

  let rec leftmost = function
    | Leaf -> None
    | Node (e, l, r) -> 
      if l = Leaf then Some e 
      else leftmost l

  let rec rightmost = function
    | Leaf -> None
    | Node (e, l, r) ->
      if r = Leaf then Some e 
      else rightmost r

  let rec reverse = function
    | Leaf -> Leaf
    | Node (e, l, r) -> Node (e, reverse r, reverse l)

  let rec map f = function
      | Leaf -> Leaf
      | Node (e, l, r) -> Node (f e, (map f l), (map f r))   
end

(* let t = Tree.Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Node (90, Leaf, Leaf)))
let test_size = Tree.size t

let test_singleton = Tree.singleton 8
let test_rightmost = Tree.rightmost t
let test_leftmost = Tree.leftmost t
let test_reverse = Tree.reverse t *)



module type ListSignature =
sig
  (** 👉 Document it *)
  type 'a t = 'a list

  (** 👉 Document it *)
  val empty : 'a t

  (** 👉 Document it *)
  val singleton : 'a -> 'a t

  (** 👉 Document it *)
  val size : 'a t -> int

  (** 👉 Document it *)
  val leftmost : 'a t -> 'a option

  (** 👉 Document it *)
  val rightmost : 'a t -> 'a option

  (** 👉 Document it *)
  val reverse : 'a t -> 'a t

  (** 👉 Document it *)
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(* 👉 Define a module [MyList] of type [ListSignature]

   You should use functions in [List] only if you know how to define them.
   [reverse] is challenging,
   but I wrote a version of it using fold_left during one of the lectures.

   Again, put the public-facing top-level comments in the module type (signature),
   not in the module (structure). *)

module MyList : ListSignature = 
struct
  type 'a t = 'a list

  let empty = []

  let singleton e = [e]

  let rec size = function
    | [] -> 0
    | h :: t -> 1 + size t

  let rec leftmost = function
    | [] -> None
    | h :: t -> Some h

  let rec rightmost = function
    | [] -> None
    | h :: [] -> Some h 
    | h :: t -> rightmost t 

  let rec reverse list = 
    match list with
    | [] -> []
    | _ -> List.fold_left (fun x y -> y :: x) [] list

  let rec map f = function
    | [] -> []
    | h :: t -> f h :: map f t 
end

(* let test_reverse = MyList.reverse [1;2;3;4;5]
let test_map = MyList.map (fun x -> x * 2) [1;2;3;4;5]
let test_rightmost = MyList.rightmost [1;2;3;4;5]
let test_leftmost = MyList.leftmost [1;2;3;4;5] *)



module type Sequence =
sig
  (** The abstract type of sequences. *)
  type 'a t

  (** [empty] is an empty sequence. *)
  val empty : 'a t

  (** 👉 Document it *)
  val singleton : 'a -> 'a t

  (** 👉 Document it *)
  val size : 'a t -> int

  (** 👉 Document it *)
  val leftmost : 'a t -> 'a option

  (** 👉 Document it *)
  val rightmost : 'a t -> 'a option

  (** 👉 Document it *)
  val reverse : 'a t -> 'a t

  (** 👉 Document it *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (* 👉 Fill out the content of [Sequence] such that

     1. All value definitions ([empty], [map], etc.) are still visible.
     2. It is properly documented; that is, you should not refer to trees
        or lists because you would not even know what the underlying
        implementation is.
     3. The following two lines should type check:
     {[
       module SealedTree = (Tree : Sequence)
       module SealedList = (MyList : Sequence)
     ]} *)
end