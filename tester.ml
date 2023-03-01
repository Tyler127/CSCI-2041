(** This signature was adapted from the textbook
    https://cs3110.github.io/textbook/chapters/modules/functional_data_structures.html#maps *)
module type STRING_MAP = sig
  (** ['a t] is the type of string-indexed maps
      with value type ['a]. *)
  type 'a t

  (** [empty] is an empty map. *)
  val empty  : 'a t

  (** [insert k v m] is the map [m] augmented with
      the mapping from [k] to [v]. *)
  val insert : string -> 'a -> 'a t -> 'a t

  (** [lookup k m] returns the value [k] maps to in [m].

      @raise Not_found if [k] is not bound in [m]. *)
  val lookup : string -> 'a t -> 'a
end

(** The type of recipes to build a map. *)
type 'a build =
  | Empty
  (** [Empty] means calling [empty] to get a map. *)
  | Insert of string * 'a * 'a build
  (** [Insert (k, v, b)] means building a map [m]
      from the recipe [b] and then calling
      [insert k v m] to get a map. *)

(** The type of checks on a map. *)
type 'a check =
  | Lookup of string * ('a -> bool) option
  (** [Lookup (k, None)] means calling [lookup]
      with the key [k] on the map should fail.

      [Lookup (k, Some p)] means calling [lookup]
      with the key [k] on the map should succeed,
      and the obtained value [v] should satisfy [p].
      That is, [p v] should be [true]. *)

module StringMapTester (StringMap : STRING_MAP) :
sig
  (** [build b] follows the recipe [b] and
      build the map. *)
  val build : 'a build -> 'a StringMap.t

  (** [test m c] tests whether the map [m]
      satisfies the check [c]. *)
  val test : 'a StringMap.t -> 'a check -> bool
end
=
struct
  (* 👉 Complete this module. *)
  (* let rec build_helper map build = 
    match build with
    | Empty -> map
    | Insert (k, v, b) -> build_helper (StringMap.insert k v map) b 

  let build build = 
    match build with 
    | Empty -> StringMap.empty
    | Insert (k, v, b) -> build_helper (StringMap.insert k v StringMap.empty) b *)

  let rec build input_b =
    match input_b with
    | Empty -> StringMap.empty
    | Insert (k, v, b) -> StringMap.insert k v (build b)


  let rec test map check = 
    match check with 
    | Lookup (key, p) -> 
      match p with
      | None -> false
      | Some c -> c (StringMap.lookup key map)
    
end


module MyStringMap1 : STRING_MAP = struct
  (* implementation of STRING_MAP *)
  type 'a t = (string * 'a) list 

  let empty = []

  let insert key value map = (key, value) :: map

  let rec lookup key = function
    | [] -> raise Not_found
    | (k, v) :: t -> 
        if k == key then v 
        else lookup key t

end

module MyStringMap1Tester = StringMapTester (MyStringMap1)
let () = assert MyStringMap1Tester.(test (build (Insert("hi", 1, Empty))) (Lookup ("hi", Some (fun x -> x = 1))))
let () = assert MyStringMap1Tester.(test (build Empty) (Lookup ("hi", None)))


(* After finishing the above functor, you might wonder how to
   test a tester. You should implement a few modules of the
   signature [STRING_MAP] (the textbook has one) and check
   whether you can use your functor to test them. The code
   could be:

   {[
     module MyStringMap1 : STRING_MAP =
     struct
       (* implementation of STRING_MAP *)
     end

     module MyStringMap1Tester = StringMapTester (MyStringMap1)

     let () = assert MyStringMap1Tester.(test (build Empty) (Lookup ("hi", None)))
   ]}

   The [assert expr] is a convenient built-in syntax that
   raises an exception when [expr] is not true. Ask a TA
   if you are confused.
*)
