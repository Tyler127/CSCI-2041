
module type RING = sig
  type t
  (** The unit element for + *)
  val zero : t
  (** The unit element for * *)
  val one : t
  (** One of the two associative operations *)
  val ( + ) : t -> t -> t
  (** The other associative operation *)
  val ( * ) : t -> t -> t
  (** The additive inverse, that is: for any number a, this will give a number b s.t. a + b = 0.
      In other words: a + ( ~- a ) = 0 for all a.
      Sometimes called unary minus *)
  val ( ~- ) : t -> t
  (** A function to greatly improve testability *)
  val to_string : t -> string
end

module type RING_PLUS = sig
  include RING
(* 👉 Document these yourself (the implementation is given in case you want to know what they do) *)
  val from_int : int -> t
  val ( - ) : t -> t -> t (* binary minus *)
end

(* You may need to expose the type of t in IntRing,
   depending on how you'd like to solve the last part of this exercise. *)
module IntRing : RING with type t = int = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let to_string = string_of_int
end

module FloatRing : RING = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = Stdlib.( +. )
  let ( * ) = Stdlib.( *. )
  let ( ~- ) = Stdlib.( ~-. )
  let to_string = string_of_float
end

(* Suggested implementations for the from_int and - functions:
  let rec from_int ( i : int ) : t
    = if i < 0 then (- (from_int ((Stdlib.( ~- ) i) )))
      else if i=0 then zero else one + from_int (i - 1))
  let ( - ) (x : t) (y : t) : t
    = x + (~- y)

    Note: you can implement more efficient from_int functions if you'd like!
    *)

(* 👉 Create a functor module called RingPlus that extends a ring
      with the from_int and binary minus functions. *)

module RingPlus (R : RING_PLUS) = structrovided
File "rings.ml", line 24, characters 2-25: Expected declaration
The value `from_int' is required but not provided
  include R

  let rec from_int (i : int) : t =
    if i < 0 then (- (from_int ((Stdlib.( ~- ) i))))
    else if i = 0 then zero else ( + ) (one) (from_int (Stdlib.( - ) i 1))
      
  let ( - ) (x : t) (y : t) = x + (~- y)
end

module FloatRingPlus = RingPlus(FloatRing)
module IntRingPlus = RingPlus(IntRing)

(* 👉 The following code should work after you've implemented the RingPlus functor *)

(*
module FloatRingPlus = RingPlus(FloatRing)
module IntRingPlus = RingPlus(IntRing)
*)


(* Three things to try in utop: 

FloatRingPlus.(to_string (from_int 5 - from_int 3));;
IntRingPlus.(to_string (from_int 5 - from_int 3));;

Testing the efficiency of your implementations (will initially give a stack overflow):
IntRingPlus.(to_string (from_int 500000 - from_int 499999));;

*)

(* 👉 Override the IntRingPlus.from_int with a more efficient version,
      so that all three of the above tests work (quickly).
      (There are multiple ways to do this)
      *)
