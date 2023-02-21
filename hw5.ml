module type RATIONAL =
sig
  (** The abstract type of rational numbers. *)
  type t

  (** [zero] is 0 (or: from_int 0). *)
  val zero : t

  (** [one] is 1 *)
  val one : t

  (** [equal x y] tests whether [x] is equal to [y]. *)
  val equal : t -> t -> bool

  (** 👉 Document it *)
  val less_than : t -> t -> bool
  
  (** 👉 Document it *)
  val neg : t -> t

  (** [add x y] is the addition [x + y]. *)
  val add : t -> t -> t

  (** 👉 Document it *)
  val sub : t -> t -> t

  (** 👉 Document it *)
  val mul : t -> t -> t

  (** [div x y] is the division [x / y]. 

      @raise Division_by_zero when if [y] is zero. *)
  val div : t -> t -> t

  (** Convert an abstract rational number to a pair of integers [(x, y)] such that

      1. the rational number is equal to [x/y]; and
      2. [x/y] is reduced and [y] is positive.

      For example, [(-4)/6] and [2/(-3)] should all output [(-2, 3)]. *)
  val to_pair : t -> int * int

  (** Convert a pair of integers [(x, y)] into an abstract rational number [x/y].
      @raise Division_by_zero when if [y] is 0. *)
  val from_pair : int * int -> t
end

module Rational : RATIONAL =
struct
  (** 👉 Fill out the content here. 

      You can choose whatever type [t] to implement your rational numbers. You can
      also decide when to reduce rational numbers as long as [to_pair] outputs
      correct pairs.

      For this homework, please use the built-in exception [Division_by_zero]
      instead of defining your own [Division_by_zero]. You will practice defining
      your own exceptions in the next lab/homework.

      Hint: For computing greatest common divisors, recall Euclid's algorithm:
      let rec gcd a b =
        if b = 0 then a else gcd b (a mod b);;
      Hint2: You'll need to do a bit more work to handle negative numbers. *)

  type t = (int * int)

  let zero = (0, 1)

  let one = (1, 1)

  let equal x y = (fst x * snd y) = (fst y * snd x)

  let less_than x y = (snd x * fst y) < (fst x * snd y)

  let neg x = (fst x * -1, snd x)

  let add x y = ((fst x * snd y) + (fst y * snd x), (snd x * snd y))

  let sub x y = ((fst x * snd y) - (fst y * snd x), (snd x * snd y))
   
  let mul x y = (fst x * fst y, snd x * snd y)

  let div x y = 
    if fst y = 0 || snd y = 0 || snd x = 0 then raise Division_by_zero 
    else ((fst x * snd y), (snd x * fst y))

  let rec gcd x y = if y = 0 then x else gcd y (x mod y)

  let to_pair x = let gcd' = gcd (fst x) (snd x) in (fst x / gcd', snd x / gcd')

  let from_pair pair = if snd pair = 0 then raise Division_by_zero else pair
end

let fneg11 = Rational.from_pair (-1,1)
let f11 = Rational.from_pair (1,1)
let f12 = Rational.from_pair (1,2)
let f13 = Rational.from_pair (1,3)
let f22 = Rational.from_pair (2,2)
let f24 = Rational.from_pair (2,4)
let f34 = Rational.from_pair (3,4)
let f31 = Rational.from_pair (3,1)

let test_zero = Rational.zero
let test_one = Rational.one
let test_equal = Rational.equal f12 f24
let test_equal2 = Rational.equal f12 f13
let test_less_than = Rational.less_than f12 f34
let test_less_than2 = Rational.less_than f34 f12
let test_neg = Rational.neg f11
let test_neg2 = Rational.to_pair (Rational.neg f)
let test_add = Rational.add f12 f12
let test_add2 = Rational.add f34 f34
let test_sub = Rational.sub f11 f12
let test_sub2 = Rational.to_pair (Rational.sub f11 f31)
let test_mul = Rational.to_pair (Rational.mul f22 f22)
let test_mul2 = Rational.to_pair (Rational.mul f12 f12)
let test_to_pair = Rational.to_pair f24


