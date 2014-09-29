(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
  (*Identifies the value is a natural number (i.e. one of 0, 1, 2, ... ) *)
  type t
  
  (*zero is the identity of + : zero+a===a *)
  val zero : t
  (*one is the identity of * : 1*a===a *)
  val one : t
  (* + is associative: (a+b)+c === a+(b+c) *)
  (* + is commutative: a+b === b+a *)
  val ( + ) : t -> t -> t
  (* * is associative: (a*b)*c === a*(b*c) *)
  (* * is commutative: a*b === b*c *)
  (* * is distributive: a*(b+c) === a*b + a*c *)
  val ( * ) : t -> t -> t
  (* a<b is true if a is less than b, else false*)
  val ( < ) : t -> t -> bool
  (* a===b is true if a equals b, else false*)
  val ( === ) : t -> t -> bool
	
  (*Thrown if while converting between int and nat, 
   * the number can't be represented with the other type.
   * e.g, since nat must be non-negative, if you try and
   * convert -1:int to nat, Unrepresentable is thrown.
   * Some other cases: convert 2**128:nat to int*)		    
  exception Unrepresentable
  
  (*Converts nat to int*)
  val int_of_nat: t -> int
  (*Converts int to nat*)
  val nat_of_int: int -> t
end

module type AlienMapping = sig
  type aliensym

  val int_of_aliensym: aliensym -> int
  val one: aliensym
  val zero: aliensym
end

type sign = Positive | Negative
let sign_int (n:int) : sign = 
  if n >= 0 then Positive else Negative
let sum_overflows (i1:int) (i2:int) : bool = 
  sign_int i1 = sign_int i2 && sign_int(i1 + i2) <> sign_int i1
(* END: DO NOT CHANGE THIS CODE *)

(* Add your solution here for IntNat, ListNat, NatConvertFn, 
   and AlienNatFn, being careful to use the declarations and
   types specified in the problem set. *)
module IntNat: NATN = struct
  type t = int

  let zero=0
  let one =1

  exception Unrepresentable

  let ( + ) (a:t) (b:t): t =
    if sum_overflows a b then raise Unrepresentable
    else a+b 

  let rec mult_overflows (i1:int) (i2:int): bool =
    let rec helper (i:int) (count:int) (acc:int): bool =
      if (sum_overflows i acc) && count=0 then (helper i count (acc+i)) else false
    in helper i1 (i2-1) i1



  let ( * ) (a:t) (b:t): t =
    if mult_overflows a b then raise Unrepresentable
    else a*b

  let ( < ) (a:t) (b:t): bool =
    a<b 

  let ( === ) (a:t) (b:t): bool =
    a=b

  let int_of_nat (a:t): int =
    a

  let nat_of_int (a:int): t =
    a
end

