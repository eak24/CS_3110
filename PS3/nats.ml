(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig

  (* Identifies the value is a natural number (within bounds)
   * (i.e. one of 0, 1, 2, ... , max_int)
   * where max_int is the largest value possible for this type  *)
  type t
  
  (* zero is the identity of + : zero+a===a 
   * zero is the smallest value a variable of type t can have *)
  val zero : t

  (*one is the identity of * : one*a===a *)
  val one : t

  (* Returns: Sum of the two arguments
   * + is associative: (a+b)+c === a+(b+c)
   * + is commutative: a+b === b+a 
   * Precondition: sum of arg1 and arg2 must be less than or equal to max_int 
   * Raises: exception Unrepresentable if arg1+arg2>max_int *)
  val ( + ) : t -> t -> t

  (*Returns: Product of the two arguments 
   * is associative: (a*b)*c === a*(b*c)
   * is commutative: a*b === b*c 
   * is distributive: a*(b+c) === a*b + a*c 
   * Precondition: arg1*arg2<= max_int or it will raise an exception.
   * Raises exception(s): Unrepresentable*)
  val ( * ) : t -> t -> t

  (* a<b is true if a is less than b, else false*)
  val ( < ) : t -> t -> bool

  (* a===b is true if a equals b, else false*)
  val ( === ) : t -> t -> bool
	
  (*Thrown if while converting between int and nat, 
   * the number can't be represented with the other type.
   * e.g, since nat must be non-negative, if you try and
   * convert -1:int to nat, Unrepresentable is thrown. 
   * Also thrown if the value a function would return is supposed to be of 
   * type t, but has a value that is greater than max_int *)		    
  exception Unrepresentable
  
  (* Converts nat to int
   * Precondition: None, but note that values of type t must be in [0, max_int]
   * Postcondition: return value will represent the natural number inputted. 
   * Raises exception(s): None *)
  val int_of_nat: t -> int

  (* Converts int to nat
   * Precondtion: arg is in [0, max_int]
   * Postcondition: return value corresponds to int
   * Raises exception(s): 
   *     Unrepresentable : if arg<0 *)
  val nat_of_int: int -> t

end

module type AlienMapping = sig

  (*Type refers to values in the alien system, which we assume to be within
   * [0, max_int] in terms of the corresponding ints they represent.*)
  type aliensym

  (*Returns: Int corresponding to the aliensym value. *)
  val int_of_aliensym: aliensym -> int

  (* The value of type aliensym that corresponds to a NATN value of one *)
  val one: aliensym

  (* The value of type aliensym that corresponds to a NATN value of zero *)
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

  (* Tells whether product overflows or not.
   * Returns: Boolean, True if product overflows.
   * Precondition: None
   * Postcondition: None 
   * Raises: Shouldn't raise any errors *)
  let rec mult_overflows (i1:int) (i2:int): bool =
    let rec helper (i:int) (count:int) (acc:int): bool =
      if not (sum_overflows i acc) && count>0 then (helper i (count-1) (acc+i)) 
    else if count<=0 then false else true in 
    helper i1 (i2-1) i1

  let ( * ) (a:t) (b:t): t =
    if mult_overflows a b then raise Unrepresentable
    else a*b

  let ( < ) (a:t) (b:t): bool =
    a<b 

  let ( === ) (a:t) (b:t): bool =
    a=b

  let int_of_nat (a:t): int =
    if a>max_int then raise Unrepresentable
    else a

  let nat_of_int (a:int): t =
    if a<0 then raise Unrepresentable
    else a
end

module ListNat : NATN = struct

  (* The list [ a1 ; ...; an ] represents the
  * natural number n . That is , the list lst represents
  * length ( lst ). The empty list represents 0. The values of
  * the list elements are irrelevant . *)
  type t = int list

  let zero = []
  let one = [1]

  exception Unrepresentable

  (* Tells whether product overflows or not.
   * Returns: Boolean, True if product overflows.
   * Precondition: None
   * Postcondition: None 
   * Raises: Shouldn't raise any errors *)
  let mult_overflows (i1:int) (i2:int): bool =
    let rec helper (i:int) (count:int) (acc:int): bool =
      if not (sum_overflows i acc) && count>0 then (helper i (count-1) (acc+i)) 
    else if count<=0 then false else true in 
    helper i1 (i2-1) i1

  let nat_of_int (i:int):t =
    if i<0 then raise Unrepresentable 
    else
      let rec helper (i:int) (acc:t): t =
        if i=0 then acc 
        else helper (i-1) (1::acc)
      in
      helper i []
      (*???I think this is tail recursive*)

  let ( + ) (a:t) (b:t): t =
    (*The return type of List.length is an int which must be less than max_int*)
    let la= List.length(a) in
    let lb= List.length(b) in
    if sum_overflows la lb then raise Unrepresentable
    else a@b  

  let ( * ) (a:t) (b:t): t =
    let la= List.length(a) in
    let lb= List.length(b) in
    if mult_overflows la lb then raise Unrepresentable
    else nat_of_int(la*lb)

  let ( < ) (a:t) (b:t): bool =
    let la= List.length(a) in
    let lb= List.length(b) in
    la<lb 

  let ( === ) (a:t) (b:t): bool =
    List.length(a)=List.length(b)

  let int_of_nat (nat:t):int=
    List.length(nat)

end


module NatConvertFn ( N : NATN ) = struct
  (*For the below functions, I take advantage of the respective functions
   * inside the module *)

  (* Converts a number of type N.t to the corresponding int value.
   * May raise an error if the value of type t cannot be represented as an int
   * Precondition: None
   * Postcondition: None*)
  let int_of_nat ( n : N.t ): int = N.int_of_nat(n)

  (* Converts a number of type int to the corresponding value of type N.t 
   * May raise an error if the value of type int cannot be represented as a 
   * value of type t.
   * Precondition: None
   * Postcondition: None*)
  let nat_of_int ( n : int ): N.t = N.nat_of_int(n)
end

module AlienNatFn ( M : AlienMapping ): NATN = struct
  (* max value for type t is stil max_int, see below:
   * As specified above in the defn of NATN, all values of type t must be
   * between [0, max_int].  Thus an error Unrepresentable 
   * will be raised even if it is possible to assign a value of type
   * t to be greater than max_int.  Consider the M.aliensym list
   * [%, %] where % is the symbol for max_int, although this is 
   * technically representable in this system, because it wasn't
   * in other implementations, trying to create this natural number
   * will still throw an error for consistency. *)
  type t = M.aliensym list
  let zero = [M.zero]
  let one = [M.one]
  exception Unrepresentable

  let nat_of_int (i:int):t= 
  (* Any int is garunteed to be of value max_int or less, so type t invariant
   * preserved.*)
    if i<0 then raise Unrepresentable 
    else
      let rec helper (counter:int):t =
        match counter with
        | 0 -> [M.one]
        | x -> M.one::helper (x-1)
      in
      helper(i)
  
  let int_of_nat (n:t):int = 
  (*No need to check for unrepresentability because of invariant about type t*)
    let int_list=
      List.fold_left (fun a x -> M.int_of_aliensym(x)::a) [] n
    in
    List.fold_left (fun a x -> a+x) 0 int_list

  let ( === ) (a:t) (b:t):bool = 
    int_of_nat(a)=int_of_nat(b)

  let ( < ) (a:t) (b:t):bool = 
    int_of_nat(a)<int_of_nat(b)

  let ( + ) (a:t) (b:t):t = 
    (*Will throw an error if the sum is greater than max_int even if it would
     * be representable*)
    let ia = int_of_nat(a) in
    let ib = int_of_nat(b) in
    if sum_overflows ia ib then raise Unrepresentable
    else a@b

  let mult_overflows (i1:int) (i2:int): bool =
    let rec helper (i:int) (count:int) (acc:int): bool =
      if not (sum_overflows i acc) && count>0 then (helper i (count-1) (acc+i)) 
    else if count<=0 then false else true in 
    helper i1 (i2-1) i1

  let ( * ) (a:t) (b:t):t = 
    let la= int_of_nat(a) in
    let lb= int_of_nat(b) in
    if mult_overflows la lb then raise Unrepresentable
    else nat_of_int(la*lb)
  

end
