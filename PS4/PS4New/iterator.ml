module type ITERATOR = sig
  type 'a t
  exception NoResult

  (* returns: true if there are more results to yield,
   *   otherwise returns false. *)
  val has_next: 'a t -> bool

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  val next: 'a t -> 'a
end

module type LIST_ITERATOR = sig
  include ITERATOR
  (* parameters:  a list l
   * returns:  an iterator that will yield the elements of l,the
   *   each exactly once, in the order that they appear in l,
   *   starting with the head.  *)
  val create: 'a list -> 'a t
end

module ListIterator : LIST_ITERATOR = struct
  type 'a t =  'a list ref
  exception NoResult
  let has_next (x:'a t)=
   match !x with
   | []->false
   | h::t -> true

  let next (x: 'a t): 'a =
    match !x with 
    | [] -> raise NoResult
    | h::t -> let _ = x:=t in h

  let create  (lst:'a list): 'a t =
    ref lst

end

type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

module type INORDER_TREE_ITERATOR = sig
  include ITERATOR
  (* parameters:  a tree t
   * returns:  an iterator that will yield the elements of t,
   *   each exactly once, in the order that would be produced
   *   by an in-order traversal of t. *)
  val create: 'a tree -> 'a t
end

module InorderTreeIterator : INORDER_TREE_ITERATOR = struct
  type 'a t = 'a list ref
  exception NoResult
  let has_next (x:'a t)=
   match !x with
   | []->false
   | h::t -> true

  let next (x: 'a t): 'a =
    match !x with 
    | [] -> raise NoResult
    | h::t -> let _ = x:=t in h

  let rec create (t:'a tree): 'a t =
    match t with
    | Leaf -> ref []
    | Node (v,l,r) -> ref ((!(create l))@[v]@(!(create r)))
end

module type TAKE_ITERATOR = functor (I: ITERATOR) -> sig
  include ITERATOR

  (* parameters:  an integer n and an iterator i
   * returns:  an iterator that behaves the same as i for
   *   exactly n calls to next, but afterwards
   *   raises NoResult. *)
  val create: int -> 'a I.t -> 'a t
end


module TakeIterator : TAKE_ITERATOR = functor (I : ITERATOR) -> struct
  type 'a t =  'a I.t * int ref
  exception NoResult

  (* Returns: bool, true if I.has_next would return true
   * and has_next*)
  let has_next (x: 'a t)= 
    let (a, i)=x in 
    if (!i)>0 then I.has_next (fst x) else false (*next should be
    able to return something on its next call for this to
    be true *)

  let next (x: 'a t)=
    let (a, i)=x in 
    let _ = i:=((!i)-1) in
    if (!i)>=0 then I.next (fst x) else raise NoResult
  let create  (x:int) (y: 'a I.t) =
    (y, ref x)

end


module IteratorUtilsFn (I : ITERATOR) = struct
  open I

  (* effects: causes i to yield n results, ignoring
   *   those results.  Raises NoResult if i does.  *)
  let advance (n: int) (iter: 'a I.t) : unit =
    failwith "Not implemented"

  (* returns: the final value of the accumulator after
   *   folding f over all the results returned by i,
   *   starting with acc as the initial accumulator.
   * effects: causes i to yield all its results. *)
  let rec fold (f : ('a -> 'b -> 'a)) (acc : 'a) (iter: 'b I.t) : 'a =
    failwith "Not implemented"
end

module type RANGE_ITERATOR = functor (I : ITERATOR) -> sig
  include ITERATOR

  (* parameters: integers n and m and an iterator i
   * returns: an iterator that behaves the way I would
   *   on the nth through mth calls to next, and
   *   afterwards raises NoResult.
   *
   *   If n > m the resulting iterator should always raise NoResult.
   *)
  val create : int -> int -> 'a I.t -> 'a t
end

(*
module RangeIterator : RANGE_ITERATOR = functor (I : ITERATOR) -> struct
  
end
*)
