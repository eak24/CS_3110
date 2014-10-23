open Array
(* Returns a function of type unit -> int; let’s call that returned function
 * f. The first time f is called, it should return n. The second time f is 
 * called, it should return n+k. More generally, the ith time f is called, 
 * it should return n+(i-1)*k.
 * Precondition: none.*)
let count_up_from (n:int) (k:int) : unit -> int = 
  let i = ref 0 in
  fun ()-> let _ = i:=(!i+1) in n+(!i-1)*k

(* Returns an array of length n where the element at index i equals f i. 
 * For example, tabulate (fun x -> x*x) 5 yields the array [|0; 1; 4; 9; 16|].*)
let tabulate (f:int -> 'a) (n:int) : 'a array = 
  let a = make n (f 0) in
  let rec helper i =
    set a i (f i);
    if i=0 then 0 else helper (i-1) 
  in 
  let _ = helper (n-1) in
  a

(* Folds a function over a list starting from the left with accumulator acc such 
 * that List.fold_left f a [b1; ...; bn] is f (... (f (f a b1) b2) ...) 
 * bn.
 * Precondition: None *)
let fold_left_imp (f:'a -> 'b -> 'a) (acc:'a) (xs:'b list) : 'a = 
let aref = ref acc in
let xref = ref xs in
let _ = while !xref <> [] do
match !xref with
h::t -> aref := (f !aref h); xref := t 
|[] -> ()
done in
!aref

(* An example of when mutability makes List.map f (List.rev xs) = 
 * List.rev (List.map f xs) not hold. It works by storing a mutable
 * value outside of the map function call so that map depends on 
 * the order in which values were passed in.*)
type t = bool  
type u = int  
let lst : t list = [true;false]

let store_state_outside = count_up_from 3 4
let zardoz (x:t) : u =
  if x then store_state_outside() else 0

