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

(* List.fold_left f a [b1; ...; bn] is f (... (f (f a b1) b2) ...) 
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

type t = unit  (* TODO: change unit to whatever you want *)
type u = unit  (* TODO: change unit to whatever you want *)
let lst : t list = failwith "TODO"
let zardoz (x:t) : u = failwith "TODO"

