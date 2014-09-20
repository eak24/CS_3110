
(* PART 1: EXPRESSION TREES *)
type 'a exprTree = 
  | Val of 'a
  | Unop of ('a -> 'a) * 'a exprTree 
  | Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree

  (**x:a'exprTree = (Binop ((+), Val 3, Unop ((~-), Binop ((/), Val 5, Val 2))))**)
(*returns the number of function app in an exprTree*)
let rec count_ops (et : 'a exprTree) : int = 
        match et with
        | Val x -> 0
        | Unop (y,yt) -> count_ops (yt) + 1 
        | Binop (z,z2,zt) -> count_ops (z2) + count_ops (zt) + 1

  

(*returns an exprTree representing the execution of fact n (as defined in the
  write-up for exercise 5)*)
let rec make_fact_tree (n : int) : int exprTree =
  failwith "I mean, why would a poptart want to live inside a toaster, Rick? I 
  mean, that would be like the scariest place for them to live. You know what I 
  mean?"
  
(*computes the expression represented by [et]*)
let rec eval (et : 'a exprTree) : 'a =
  failwith "You're missing the point, Morty. Why would he drive a smaller 
  toaster with wheels? I mean does your car look like a smaller version of your 
  house? No."

(* PART 2: FOLDING*)

let product (lst : float list) : float =
    match lst with
    [] -> 1.0
    | lst -> List.fold_left (fun acc x -> acc*.x) 1.0 lst

let concat_left (lst : string list) : string = 
  failwith "SQUIRREL!!"

let concat_right (lst : string list) : string = 
  failwith "POINT!"

let mapi_lst (f: (int -> 'a -> 'b)) (lst: 'a list) : 'b list =
    let length lst: int = List.fold_right (fun x acc -> acc+1) lst 0
in
  List.rev (List.fold_left (fun acc x -> (f (length acc) x)::acc) [] lst)
  

let outline (lst: string list) : string list =
    mapi_lst (fun i a -> (string_of_int (i+1))^". "^a) (lst: 'a list)

      
let scan_right (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  List.rev (List.fold_right (fun x acc -> (f x (List.hd acc))::acc) lst [acc])
      
let scan_left (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  List.rev (List.fold_left (fun acc x -> (f x (List.hd acc))::acc) [acc] lst)

(* requires: n >= 1 
   returns: the list [1;2;...;n] *)
let countup (n:int) : int list =
  (* tail-recursive helper function for countup:  
       accumulate the answer in l, 
       starting from n and working down *)
  let rec countup' i l =
    if i<=0 then l
    else countup' (i-1) (i::l)
  in countup' n []

let fact_list (n: int) : int list =
  scan_left (fun acc x -> x*acc) 1 (List.tl (countup n))


(* PART 3: MATRICES *)

type vector = int list
type matrix = vector list

exception MatrixFailure of string

let show (m : matrix) : unit = 
  List.fold_left (fun acc x -> print_endline (List.fold_left (fun acc x -> 
    (string_of_int x)^" "^acc) "" (List.rev x))) () m

let insert_col (m : matrix) (c : vector) : matrix = 
  failwith "Seize reason in your own hand / With your own teeth savor the fruit"

let transpose (m : matrix) : matrix = 
  failwith "It is a way of thought"

let add_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  failwith "My brain is open"

let multiply_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  failwith "If numbers aren't beautiful, I don't know what is"

(* PART 4: PATTERN MATCHING *)

(*type definitions: **********************************************************)
type pat =
  | WCPat (*short for "wildcard pattern"; equivalent to an underscore*)
  | VarPat of string
  | UnitPat
  | ConstPat of int
  | TuplePat of pat list
  | StructorPat of string * (pat option) (*Short for "constructor pattern"*)

type value = 
  | ConstVal of int
  | UnitVal
  | TupleVal of value list
  | StructorVal of string * (value option)

type bindings = (string * value) list option

(*1. *************************************************************************)
(*Precondition: f1 must take in type unit (unit->int), and f2 takes in type 
string (string->int) and both return an integer. 
Postcondition: returns an integer. Takes in a pattern and functions f1 and f2. 
Applies f1 to wildcard patterns and f2 to variable patterns within Tuple or 
Struct patterns.*)
let rec z f1 f2 p =
  let r = z f1 f2 in
    match p with
    | WCPat -> f1 ()
    | VarPat x -> f2 x
    | TuplePat ps -> List.fold_left (fun acc e -> (r e) + acc) 0 ps
    | StructorPat (_,Some p) -> r p
    | _ -> 0

(*counts the number of wildcards that occur in a pattern*)
let count_wcs (p: pat) : int = 
    z (fun () -> 1) (fun s -> 0) p

(*counts the number of wildcards in a pattern, and adds that quantity to the sum
of the lengths of the variable names used in the pattern*)
let count_wcs_and_var_lengths (p: pat) : int = 
  z (fun () -> 1) (fun s -> String.length s) p

(*counts how oftern a variable occurs in a pattern*)
let count_var (var_name: string) (p: pat) : int = 
  z (fun () -> 0) (fun s -> if (s=var_name) then 1 else 0) p

(*2. *************************************************************************)

(*Takes in a type pat and returns the names of all the variables in the pattern 
in no specified order. Precondition: p is a valid pattern. 
Postcondition: a list of the strings of the variable names)*)
let rec extract_names (p: pat) : string list = 
  match p with
    | VarPat x -> [x]
    | TuplePat ps -> List.fold_left (fun acc e -> (extract_names e)@acc) [] ps
    | StructorPat (s,Some p) -> s::(extract_names p)
    | _ -> []

(*Takes a list of any type and returns true if the list contains duplicates, 
and false if all elements are unique. []->false. Precondition: A list. 
Postcondition: boolean.)*)
let has_dups (l: 'a list) : bool = 
  (*checks if element e is equal to any elements is l*)
  let rec chkforel (e: 'a) (l:'a list) : bool =
    match l with 
    |[] -> false
    |hd::tl -> if (e=hd) then true else (chkforel e tl)
  in
  match l with
  |[] -> false
  |h1::h2::tl -> if (chkforel h1 (h2::tl)) then true else (chkforel h2 tl)
  |hd::tl -> false

(*Checks if all the variable names in a pattern are all unique. 
*Precondition: a valid pattern. Postcondition: a boolean.*)
let all_vars_unique (p: pat) : bool = 
  not (has_dups (extract_names p))
(*3. *************************************************************************)

(*Passes elements in the original list into f and combines the output, minus 
*the option into a single list. If any f a -> None then return None, else return 
*Some 'b list.
*Precondition: 
*Postcondition:*)
let all_answers (f: 'a -> 'b list option) (l: 'a list) : 'b list option =
  let rec comblist (acc: 'b list) (l: 'b list option list) : 'b list option =
    match l with
    [] -> Some (acc)
    | (Some (hd))::tl -> comblist (hd@acc) tl
    | None::_ -> None
  in
comblist [] (List.map f l)

(*4. *************************************************************************)

(*Matches value v with pattern p and returns the bindings produced as a result 
*of the match. If no match is produced, return None. If a match but no bindings 
*are produced, return Some []. If a match and bindings are produced, return 
*Some l where l is the list of bindings.
*Precondtion:
Postcondition:*)
let rec match_pat (v,p) : bindings =
  match (v,p) with
  | _ , WCPat -> Some []
  | _ , VarPat s -> Some [(s,v)]
  | UnitVal, UnitPat -> Some []
  | ConstVal a, ConstPat b -> Some []
  | TupleVal v', TuplePat p' -> if (List.length v' = List.length p') then 
    (all_answers match_pat (List.combine v' p')) else None
  | StructorVal (s,v'_opt), StructorPat (s',p'_opt) -> if s<>s' then None else
    (match (v'_opt,p'_opt) with 
    | (None, None) -> Some []
    | (_,None) -> None
    | (None,_) -> None
    | (Some v', Some p') -> match_pat (v',p'))
  | (_,_) -> None

(*5. *************************************************************************)
exception NoAnswer

(*Finds the first occurence of some when each element a of l is passed into f
*Precondition:
*Postcondition:*)
let rec first_answer (f: 'a -> 'b option) (l: 'a list) =
  match l with
  |hd::tl -> if ((f hd)=None) then first_answer f tl else 
    (match (f hd) with
    |Some x -> x
    |_ -> NoAnswer)
  |[] -> NoAnswer


(*6. *************************************************************************)

(*Performs a pattern matching on a list of patters, ps and a value v. Returns 
*the bindings produced from the first successful match. If no patterns match, 
*returns None, and if a pattern matches but no bindings produced, 
*returns Some[].
*Precondition:
*Postcondition:*)
let rec match_pats ((v: value), (ps: pat list)) : bindings =
  match ps with
  [] -> None
  | hd::tl -> if match_pat (v,hd)=None then match_pats (v,tl) 
    else match_pat (v,hd)
