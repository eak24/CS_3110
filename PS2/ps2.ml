
(* PART 1: EXPRESSION TREES *)
type 'a exprTree = 
  | Val of 'a
  | Unop of ('a -> 'a) * 'a exprTree 
  | Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree

(*returns the number of function app in an exprTree*)
let rec count_ops (et : 'a exprTree) : int = 
        match et with
        | Val x -> 0
        | Unop (y,yt) -> count_ops (yt) + 1 
        | Binop (z,z2,zt) -> count_ops (z2) + count_ops (zt) + 1

  

(*returns an exprTree representing the execution of fact n (as defined in the
  write-up for exercise 5)*)
let rec make_fact_tree (n : int) : int exprTree =
  if n=0||n=1 then Val 1 else
  Binop (( * ), Val n, make_fact_tree(n-1))
  
(*computes the expression represented by [et]*)
let rec eval (et : 'a exprTree) : 'a =
  match et with 
  | Val x-> x
  | Unop (u, y) -> u (eval(y))
  | Binop (b, w, v) -> b (eval(w)) (eval(v))

(* PART 2: FOLDING*)

(*Computes the product of all the floats in a list, ie h1*h2*h3...*t*)
let product (lst : float list) : float =
  match lst with
  [] -> 1.0
  | lst -> List.fold_left (fun acc x -> acc*.x) 1.0 lst

(*Concatenates a list of strings together using fold_left, ie h1^h2^h3...^t*)
let concat_left (lst : string list) : string = 
  List.fold_left (fun s1 s2-> s1^s2) "" lst

(*Concatenates a list of strings together using fold_right, ie h1^h2^h3...^t*)
let concat_right (lst : string list) : string = 
  List.fold_right (fun s1 s2-> s1^s2) lst ""

(*The input function f is applied to the index (counting from 0) of the element 
in the list as the first argument and the element itself as the second 
argument.*)
let mapi_lst (f: (int -> 'a -> 'b)) (lst: 'a list) : 'b list =
  let length lst: int = List.fold_right (fun x acc -> acc+1) lst 0
in
  List.rev (List.fold_left (fun acc x -> (f (length acc) x)::acc) [] lst)
  
(*Appends an outline numbering to each string of the list that includes the 
*index number +1, (starting with 1), followed by a period followed by a space. 
*Ex: outline ["A";"B";"C"] = ["1. A"; "2. B"; "3. C"].*)
let outline (lst: string list) : string list =
  mapi_lst (fun i a -> (string_of_int (i+1))^". "^a) (lst: 'a list)

(*Performs a fold_right, but the accumulator is a list containing all the values 
*the accumulator takes on as f is applied to each element in lst starting with 
*the right-most element*)     
let scan_right (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  List.rev (List.fold_right (fun x acc -> (f x (List.hd acc))::acc) lst [acc])

(*Performs a fold_left, but the accumulator is a list containing all the values 
*the accumulator takes on as f is applied to each element in lst starting with 
*the left-most element*)       
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

(*Takes in a positive int n and creates a list of each factorial of every 
*integer starting from 1 and ending with n. 
*Precondition: n > 0 and !n < max int.
*Postcondition: if !n happens to be larger than max int, may return 0, 
*negatives or other nonsense.*)
let fact_list (n: int) : int list =
  scan_left (fun acc x -> x*acc) 1 (List.tl (countup n))


(* PART 3: MATRICES *)

type vector = int list
type matrix = vector list

exception MatrixFailure of string

(*Takes in a rectangular matrix and returns the number of columns in the 
*matrix.
*Preconditions: matrix must be rectangular, meaning all vector lengths are equal
*Postcondition: the number of vectors in the matrix or -1 if the matrix is not 
*rectangular*)
let num_cols (m : matrix) : int =
  match m with 
  [] -> 0
  | h::t -> List.fold_left (fun acc x -> if (acc=List.length x) then acc 
    else -1) (List.length h) m

(*Takes in a matrix and returns true if the matrix is rectangular and false if 
*not.*)
let is_matrix (m : matrix) : bool =
  if (num_cols m) = -1 then false else true

(*Takes in a rectangular matrix and returns a printout of the whole matrix, 
*with each vector on a new line and spaces between the elements.*)  
let show (m : matrix) : unit = 
  if (is_matrix m)=false then raise (MatrixFailure "Matrix is not rectangular") 
  else List.fold_left (fun acc x -> print_endline (List.fold_left (fun acc x -> 
  (string_of_int x)^" "^acc) "" (List.rev x))) () m

(*Takes in a rectangular matrix and a vector with the same number of elements 
*as the number of rows in the input matrix.
*Precondition: vector c must have the same number of elements as the number of 
*rows in matrix m.
*Postcondition: returns a matrix with the vector elements appended as a new 
*column to matrix m.*)
let insert_col (m : matrix) (c : vector) : matrix = 
  if (is_matrix m)=false then raise (MatrixFailure "Matrix is not rectangular") 
  else if (List.length c) <> (List.length m) then raise
    (MatrixFailure "Number of elements in c and rows in m do not match") else
    List.rev (List.fold_left2 (fun acc v ce -> (v@[ce])::acc) [] m c)

(*Takes in a rectangular matrix and switches the rows for columns, transposing 
*it.*)
let transpose (m : matrix) : matrix = 
  if (is_matrix m)=false then raise (MatrixFailure "Matrix is not rectangular") 
  else match m with 
      [] -> []
      | h::t -> 
        let m' = List.rev (List.fold_left (fun acc x -> [x]::acc) [] h) in
      List.fold_left (fun acc x -> insert_col acc x) m' t

(*Takes in two rectangular matrices of equal size and returns a matrix of 
*equal size composed of the sum of each element of the first two in the 
*corresponding locations.
*Preconditions: The matrices m1 and m2 must be of identical size.*)
let add_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  if ((is_matrix m1)=false || (is_matrix m2)=false) then raise (MatrixFailure 
    "Matrix is not rectangular")
  else List.rev (List.fold_left2 (fun accv v1 v2 -> List.rev (List.fold_left2 
    (fun acce e1 e2 -> (e1+e2)::acce) [] v1 v2)::accv) [] m1 m2)

(*Multiplies two rectangular matrices together. 
*Preconditions: the number of columns of the first matrix must equal the number 
of rows of the second.
*Postcondition: the returned matrix has as many rows as m2 has columns and as 
*many columns as m1 has rows*)
let multiply_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  let dt_prd v1 v2 = List.fold_left2 (fun acc e1 e2 -> 
      (e1*e2)+acc) 0 v1 v2 in
  if ((is_matrix m1)=false || (is_matrix m2)=false) then raise (MatrixFailure 
    "Matrix is not rectangular") else if (num_cols m1) <> (List.length m2) then
    raise (MatrixFailure "The number of columns of the first matrix does not 
      equal the number of rows of the second")
    else List.rev (List.fold_left (fun acc r -> (List.rev (List.fold_left 
      (fun r' c -> (dt_prd c r)::r') [] (transpose m2)))::acc) [] m1) 

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
(*Takes in a pattern p and functions f1 and f2. Applies f1 to wildcard patterns 
*and f2 to variable patterns that can be within Tuple or Struct patterns.
*Precondition: f1 must be of type (unit->int), and f2 must be of type 
*(string->int). 
*Postcondition: returns an integer.*)
let rec z (f1: unit->int) (f2: string->int) (p: pat) : int =
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

(*counts the number of wildcards in a pattern, and adds that quantity to the 
*sum of the lengths of the variable names used in the pattern*)
let count_wcs_and_var_lengths (p: pat) : int = 
  z (fun () -> 1) (fun s -> String.length s) p

(*counts how often a variable occurs in a pattern*)
let count_var (var_name: string) (p: pat) : int = 
  z (fun () -> 0) (fun s -> if (s=var_name) then 1 else 0) p

(*2. *************************************************************************)

(*Takes in a type pat and returns the names of all the variables in the pattern 
*in no specified order. 
*Precondition: p is a valid pattern. 
*Postcondition: a list of the strings of the variable names)*)
let rec extract_names (p: pat) : string list = 
  match p with
    | VarPat x -> [x]
    | TuplePat ps -> List.fold_left (fun acc e -> (extract_names e)@acc) [] ps
    | StructorPat (s,Some p) -> s::(extract_names p)
    | _ -> []

(*Takes a list of any type and returns true if the list contains duplicates, 
*and false if all elements are unique. []->false.*)
let has_dups (l: 'a list) : bool = 
  (*checks if element e is equal to any elements is l*)
  let rec chk_for_el (e: 'a) (l:'a list) : bool =
    match l with 
    |[] -> false
    |hd::tl -> if (e=hd) then true else (chk_for_el e tl)
  in
  match l with
  |[] -> false
  |h1::h2::tl -> if (chk_for_el h1 (h2::tl)) then true else (chk_for_el h2 tl)
  |hd::tl -> false

(*Checks if all the variable names in a pattern are all unique. Returns true iff
*all names are unique, false otherwise.*)
let all_vars_unique (p: pat) : bool = 
  not (has_dups (extract_names p))
(*3. *************************************************************************)

(*Passes elements in the original list into f and combines the output without 
*the option into a single list in no specified order. If any f a -> None then 
*return None, else return Some 'b list.*)
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
*Some l where l is the list of bindings.*)
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

(*Returns the contents of the first occurence of Some as each element of l 
is passed into f. If Some is never produced, return exception NoAnswer*)
let rec first_answer (f: 'a -> 'b option) (l: 'a list) =
  match l with
  |hd::tl -> if ((f hd)=None) then first_answer f tl else 
    (match (f hd) with
    |Some x -> x
    |_ -> raise NoAnswer)
  |[] -> raise NoAnswer


(*6. *************************************************************************)

(*Performs a pattern matching on a list of patterns, ps and a value v. Returns 
*the bindings produced from the first successful match. If no patterns match, 
*returns None, and if a pattern matches but no bindings produced, 
*returns Some[]. If a match and bindings are produced, return 
*Some l where l is the list of bindings produced by the first match.*)
let rec match_pats ((v: value), (ps: pat list)) : bindings =
  match ps with
  [] -> None
  | hd::tl -> if match_pat (v,hd)=None then match_pats (v,tl) 
    else match_pat (v,hd)
