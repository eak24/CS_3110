open Ast

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding
and environment = value ref Environment.environment

(* Exceptions*)
exception Error of string 

(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =


  (* Helper functions for read_expression... takes in a datum with cons'd 
   * expressions and produces the corresponding list of expressions.*)
  let rec cons_to_exp_list (dat : datum) : expression list =
    match dat with 
    | Cons (car,Nil) -> [read_expression car]
    | Cons (car,cdr) -> (read_expression car)::(cons_to_exp_list cdr)
    | _ -> [read_expression dat]
  in

  let rec cons_to_var_list (dat : datum) : variable list =
    match dat with 
    | Nil -> []
    | Atom (Identifier id) -> [Identifier.variable_of_identifier id]
    | Cons (car,cdr) -> 
        (cons_to_var_list car) @ (cons_to_var_list cdr)
    | _ -> raise (Error "cons_to_var_list, this should be a variable")
  in

  let rec let_binding_helper (dat : datum) : let_binding list = 
    match dat with
    | Cons ( Cons (Atom (Identifier id), cdr'), Nil) 
        when Identifier.is_valid_variable id -> 
        [((Identifier.variable_of_identifier id),(read_expression cdr'))]

    | Cons ( Cons (Atom (Identifier id),cdr'), cdr) 
        when Identifier.is_valid_variable id -> 
        ((Identifier.variable_of_identifier id),(read_expression cdr'))::
        (let_binding_helper cdr)

    | _ -> raise (Error "let_binding_helper warning, datum must be let binding")
in

  match input with
  | Nil -> raise (Error "Unknown expression form")

  (* Self evaluating matches*)
  | Atom (Identifier id) when Identifier.is_valid_variable id ->
    ExprVariable (Identifier.variable_of_identifier id)

  | Atom (Identifier id) -> raise (Error "Keyword is invalid variable name")
  | Atom (Boolean b) -> ExprSelfEvaluating (SEBoolean b)
  | Atom (Integer i) -> ExprSelfEvaluating (SEInteger i)

  (* Cons(keyword, cdr) matches*)
  | Cons (Atom (Identifier id),cdr) when Identifier.string_of_identifier id = 
      "quote" -> ExprQuote cdr

  | Cons (Atom (Identifier id),(Cons (a, (Cons (b, c))))) when 
      Identifier.string_of_identifier id = "if" ->
      ExprIf ((read_expression a), (read_expression b), (read_expression c))

  | Cons (Atom (Identifier id),Cons (car',cdr')) when 
      Identifier.string_of_identifier id = 
      "lambda" -> ExprLambda ((cons_to_var_list car'),(cons_to_exp_list cdr'))

  | Cons (Atom (Identifier id), Cons (Atom (Identifier var),cdr)) 
      when Identifier.string_of_identifier id = 
      "set!" -> ExprAssignment 
      ((Identifier.variable_of_identifier var),(read_expression cdr)) 

  | Cons (Atom (Identifier id), Cons(b1, cdr')) when 
      Identifier.string_of_identifier id = 
      "let" -> ExprLet ((let_binding_helper b1),(cons_to_exp_list cdr')) 

  | Cons (Atom (Identifier id), Cons(b1, cdr')) when 
      Identifier.string_of_identifier id = 
      "let*" -> ExprLetStar ((let_binding_helper b1),(cons_to_exp_list cdr')) 

  | Cons (Atom (Identifier id), Cons(b1, cdr')) when 
      Identifier.string_of_identifier id = 
      "letrec" -> ExprLetRec ((let_binding_helper b1),(cons_to_exp_list cdr')) 

  (* Cons recursive calls*)
  | Cons (car, Nil) -> read_expression car
  | Cons (car, cdr) -> ExprProcCall (read_expression car, cons_to_exp_list cdr)



(* Parses a datum into a toplevel input. *)
let read_toplevel (input : datum) : toplevel =
  match input with
  | Cons (Atom (Identifier id),Cons (Atom (Identifier var), cdr)) 
      when Identifier.string_of_identifier id = 
      "define" -> ToplevelDefinition (Identifier.variable_of_identifier var,
      read_expression cdr)
  | _ -> ToplevelExpression (read_expression input)

(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =

  let e_empt = Environment.empty_environment in

  let e_course = Environment.add_binding e_empt 
    (Identifier.variable_of_identifier 
    (Identifier.identifier_of_string "course"), ref (ValDatum 
    (Atom (Integer 3110)))) in

  let e_car = Environment.add_binding e_course 
    (Identifier.variable_of_identifier 
    (Identifier.identifier_of_string "car"), ref (ValProcedure (ProcBuiltin
      (fun vlst env ->
        match vlst with 
        | [ValDatum (Cons (car, cdr))] -> ValDatum car
        | _ -> raise (Error "Invalid arguments for car"))))) in
  
  let e_cdr = Environment.add_binding e_car (Identifier.variable_of_identifier 
    (Identifier.identifier_of_string "cdr"), ref (ValProcedure (ProcBuiltin
      (fun vlst env ->
        match vlst with 
        | [ValDatum (Cons (car, cdr))] -> ValDatum cdr
        | _ -> raise (Error "Invalid arguments for cdr"))))) in
  
  let e_cons = Environment.add_binding e_cdr (Identifier.variable_of_identifier 
    (Identifier.identifier_of_string "cons"), ref (ValProcedure (ProcBuiltin
      (fun vlst env ->
        match vlst with 
        | [(ValDatum car);(ValDatum cdr)] -> ValDatum (Cons (car , cdr))
        | _ -> raise (Error "Invalid arguments for cons"))))) in
  
  let e_plus = Environment.add_binding e_cons 
    (Identifier.variable_of_identifier 
    (Identifier.identifier_of_string "+"), ref (ValProcedure (ProcBuiltin
      (fun vlst env -> 
        match vlst with
        | [] -> raise (Error "+ expects atleast 1 argument")
        | lst -> ValDatum (Atom (Integer (List.fold_left (fun acc x -> 
          match x with
          | ValDatum (Atom (Integer i)) -> i + acc
          | _ -> raise (Error "+ expects all integer arguments" )) 0 lst))))))) 
    in

  let e_mult = Environment.add_binding e_plus 
    (Identifier.variable_of_identifier 
    (Identifier.identifier_of_string "*"), ref (ValProcedure (ProcBuiltin
      (fun vlst env -> 
        match vlst with
        | [] -> raise (Error "* expects atleast 1 argument")
        | lst -> ValDatum (Atom (Integer (List.fold_left (fun acc x -> 
          match x with
          | ValDatum (Atom (Integer i)) -> i + acc
          | _ -> raise (Error "* expects all integer arguments")) 1 lst)))))))
    in

  let e_equal = Environment.add_binding e_mult 
    (Identifier.variable_of_identifier 
    (Identifier.identifier_of_string "equal?"), ref (ValProcedure (ProcBuiltin
      (fun vlst env -> 
        match vlst with
        | v1::v2::[] -> ValDatum (Atom (Boolean (v1=v2)))
        | _ -> raise (Error "equal? expects exactly 2 arguments"))))) in

  let e_eval = Environment.add_binding e_equal 
    (Identifier.variable_of_identifier 
    (Identifier.identifier_of_string "eval"), ref (ValProcedure (ProcBuiltin
      (fun vlst env -> 
        match vlst with
        | [] -> raise (Error "eval expects exactly 1 argument")
        | v1::v2::t -> raise (Error "eval expects exactly 1 argument")
        | (ValDatum datum)::[] -> eval (read_expression datum) env 
        | _ -> raise (Error "Internal error in e_eval"))))) in

    e_eval


(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =

(* Returns all duplicates in a given list. Returns [] for a lst with no dupes.*)
let dupes_in_lst (lst : 'a list) : 'a list =
  List.fold_left (fun acc x -> if List.mem x lst then x::acc else acc) [] lst 
in

let procCall_helper (e1 : expression) (elst : expression list) 
  (env : environment) : value =
  match (eval e1 env) with 
  | ValProcedure (ProcLambda (vlst , env, e1lst)) 
    (* Checks # vars = # expressions and first exp not empty.*)
    when (List.length vlst)=(List.length elst) && e1lst <> []
      -> 
      let env' = List.fold_left2 
      (* Add bindings one at a time to the environment.*)
      (fun acc v e -> Environment.add_binding acc (v, ref (eval e env))) 
      env vlst elst in
      (* Evaluate e1lst in the new environment one at a time.*)  
      List.fold_left (fun acc e -> eval e acc) (value (ValDatum Nil)) e1lst 
  | ValProcedure (ProcLambda (vlst , env, e1lst)) -> raise (Error "Invalid
    procedure call, error with ProcLambda construction.") 
  | ValProcedure (ProcBuiltin f) -> f (List.map (fun x -> eval x env) elst) env   
  | _ -> raise Error
in

let let_helper (lblst : let_binding list) (elst : expression list) : value =
  if dupes_in_lst lblst <> [] 
  then raise (Error "Duplicate variable names in let exp")
  else if elst = [] then raise (Error "let binding requires at least 1 expr") 
  else (* Add bindings simultaneously to the environment.*)
    List.fold_left 
    (fun acc b -> let (v , e) = b in Environment.add_binding env b) 
    env vlst lblst in
    (* Evaluate body expressions in new environment right to left.*)
    List.fold_left (fun acc x -> eval x env) (value (ValDatum Nil)) elst
in


  match expression with
  | ExprSelfEvaluating SEBoolean b -> ValDatum (Atom (Boolean b))
  | ExprSelfEvaluating SEInteger i -> ValDatum (Atom (Integer i))
  | ExprVariable v -> if Environment.is_bound env v then 
      !(Environment.get_binding env v) else raise (Error "Variable not bound")
  | ExprQuote d -> ValDatum d
  | ExprLambda (vl , el) -> ValProcedure (ProcLambda (vl,env,el))
  | ExprProcCall (e1 , elst) ->
     procCall_helper e1 elst env

  | ExprIf (e1 , e2 , e3) -> 
      (match (eval e1 env) with
      | ValDatum (Atom (Boolean false)) -> eval e3 env
      | _ -> eval e2 env) 

  | ExprAssignment (v , e) -> if Environment.is_bound env v then 
      (Environment.get_binding env v) := (eval e env) 
      else raise (Error "Invalid assignment")

  | ExprLet (lblst , elst) -> let_helper lblst elst

  | ExprLetStar (_, _)

  | ExprLetRec (_, _)     ->
     failwith "Ahahaha!  That is classic Rower."

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (_, _)     ->
     failwith "I couldn't have done it without the Rower!"

let rec string_of_value value =
  let rec string_of_datum datum =
    match datum with
    | Atom (Boolean b) -> if b then "#t" else "#f"
    | Atom (Integer n) -> string_of_int n
    | Atom (Identifier id) -> Identifier.string_of_identifier id
    | Nil -> "()"
    | Cons (car, cdr) -> string_of_cons car cdr

  and string_of_cons car cdr =
    let rec strings_of_cons cdr =
      match cdr with
      | Nil -> []
      | Cons (car, cdr) -> (string_of_datum car) :: (strings_of_cons cdr)
      | _ -> ["."; string_of_datum cdr;] in
    let string_list = (string_of_datum car) :: (strings_of_cons cdr) in
    "(" ^ (String.concat " " string_list) ^ ")" in
  
  match value with
  | ValDatum (datum) -> string_of_datum datum
  | ValProcedure (ProcBuiltin p) -> "#<builtin>"
  | ValProcedure (ProcLambda (_, _, _)) -> "#<lambda>"
