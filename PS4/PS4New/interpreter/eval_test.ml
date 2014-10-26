open Assertions
open Ast
open Eval
open Identifier

exception Err

let dat_list_to_dat (xs: datum list) : datum =
	match xs with 
		| h::t -> h
		| [] -> raise Err

let parse (s : string) : Ast.datum list =
  Parser.main Lexer.token (Lexing.from_string s)

let siv (arg: string) : variable =
  variable_of_identifier (identifier_of_string arg)

let sikw (arg: string) : identifier =
	let () = assert (is_keyword (identifier_of_string arg)) in 
  	(identifier_of_string arg)

let pd (s:string) : datum =
	dat_list_to_dat (parse s)

(* Next 10 TEST_UNITS for read_expression *)

TEST_UNIT "stand alone nil raises error" =
	let nil_datum = Nil in 
	let npl = parse "()" in
	let np = dat_list_to_dat npl in
	assert_raises (Some (Error "Nil matched in read_expression")) 
		read_expression nil_datum;
	assert_raises (Some (Error "Nil matched in read_expression")) 
		read_expression np;
	()

TEST_UNIT " Single Atoms" =
	let bool_atom = Atom (Boolean true) in
	let int_atom = Atom (Integer 42) in
	let id_var_atom = Atom (Identifier (identifier_of_string "x")) in
	let bal = parse "#t" in
	let ba = dat_list_to_dat bal in
	let ial = parse "42" in
	let ia = dat_list_to_dat ial in
	let ival = parse "x" in
	let iva = dat_list_to_dat ival in
	(*let kwal = parse "let" in
	let kwa = dat_list_to_dat kwal in *)
	assert_true ((read_expression bool_atom)=(ExprSelfEvaluating (SEBoolean true)));
	assert_true ((read_expression ba)=(ExprSelfEvaluating (SEBoolean true)));
	assert_true ((read_expression int_atom)=(ExprSelfEvaluating (SEInteger 42)));
	assert_true ((read_expression ia)=(ExprSelfEvaluating (SEInteger 42)));
	assert_true ((read_expression id_var_atom)=(ExprVariable (siv "x")));
	assert_true ((read_expression iva)=(ExprVariable (siv "x")));
	(*assert_raises (Some (Error "Nil matched in read_expression")) 
		read_expression np; *)
	()

TEST_UNIT "if" =
	let ifi = pd "(if #t 1 0)" in
	let ifii = pd "(if #t (if #t 100 10) 0)" in
	let ifiisub = (ExprIf (ExprSelfEvaluating (SEBoolean true), 
		ExprSelfEvaluating (SEInteger 100), ExprSelfEvaluating (SEInteger 10))) in
	let ifiii = pd "(if #f 0 (if #t (if #t 4 3) 10))" in
	let ifiiisubsub = (ExprIf ( ExprSelfEvaluating (SEBoolean true), 
		ExprSelfEvaluating (SEInteger 4), ExprSelfEvaluating (SEInteger 3))) in
	let ifiiisub = (ExprIf ( ExprSelfEvaluating (SEBoolean true), ifiiisubsub, 
		ExprSelfEvaluating (SEInteger 10))) in

	assert_true((read_expression ifi)=
		(ExprIf ( ExprSelfEvaluating (SEBoolean true), 
			ExprSelfEvaluating (SEInteger 1), ExprSelfEvaluating (SEInteger 0))) );

	assert_true((read_expression ifii)=
		(ExprIf ( ExprSelfEvaluating (SEBoolean true), ifiisub, 
			ExprSelfEvaluating (SEInteger 0))) );

	assert_true((read_expression ifiii)=
		(ExprIf ( ExprSelfEvaluating (SEBoolean false), 
			ExprSelfEvaluating (SEInteger 0), ifiiisub)) );
	()

TEST_UNIT " Quote " =
	let nilq = pd "(quote ())" in
	(*let nildat = pd "()" in *)
	let ifq = pd "(quote (if #t 1 0))" in
	let ifdat = pd "(if #t 1 0)" in 
	assert_true((read_expression nilq)= (ExprQuote (Cons(Nil, Nil))));
	assert_true((read_expression ifq)= (ExprQuote (Cons(ifdat, Nil))));
	()

TEST_UNIT " Lambda " =
	let lam = pd "(lambda (x) x)" in 
	assert_true((read_expression lam)= ExprLambda ([(siv "x")], 
		[ExprVariable (siv "x")]));
	let lam2 = pd "(lambda (x y) y x)" in
	assert_true((read_expression lam2)= ExprLambda ([(siv "x"); (siv "y")],
		[ExprVariable (siv "y"); ExprVariable (siv "x")])  );
	()

TEST_UNIT " Set! " =
	let sbif = pd "(set! x (if #t 1 0))" in
	let subif = read_expression (pd "(if #t 1 0)") in
	let sbif_expr = ExprAssignment ((siv "x"), subif) in
	assert_true((read_expression sbif)= sbif_expr);
	(* This raises an error like it's supposed to, but I'm not sure how to 
	 * catch it here with an assert_raises test case
	let sb_wrong= pd "(set! if (if #t 1 0))" in
	assert_raises (Some (Assert_failure ("", 1, 0))) read_expression sb_wrong;
	*)
	()

TEST_UNIT " Let " =
	let x_var = ExprVariable (siv "x") in
	let y_var = ExprVariable (siv "y") in
	let x0 = (siv "x"), ExprSelfEvaluating (SEInteger 0) in
	let y1 = (siv "y"), ExprSelfEvaluating (SEInteger 1) in
	let re1 = read_expression (pd "(let ((x 0)) x)") in
	let re2 = read_expression (pd "(let ((x 0) (y 1)) x)") in
	let re3 = read_expression (pd "(let ((x 0) (y 1)) x y)") in
	assert_true (re1= ExprLet ([x0], [x_var]));
	assert_true (re2= ExprLet ([x0;y1], [x_var]));
	assert_true (re3= ExprLet ([x0;y1], [x_var;y_var]));
	()

TEST_UNIT " Let* " =
	let x_var = ExprVariable (siv "x") in
	let y_var = ExprVariable (siv "y") in
	let x0 = (siv "x"), ExprSelfEvaluating (SEInteger 0) in
	let y1 = (siv "y"), ExprSelfEvaluating (SEInteger 1) in
	let re1 = read_expression (pd "(let* ((x 0)) x)") in
	let re2 = read_expression (pd "(let* ((x 0) (y 1)) x)") in
	let re3 = read_expression (pd "(let* ((x 0) (y 1)) x y)") in
	assert_true (re1= ExprLetStar ([x0], [x_var]));
	assert_true (re2= ExprLetStar ([x0;y1], [x_var]));
	assert_true (re3= ExprLetStar ([x0;y1], [x_var;y_var]));
	()

TEST_UNIT " Letrec " =
	let x_var = ExprVariable (siv "x") in
	let y_var = ExprVariable (siv "y") in
	let x0 = (siv "x"), ExprSelfEvaluating (SEInteger 0) in
	let y1 = (siv "y"), ExprSelfEvaluating (SEInteger 1) in
	let re1 = read_expression (pd "(Letrec ((x 0)) x)") in
	let re2 = read_expression (pd "(letrec ((x 0) (y 1)) x)") in
	let re3 = read_expression (pd "(letrec ((x 0) (y 1)) x y)") in
	assert_true (re1= ExprLetRec ([x0], [x_var]));
	assert_true (re2= ExprLetRec ([x0;y1], [x_var]));
	assert_true (re3= ExprLetRec ([x0;y1], [x_var;y_var]));
	()

TEST_UNIT "Procedure Call" =
	let r1 = read_expression (pd "(x 3 4)") in
	let r1a = ExprProcCall (ExprVariable (siv "x"), 
			[ExprSelfEvaluating (SEInteger 3);
			ExprSelfEvaluating (SEInteger 4)])  in
	assert_true (r1=r1a);
	()

(* Start Testing eval *)

TEST_UNIT " Booleans, Integers"
	let t = read_expression (pd "#t")




let () = Pa_ounit_lib.Runtime.summarize () 