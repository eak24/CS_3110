open Assertions
open Ps2

TEST_UNIT "count_ops" =
    let _ = assert_true (0 = count_ops(Val 2)) in
	let _ = assert_true (1 = count_ops(Binop ((+), Val 2, Val 3))) in
	let _ = assert_true (3 = count_ops(Binop ((+), Val 3, Unop ((~-), Binop ((/), Val 5, Val 2))))) in
()

(*I also test make_fact_tree by calling eval on it below.*)
TEST_UNIT "make_fact_tree" =
	let _ = assert_true(Val 1 = make_fact_tree(0)) in 
	let _ = assert_true(Val 1 = make_fact_tree(1)) in
	(* let _ = assert_true(Binop (( * ), Val 2, Val 1)  = make_fact_tree(2)) in*
	 * We tested the rest of the function calls in utop, and got convincing *
	 * results.  Here are some:
	 * utop # make_fact_tree(3);;
     * - : int exprTree = Binop (<fun>, Val 3, Binop (<fun>, Val 2, Val 1))
     * utop # make_fact_tree(2);;
     * - : int exprTree = Binop (<fun>, Val 2, Val 1) *)   

	()

(*This function helps with testing eval,
 * Returns n+n-1+...+1, with eval(0)=1*)
let rec make_add_tree(n:int) : int exprTree = 
  if n=0||n=1 then Val 1 else
  Binop (( + ), Val n, make_fact_tree(n-1))

TEST_UNIT "eval" =
	let _ = assert_true(1 = eval(make_fact_tree(0))) in 
	let _ = assert_true(1 = eval(make_fact_tree(1))) in
	let _ = assert_true(2 = eval(make_fact_tree(2))) in
	let _ = assert_true(6 = eval(make_fact_tree(3))) in
	let _ = assert_true(24 = eval(make_fact_tree(4))) in
	(*Made the make_add_tree to help with testing, see ps2.ml*)
	let _ = assert_true(1 = eval(make_add_tree(0))) in 
	let _ = assert_true(3 = eval(make_add_tree(2))) in
	let _ = assert_true(10 = eval(make_add_tree(4))) in
	(*We tested, rasing an division by zero exception in utop

	utop # let x33=(Binop((/), Val 1, Val 0));;
	val x33 : int exprTree = Binop (<fun>, Val 1, Val 0)                           
	 ─( 23:13:52 )─< command 36 >─────────────────────────────────────{ counter: 0 }─utop # eval(x33);;
	Exception: Division_by_zero. 
	*)
	()


TEST_UNIT "product" = 
    let _ = assert_true(product [777.5; 4.] = 3110.)(*given*) in
    let _ = assert_true(product [4.; 777.5] = 3110.)(*switch order*) in
    let _ = assert_true(product [] = 1.) (*given*) in
    let _ = assert_true(product [0.0;4.;1247.4] = 0.) (*zero*) in
    let _ = assert_true(product [-1.]= (-1.)) (*negative*) in
    () 

TEST_UNIT "concat_left" = 
    assert_true(concat_left[]="");(**)
    assert_true(concat_left[""]="");(**)
    assert_true(concat_left["";""]="");(**)
    assert_true(concat_left["cs";"3110"]="cs3110");(**)
    assert_true(concat_left["3110";"cs"]="3110cs");(**)
    assert_true(concat_left["3110";"cs";"uiop"]="3110csuiop");(**)
    assert_true(concat_left["";"31";"";"10";""] = "3110");

TEST_UNIT "concat_right" =
    assert_true(concat_right[]="");(**)
    assert_true(concat_right[""]="");(**)
    assert_true(concat_right["";""]="");(**)
    assert_true(concat_right["cs";"3110"]="cs3110");(**)
    assert_true(concat_right["3110";"cs"]="3110cs");(**)
    assert_true(concat_right["3110";"cs";"uiop"]="3110csuiop");(**)
    assert_true(concat_right ["";"31";"";"10";""] = "3110");

TEST_UNIT "mapi_list: PS2 P2 EX 3" = 
    assert_true(mapi_lst (+) [3;0;-1;-3] = [3;1;1;0]);
    assert_true(mapi_lst (+) [0;0;0] = [0;1;2]);
    assert_true(mapi_lst ( * ) [3;0;-1;-3] = [0;0;-2;-9]);
    assert_true(mapi_lst (+) [] = []);
    assert_true(mapi_lst ( - ) [0] = [0]);
    assert_true(mapi_lst ( - ) [0;0;0] = [0;1;2]);

TEST_UNIT "outline" = 
    assert_true(outline [] = []);
    assert_true(outline["one";"";"three";""]=["1. one"; "2. "; "3. three"; "4. "] )

TEST_UNIT "scan_left" =
    assert_true(scan_left (+) 0 [1;2;3] = [0;1;3;6]);
    assert_true(scan_left (^) ""  ["";"m"] = ["";"";"m"]);

TEST_UNIT "scan_right" = 
   assert_true(scan_right (+) 0 [1;2;3] = [0;3;5;6]);
   assert_true(scan_right (^) "swag" ["zar";"doz"] = ["swag";"dozswag";"zardozswag"]);

 TEST_UNIT "show" =
   assert_true(show [] = ());

TEST_UNIT "insert_col" =
    assert_true(insert_col [[1;2;3];[4;5;6]] [6;7] = [[1;2;3;6];[4;5;6;7]]);

(*Tests the count_ops function*)
TEST_UNIT "count_ops" =
	let _ = assert_true (0 = count_ops(Val 2)) in
	let _ = assert_true (1 = count_ops(Binop ((+), Val 2, Val 3))) in
	let _ = assert_true (3 = count_ops(Binop ((+), Val 3, Unop 
		((~-), Binop ((/), Val 5, Val 2))))) in
()

(*Tests the count_wcs function*)
TEST_UNIT "count_wcs" =
	let _ = assert_true (1 = count_wcs WCPat) in
	let _ = assert_true (0 = count_wcs (VarPat "howdy")) in
	let _ = assert_true (2 = count_wcs (TuplePat [WCPat;WCPat])) in
	let _ = assert_true (1 = count_wcs (StructorPat ("Fred", Some WCPat))) in
()

(*Tests the count_wcs_and_var_lengths function*)
TEST_UNIT "count_wcs_and_var_lengths" =
	let _ = assert_true (1 = count_wcs_and_var_lengths WCPat) in
	let _ = assert_true (5 = count_wcs_and_var_lengths (VarPat "howdy")) in
	let _ = assert_true (2 = count_wcs_and_var_lengths 
		(TuplePat [WCPat;WCPat])) in
	let _ = assert_true (4 = count_wcs_and_var_lengths (StructorPat ("Fred", 
		Some (VarPat "Bill")))) in
()

(*Tests the count_var function*)
TEST_UNIT "count_var" =
	let _ = assert_true (0 = count_var "hi" (WCPat)) in
	let _ = assert_true (0 = count_var "hi" (VarPat "howdy")) in
	let _ = assert_true (0 = count_var "hi" (TuplePat [WCPat;WCPat])) in
	let _ = assert_true (0 = count_var "Steve" (StructorPat 
		("Fred", Some (VarPat "Bill"))))in
	let _ = assert_true (0 = count_var "hi" (VarPat "howdy")) in
	let _ = assert_true (2 = count_var "howdy" 
		(TuplePat [VarPat "howdy";VarPat "howdy"])) in
	let _ = assert_true (1 = count_var "Steve" (StructorPat 
		("Steve", Some (VarPat "Steve")))) in
	let _ = assert_true (1 = count_var "Steve" (StructorPat 
		("Steve", Some (StructorPat ("Steve", Some (VarPat "Steve")))))) in
()

(*Tests the extract_names function*)
TEST_UNIT "extract_names" =
	let _ = assert_true ([] = extract_names (WCPat)) in
	let _ = assert_true (["howdy"] = extract_names (VarPat "howdy")) in
	let _ = assert_true ([] = extract_names (TuplePat [WCPat;WCPat])) in
	let _ = assert_true (["howdy";"howdy"] = 
		extract_names (TuplePat [(VarPat "howdy");(VarPat "howdy")])) in
	let _ = assert_true (["Fred";"Bill"] = extract_names (StructorPat ("Fred", 
		Some (VarPat "Bill"))))in
()

(*Tests the has_dups function*)
TEST_UNIT "has_dups" =
	let _ = assert_true (false = has_dups []) in
	let _ = assert_true (false = has_dups [WCPat]) in
	let _ = assert_true (false = has_dups ["hi";"hello";"howdy"])in
	let _ = assert_true (true = has_dups [WCPat;WCPat]) in
	let _ = assert_true (true = has_dups ["hi";"hi"]) in
	let _ = assert_true (true = has_dups ["hi";"hello";"hi"])in
()

(*Tests the all_vars_unique function*)
TEST_UNIT "all_vars_unique" =
	let _ = assert_true (true = all_vars_unique (WCPat)) in
	let _ = assert_true (true = all_vars_unique (TuplePat [WCPat;WCPat])) in
	let _ = assert_true (true = all_vars_unique (StructorPat ("Fred", 
		Some (VarPat "Bill"))))in
	let _ = assert_true (false = all_vars_unique (StructorPat ("Fred", 
		Some (VarPat "Fred"))))in
()

(*Tests the all_answers function*)
TEST_UNIT "all_answers" =
	let _ = assert_true (Some [1; 2; 3; 1; 2; 1] = all_answers (fun x -> 
		Some (countup x)) [1;2;3]) in
	let _ = assert_true (None = all_answers (fun x -> None) [1;2;3]) in
	let _ = assert_true (None = all_answers (fun x -> if x<2 then Some [1;2;3] 
		else None) [1;2;3]) in
()

(*Tests the match_pat function*)
TEST_UNIT "match_pat" =
	let _ = assert_true (Some [] = match_pat (UnitVal,WCPat)) in
	let _ = assert_true (Some [] = 
		match_pat (StructorVal ("Jane",Some (ConstVal 5)), 
		StructorPat ("Jane",Some (ConstPat 5)))) in
()

let () = Pa_ounit_lib.Runtime.summarize ()
