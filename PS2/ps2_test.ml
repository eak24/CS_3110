open Assertions
open Ps2

(*Tests the count_ops function*)
TEST_UNIT "count_ops" =
	let _ = assert_true (0 = count_ops(Val 2)) in
	let _ = assert_true (1 = count_ops(Binop ((+), Val 2, Val 3))) in
	let _ = assert_true (3 = count_ops(Binop ((+), Val 3, Unop ((~-), Binop ((/), Val 5, Val 2))))) in
()

(*
WCPat
VarPat "hi"
UnitPat
ConstPat 1
TuplePat [WCPat,WCPat]
StructorPat ("Fred", VarPat "hi")
ConstVal 1
UnitVal
TupleVal [UnitVal,UnitVal]
StructorVal ("Bob", UnitVal)
*)
(*Tests the count_ops function*)
TEST_UNIT "count_wcs" =
	let _ = assert_true (1 = count_wcs WCPat) in
	let _ = assert_true (2 = count_wcs (TuplePat [WCPat;WCPat])) in
	let _ = assert_true (1 = count_wcs (StructorPat ("Fred", Some WCPat))) in
()

(*Tests the count_wcs_and_var_lengths function*)
TEST_UNIT "count_wcs_and_var_lengths" =
	let _ = assert_true (1 = count_wcs_and_var_lengths WCPat) in
	let _ = assert_true (2 = count_wcs_and_var_lengths 
		(TuplePat [WCPat;WCPat])) in
	let _ = assert_true (4 = count_wcs_and_var_lengths (StructorPat ("Fred", 
		Some (VarPat "Bill")))) in
()

(*Tests the count_var function*)
TEST_UNIT "count_var" =
	let _ = assert_true (0 = count_var "hi" (WCPat)) in
	let _ = assert_true (0 = count_var "hi" (TuplePat [WCPat;WCPat])) in
	let _ = assert_true (1 = count_var "Bill" (StructorPat ("Fred", Some (VarPat 
		"Bill"))))in
()

(*Tests the extract_names function*)
TEST_UNIT "extract_names" =
	let _ = assert_true ([] = extract_names (WCPat)) in
	let _ = assert_true ([] = extract_names (TuplePat [WCPat;WCPat])) in
	let _ = assert_true (["Fred";"Bill"] = extract_names (StructorPat ("Fred", 
		Some (VarPat "Bill"))))in
()

(*Tests the has_dups function*)
TEST_UNIT "has_dups" =
	let _ = assert_true (false = has_dups []) in
	let _ = assert_true (false = has_dups [WCPat]) in
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
	let _ = assert_true (Some [1; 2; 3; 1; 2; 1] = all_answers (fun x -> 
		Some (countup x)) [1;2;3]) in
	let _ = assert_true (None = all_answers (fun x -> None) [1;2;3]) in
	let _ = assert_true (None = all_answers (fun x -> if x<2 then Some [1;2;3] 
		else None) [1;2;3]) in
()

let () = Pa_ounit_lib.Runtime.summarize ()
