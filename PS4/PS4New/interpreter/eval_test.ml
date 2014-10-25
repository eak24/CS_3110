open Assertions
open Ast
open Eval
include Repl


exception Err
let dat_list_to_dat (xs: datum list) : datum =
	match xs with 
		| h::t -> h
		| [] -> raise Err

let parse (s : string) : Ast.datum list =
  Parser.main Lexer.token (Lexing.from_string s)


let nil_datum = Nil
let npl = parse "()"
let np = dat_list_to_dat npl

(*
let nil_parse_list = Repl.parse "()"
let nil_parse = dat_list_to_dat nil_parse_list *)


TEST_UNIT "stand alone nil raises error" =
	assert_raises (Some (Error "Nil matched in read_expression")) 
		read_expression nil_datum;
	assert_raises (Some (Error "Nil matched in read_expression")) 
		read_expression np;
	()

TEST_UNIT "" =

	()



let () = Pa_ounit_lib.Runtime.summarize () 