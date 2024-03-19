(* This is the main code of the interpreter *)
open Printf
open Ast

let main =
	let lexbuf = Lexing.from_channel stdin in 
	let res =
	try
		Parser.main Lexer.token lexbuf
	with
		| Lexer.Error(c) ->
			fprintf stderr "Lexical char error at line %d: Unknown token '%c'\n" 
				lexbuf.lex_curr_p.pos_lnum c;
			exit 1
		| Lexer.ErrorStr(str) ->
			fprintf stderr "Lexical string error at line %d: Unknown token '%s'\n" 
				lexbuf.lex_curr_p.pos_lnum str;
			exit 1
		| Parser.Error ->
			fprintf stderr "Parsing error at line %d" lexbuf.lex_curr_p.pos_lnum;
			exit 1
	in
	let environment : Ast.iot_env = ([], []) in
	let set_env = Ast.frst_eval_IotStructure environment res in
	(*Printf.printf "%s\n" (pprint_IoTStruct res)*)
	Ast.recursive_eval_IoTStructure set_env res;
