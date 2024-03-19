(* This is the main code of the interpreter *)
open Printf
open Ast
open Static
open Sys

let main =
	(*
	let _ =
	try
		Sys.mkdir "./IoT-System" 777;
		Sys.mkdir "./IoT-System/nodes" 777
	with Sys_error msg -> Printf.printf "%s" msg
	in
	*)
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
	let mode =
	begin
		if (Sys.argv.(1) = "management") then 0
		else (int_of_string Sys.argv.(1))
	end in
	let environment : Ast.iot_env = ([], []) in
	let set_env = Ast.frst_eval_IotStructure environment res in
	let chan_set = Ast.channel_creation [] res in
	let _ = 
		begin 
			let path = "./IoT-System/ast.txt" in
			let oc = open_out path in
			Printf.fprintf oc "%s\n" (pprint_IoTStruct res);
			close_out oc;
		end in
	if (mode = 0) then 
		begin
			if (Static.static_analyzer set_env res) then
			Ast.eval_IoTStructure set_env chan_set res mode
			else failwith("Static analysis gone bad. Something bad found")
		end
	else Ast.eval_IoTStructure set_env chan_set res mode

	 
