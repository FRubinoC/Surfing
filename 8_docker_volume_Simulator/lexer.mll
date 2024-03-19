(* This is the lexer part of the interpreter *)
{
	open Parser
	open String
	exception Error of char
	exception ErrorStr of string
}


let letter = ['A'-'Z'] | ['a'-'z']
let digit = ['0'-'9']
let non_digit = '_' | letter
let ident = non_digit (digit | non_digit)*
let line_comment = "//" [^ '\n']*

let acceptable_char = ['('] | [')'] | ['|'] | [':'] | ['='] | ['<'] | ['>'] | ['['] | [']']
						| ['{'] | ['}'] | ['-'] | ['?']
let operator = (acceptable_char)*

let word = ['"'] (non_digit | ' ')* ['"']
let processnames = ['P'] (digit)*
let sensorlocs = ['S'] (digit)*
let actuatorlocs = ['A'] (digit)*

rule token = parse
	  [' ' '\t'] | line_comment
	  	{ token lexbuf }
	| ['\n'] 
		{ Lexing.new_line lexbuf; token lexbuf }
	| word as str
		{
			let len = (String.length str)-2 in
			STRING(String.sub str 1 len)
		}
	| processnames as str
		{ PROCESSNAME(str) }
	| sensorlocs as str
		{ SENSORLOC(str) }
	| actuatorlocs as str
		{ ACTUATORLOC(str) }
	| ident as str
		{
			match str with
			| "uh"   -> OPENITER
			| "h"    -> CLOSEITER 
			| "NULL" -> INACTTOKEN
			| "true" -> BOOL(bool_of_string(Lexing.lexeme lexbuf))
			| "false"-> BOOL(bool_of_string(Lexing.lexeme lexbuf))
			| "tau"  -> INTERNALACT
			| "wait" -> WAIT
			| "release" -> RELEASE
			| "used" -> USED
			| "prepare" -> PREPARE
			| "encrypt" -> ENCRYPT
			| "decrypt" -> DECRYPT
			| "nodes_declaration" -> NODESDECLARATION
			| "nodes_definition"  -> NODESDEFINITION
			| "functions_definition" -> FUNCTDEFINITION
			| s      -> IDENT(s)
		}
	| operator as str
		{
			match str with
			| "(|"   -> OPENACTINPUT
			| "|)"   -> CLOSEACTINPUT
			| ":="   -> ASSIGNMENT
			| ":["   -> OPENNODE
			| "]"    -> ENDNODE
			| "{{"   -> OPENCONDBODY
			| "}}"	 -> CLOSECONDBODY
			| "||"   -> PCOMPONENT
			| "|"    -> PARALNODE
			| "<<"   -> OPENOUTPUT
			| ">>->" -> CLOSEOUTPUT
			| "("    -> LPAREN
			| ")"    -> RPAREN
			| ">="   -> EQUALORGREATER
			| "<="   -> EQUALORLOWER
			| "<"    -> OPENACTOUTPUT
			| ">"    -> CLOSEACTOUTPUT
			| "=="   -> DOUBLEEQUAL
			| "="    -> EQUAL
			| "{"    -> LROUND
			| "}"    -> RROUND
			| "?"    -> QUESTION
			| ":"    -> COLON
			| "-"	 -> SUB
			| _ as str -> raise (ErrorStr str)
		}
	| ['0'-'9']+ 
		{ INT(int_of_string(Lexing.lexeme lexbuf)) }
	| [';'] 
		{ SEMICOLON }
	| [','] 
		{ COMMA }
	| ['.'] 
		{ DOT }
	| ['+'] 
		{ ADD }
	| ['/']
		{ DIV }
	| ['*']
		{ MUL }
	| eof 
		{ EOF }
	| _ as c 
		{ raise (Error c) }