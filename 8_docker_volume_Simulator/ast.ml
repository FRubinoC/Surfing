(* Abstract Syntax Tree *)
open Printf
open List
open Random
open Domainslib
open Unix
open Semaphore
open Binary
open Bytes
open Str

let wait_container_setting = 60;;

type encryption = 
	| Clear
	| Encrypted

type value =
	| None
	| Eint of int
	| Ebool of bool
	| Estring of string
	| Echar of char
	

type term =
	| ParallelTerms of term * term
	| Value of value
	| SensorLoc of string
	| Variable of string
	| Funct of string * term
	| Add of term * term
	| Sub of term * term
	| Mul of term * term
	| Div of term * term
	
	
type actuator = 
	| InactiveActuator
	| ActuatorIntAction of actuator
	| ActuatorCommand of term * actuator
	| ACloseIter
	| AOpenIter of actuator


type sensor =
	| InactiveSensor
	| SensorIntAction of sensor
	| SensorStore of string * int * int * sensor
	| SCloseIter
	| SOpenIter of sensor
	

type node_number =
	| NoNode
	| NodeNumber of int
	| ParallelNodeNumber of node_number * node_number


type condition = 
	| Bool of bool
	| Greater of term * term
	| Lower of term * term
	| EqGr of term * term
	| EqLw of term * term
	| Equal of term * term
	| And of condition * condition
	| Or of condition * condition


type process =
	| InactProcess
	| MultiOutput of  string * term * node_number * process
	| InputProc of int * string * term * process
	| ConditionProc of condition * process * process * process
	| Assignment of string * term * process
	| ActivateActuator of string * string * process
	| PCloseIter
	| POpenIter of process
	| Wait of string * process
	| Release of string * process
	| Used of string * process
	| Prepare of string * process
	| Encrypt of string * process
	| Decrypt of string * process


type component =
	| InactiveComponent
	| Process of string * process
	| Sensor of string * sensor
	| Actuator of string * actuator
	| ParallelComponent of component * component
	

type node =
	| InactNode
	| Node of int * component
	| ParallelNodes of node * node


type funct_definition =
	| InactFunction
	| ParallelFunctions of funct_definition * funct_definition
	| FunctionDefinition of string * term * term


type declaration =
	| ParallelDeclarations of declaration * declaration
	| SensorDeclaration of string
	| ActuatorDeclaration of string
	| VariableDeclaration of string
	

type node_declaration =
	|	ParallelNodesDeclaration of node_declaration * node_declaration
	| Store of int * declaration


type iot_structure =
	| IoTStructure of node_declaration * funct_definition * node


(* Exceptions *)
exception ErrReturnVariableValue of string
exception ErrNotExistentActuator of string
exception ErrNotExistentVar of string
exception ErrNotExistingStore of int
exception ErrActuatorOptionDef of string
exception ErrNonExistentFun of string
exception ErrMinHigherThanMaxSensor of string
exception ErrParallelTerrmsNotPermitted of (value list) * (value list) 
exception ErrPatternMismatch of string list
exception ErrOutputGoneBad of int
exception ErrNotExistentChannel of int * int * string
exception ErrBadInput of int * int
exception ErrAdd of value * value
exception ErrSub of value * value
exception ErrMul of value * value
exception ErrDiv of value * value
exception ErrAddList of (value list) * (value list)
exception ErrSubList of (value list) * (value list)
exception ErrMulList of (value list) * (value list)
exception ErrDivList of (value list) * (value list)
exception ErrDeserialization of string



(* Environmental structures *)
type store = 
{
	mutable variables   : (string * value * encryption) list;
	mutable sensors     : (string * value) list;
	mutable actuators   : (string * (string list)) list;
  mutable semaphores   : (string * Binary.t * Binary.t) list;
}

type fun_env = (string * (string list) * term) list
type nodes_env = (int * store) list
type iot_env = nodes_env * fun_env
let empty_env = []
(*let chan_env = int * int * string * Chan.t *)


(* ***************** AST PRINT FUNCTIONS ******************)

let pprint_value = function
	| None -> "None"
	| Eint(i) -> sprintf "Int(%i)" i
	| Ebool(b) -> sprintf "Bool(%b)" b
	| Estring(str) -> sprintf "String(%s)" str
	| Echar(ch) -> sprintf "Char(%c)" ch


let rec pprint_nodeNum = function
	| NoNode -> "NoNode"
	| NodeNumber(nnode) -> "NodeNumber(" ^ sprintf "%i" nnode ^ ")"
	| ParallelNodeNumber(nodea, nodeb) -> 
			"ParallelNodeNumber(" ^ pprint_nodeNum nodea ^ ", " ^ pprint_nodeNum nodeb ^ ")"


let rec pprint_term = function
	| ParallelTerms(term1, term2) -> "ParallelTerms(" ^ pprint_term term1 ^ ", " ^ pprint_term term2 ^ ")"
	| Value(value) -> "Value(" ^ pprint_value value ^ ")"
	| SensorLoc(sloc) -> sprintf "SensorLoc(%s)" sloc
	| Variable(var) -> sprintf "Variable(%s)" var
	| Funct(str, term) -> "Funct(" ^ sprintf "%s" str ^ ", " ^ pprint_term term ^ ")"
	| Add(terma, termb) -> "Add(" ^ pprint_term terma ^ ", " ^ pprint_term termb ^ ")"
	| Sub(terma, termb) -> "Sub(" ^ pprint_term terma ^ ", " ^ pprint_term termb ^ ")"
	| Mul(terma, termb) -> "Mul(" ^ pprint_term terma ^ ", " ^ pprint_term termb ^ ")"
	| Div(terma, termb) -> "Div(" ^ pprint_term terma ^ ", " ^ pprint_term termb ^ ")"


let rec pprint_condition = function
	| Bool(b) -> "Bool(" ^ sprintf "%b" b ^ ")"
	| Greater(a, b) ->
			"Greater(" ^ pprint_term a ^ ", " ^ pprint_term b ^ ")"
	| Lower(a, b) ->
			"Lower(" ^ pprint_term a ^ ", " ^ pprint_term b ^ ")"
	| EqGr(a, b) ->
			"EqGr(" ^ pprint_term a ^ ", " ^ pprint_term b ^ ")"
	| EqLw(a, b) ->
			"EqLw(" ^ pprint_term a ^ ", " ^ pprint_term b ^ ")" 
	| Equal(a, b) ->
			"Equal(" ^ pprint_term a ^ ", " ^ pprint_term b ^ ")"
	| And(a, b) ->
			"And(" ^ pprint_condition a ^ ", " ^ pprint_condition b ^ ")"
	| Or(a, b) ->
			"Or(" ^ pprint_condition a ^ ", " ^ pprint_condition b ^ ")"
	
let rec pprint_process = function
	| InactProcess -> "InactProcess"
	| MultiOutput(const, terms, nodn, proc) ->
			"\n\t\t\tMultiOutput(" ^ sprintf "%s" const ^ ", " ^ pprint_term terms ^ ", " ^ pprint_nodeNum nodn ^ ", " ^ pprint_process proc ^ ")"
	| InputProc(src_node, const, var, proc) ->
			"\n\t\t\tInputProc(" ^ sprintf "%i" src_node ^ ", " ^ sprintf "%s" const ^ ", " ^ pprint_term var ^ ", " ^ pprint_process proc ^ ")"
	| ConditionProc(cond, proca, procb, res_proc) ->
			"\n\t\t\tConditionProc(" ^ pprint_condition cond ^ ", " ^ pprint_process proca ^ ", " ^ pprint_process procb ^ ", " ^ pprint_process res_proc ^ ")"
	| Assignment(var, value, proc) ->
			"\n\t\t\tAssignment(" ^ sprintf "%s" var ^ ", " ^ pprint_term value ^ ", " ^ pprint_process proc ^ ")"
	|	ActivateActuator(actloc, action, proc) ->
			"\n\t\t\tActivateActuator(" ^ sprintf "%s" actloc ^ ", " ^ sprintf "%s" action ^ ", " ^ pprint_process proc ^ ")"
	| PCloseIter -> "\n\t\t\tPCloseIter"
	| POpenIter(proc) -> "\n\t\t\tPOpenIter(" ^ pprint_process proc ^ ")"
	| Wait(var, proc) -> "\n\t\t\tWait(" ^ sprintf "%s" var^ ", " ^ pprint_process proc ^ ")"
	| Release(var, proc) -> "\n\t\t\tRelease(" ^ sprintf "%s" var ^ ", " ^ pprint_process proc ^ ")"
	| Used(var, proc) -> "\n\t\t\tUsed(" ^ sprintf "%s" var ^ ", " ^ pprint_process proc ^ ")"
	| Prepare(var, proc) -> "\n\t\t\tPrepare(" ^ sprintf "%s" var ^ ", " ^ pprint_process proc ^ ")"
	| Encrypt(var, proc) -> "\n\t\t\tEncrypt(" ^ sprintf "%s" var ^ ", " ^ pprint_process proc ^ ")"
	| Decrypt(var, proc) -> "\n\t\t\tDecrypt(" ^ sprintf "%s" var ^ ", " ^ pprint_process proc ^ ")"


let rec pprint_sensor = function
	| InactiveSensor -> "\n\t\t\tInactiveSensor"
	| SensorIntAction(sen) -> "\n\t\t\tSensorIntAction(" ^ pprint_sensor sen ^ ")"
	| SensorStore(sloc, min, max, sen) 
		-> "\n\t\t\tSensorStore(" ^ sprintf "%s" sloc ^ ", " ^ sprintf "%i" min ^ ", " ^ sprintf "%i" max ^ ", " ^ pprint_sensor sen ^ ")"
	| SCloseIter -> "\n\t\t\tSCloseIter"
	| SOpenIter(sen) -> "\n\t\t\tSOpenIter(" ^ pprint_sensor sen ^ ")"


let rec pprint_actuator = function
	| InactiveActuator -> "\n\t\t\tInactiveActuator"
	| ActuatorIntAction(act) -> "\n\t\t\tActuatorIntAction(" ^ pprint_actuator act ^ ")"
	| ActuatorCommand(com, act) 
		-> "\n\t\t\tActuatorCommand(" ^ pprint_term com ^ ", " ^ pprint_actuator act ^ ")"  
	| ACloseIter -> "\n\t\t\tACloseIter"
	| AOpenIter(act) -> "\n\t\t\tAOpenIter(" ^ pprint_actuator act ^ ")"


let rec pprint_component = function
	| InactiveComponent -> "InactiveComponent"
	| Process(name, proc) ->
			"\n\t\tProcess(" ^ sprintf "%s" name ^ ", " ^ pprint_process proc ^ ")"
	| Sensor(loc, sen) ->
			"\n\t\tSensor(" ^ sprintf "%s" loc ^ ", " ^ pprint_sensor sen ^ ")"
	| Actuator(loc, act) ->
			"\n\t\tActuator(" ^ sprintf "%s" loc ^ ", "^ pprint_actuator act ^ ")"
	| ParallelComponent(compa, compb) ->
			"\n\t\tParallelComponent(" ^ pprint_component compa ^ ", \n\t\t\t\t" ^ pprint_component compb ^ ")"


let rec pprint_node = function
	| ParallelNodes(nodea, nodeb) -> 
			"\n\tParallelNodes(" ^ pprint_node nodea ^ ", " ^ pprint_node nodeb ^ ")"
	| Node(nnode, nodea) ->
			"\n\tNode(\n\t\t" ^ sprintf "%i" nnode ^ ", " ^ pprint_component nodea ^ ")"
	| InactNode -> "InactNode"

let rec pprint_functions_defintion = function
	| InactFunction -> "InactFunction"
	| ParallelFunctions(fun1, fun2) ->
			"\n\tParallelFunctions(" ^ pprint_functions_defintion fun1 ^ ", " ^ pprint_functions_defintion fun2 ^ ")" 
	| FunctionDefinition(name, args, body) ->
			"\n\tFunctionDefinition(" ^ sprintf "%s" name ^ ", " ^ pprint_term args ^ ", " ^ pprint_term body ^ ")"


let rec pprint_declaration = function
	| ParallelDeclarations(decla, declb) ->
			"\n\t\tParallelDeclarations(" ^ pprint_declaration decla ^ ", " ^ pprint_declaration declb ^ ")"
	|	SensorDeclaration(sen) ->
			sprintf "\n\t\tSensorDeclaration(%s)" sen
	| ActuatorDeclaration(act) ->
			sprintf "\n\t\tActuatorDeclaration(%s)" act
	| VariableDeclaration(var) ->
			sprintf "\n\t\tVariableDeclaration(%s)" var 


let rec pprint_node_declaration = function
	| ParallelNodesDeclaration(nodea, nodeb) ->
			"\n\tParallelNodesDeclaration(" ^ pprint_node_declaration nodea ^ ", " ^ pprint_node_declaration nodeb ^ ")"
	| Store(nnode, decl) ->
			"\n\tStore(\n\t\t" ^ sprintf "%i" nnode ^ ", " ^ pprint_declaration decl ^ ")"
	
			
let rec pprint_IoTStruct = function
	| IoTStructure(decl, func, def) ->
			"IoTStructure(" ^ pprint_node_declaration decl ^ ", " ^ pprint_functions_defintion func ^ ", " ^ pprint_node def ^ ")"





(* ************* ENVIRONMENT PRINT FUNCTIONS ******************** *)
let pprint_encryption enc : string =
	match enc with
	| Clear -> "clear"
	| Encrypted -> "encrypted"


let rec pprint_node_variables (var_store : (string * value * encryption) list) : string =
	match var_store with
	| [] -> ""
	| (name, value, enc)::res_store ->
			"(" ^ name ^ " : " ^ pprint_value value ^ ", " ^ pprint_encryption enc ^ "); " ^ pprint_node_variables res_store


let rec pprint_node_sensors (sen_store : (string * value) list) : string =
	match sen_store with
	| [] -> ""
	| (name, value)::res_store ->
			"(" ^ name ^ " : " ^ pprint_value value ^ "); " ^ pprint_node_sensors res_store


let rec pprint_options (options : string list) : string =
	match options with
	| [] -> ""
	| ele::res_ele -> ele ^ if (res_ele = []) then ""
													else ", " ^ pprint_options res_ele


let rec pprint_node_actuators (act_store : (string * (string list)) list) : string =
	match act_store with
	| [] -> ""
	| (name, value_lst):: res_store ->
			name ^ " : (" ^ pprint_options value_lst ^ "); " ^ pprint_node_actuators res_store


let rec pprint_input_list (inp_lst : value list) : string =
	match inp_lst with
	| [] -> ""
	| ele::res_ele -> pprint_value ele ^ if (res_ele = []) then ""
																			 else ", " ^ pprint_input_list res_ele


let rec pprint_node_next_inputs (ninp_store : (int * (value list)) list) : string =
	match ninp_store with
	| [] -> ""
	| (origin, ninp_list)::res_store ->
			string_of_int origin ^ ": (" ^ pprint_input_list ninp_list ^ ");" ^ pprint_node_next_inputs res_store


let rec pprint_node_store (store : store) : string =
	"\n\tVariables -> " ^ pprint_node_variables store.variables ^ 
	";\n\tSensors -> " ^ pprint_node_sensors store.sensors ^
	";\n\tActuators -> " ^ pprint_node_actuators store.actuators ^ ";\n"


let rec pprint_function_env (environment : fun_env) : string =
	match environment with
	| [] -> ""
	| (funname, args, body)::res_env ->
			sprintf "%s" funname ^ if res_env = [] then ""
														 else ", " ^ pprint_function_env res_env


let pprint_channels channels =
	match channels with
	| (src, dst, _)::res_channels ->
		(sprintf "(%i, %i)" src dst) ^ if res_channels = [] then ";"
																	 else ", "
	| [] -> ""   


let rec pprint_proc_status (name : string) (nnode : int) (node_store : store) =
	begin
		let path = "./IoT-System/nodes/" ^ (string_of_int nnode) ^ "/" ^ name ^ ".txt" in
		let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
		begin
			Printf.fprintf oc "\n\n************************************\nNode number %i environment" nnode;
			Printf.fprintf oc "%s" (pprint_node_store node_store);
			(*Printf.printf "\n\nChannels:\n";
			Printf.printf "%s" (pprint_channels channels);*)
			Printf.fprintf oc "\n************************************\n\n\n\n\n";
			close_out oc;
			Unix.sleep 1;
		end
	end


let rec pprint_node_status (nnode : int) (node_store : store) (channels) =
	begin
		let path = "./IoT-System/nodes/" ^ (string_of_int nnode) ^ ".txt" in
		let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
		begin
			Printf.fprintf oc "\n\n************************************\nNode number %i environment" nnode;
			Printf.fprintf oc "%s" (pprint_node_store node_store);
			(*Printf.printf "\n\nChannels:\n";
			Printf.printf "%s" (pprint_channels channels);*)
			Printf.fprintf oc "\n************************************\n\n\n\n\n";
			close_out oc;
			Unix.sleep 1;
		end
	end


(* ************  CHANNEL CREATION **************)
let rec check_channel_existance (src : int) (const : string) (des : int) (chan_env) : bool =
	match chan_env with 
	| [] -> false 
	| (ch_src, ch_des, ch_const, ch_ip, ch_port)::res_chan_env -> 
			if ((src = ch_src) && (des = ch_des) && (ch_const = const)) then true
			else check_channel_existance src const des res_chan_env


let rec take_new_port (des_node : int) (max_port : int) (chan_env) : int =
	match chan_env with
	| (_, des, _, _, port)::res_chan_env ->
			if (des = des_node)&&(port > max_port) then take_new_port des_node port res_chan_env
			else take_new_port des_node max_port res_chan_env
	| [] -> max_port+1


let rec output_channel_insertion (nnode : int) (const : string) (des : node_number) (chan_env) =
	match des with
	| NoNode -> chan_env
	| ParallelNodeNumber(nn1, nn2) ->
			let new_chan_env = output_channel_insertion nnode const nn1 chan_env in
			output_channel_insertion nnode const nn2 new_chan_env
	| NodeNumber(des_node) ->
			if (check_channel_existance nnode const des_node chan_env) then chan_env
			else 
				begin
					let ip_addr = "172.28.1." ^ (string_of_int des_node) in
					let port = take_new_port des_node 5000 chan_env in
					(nnode, des_node, const, ip_addr, port)::chan_env
				end


let rec input_channel_insertion (src : int) (const : string) (des : int) (chan_env) =
	begin
		if (check_channel_existance src const des chan_env) then chan_env
		else 
			begin
				let ip_addr = "172.28.1." ^ (string_of_int des) in
				let port = take_new_port des 5000 chan_env in
				(src, des, const, ip_addr, port)::chan_env
			end
	end


let rec channel_creation_processes (nnode : int) (chan_env) (ast : process) =
	match ast with
	| InactProcess -> chan_env
	| MultiOutput(const, outputs, des, res_proc) ->
			let new_chan_env = output_channel_insertion nnode const des chan_env in
			channel_creation_processes nnode new_chan_env res_proc
	| InputProc(src_node, const, _, res_proc) ->
			let new_chan_env = input_channel_insertion src_node const nnode chan_env in
			channel_creation_processes nnode new_chan_env res_proc
	| ConditionProc(_, proca, procb, res_proc) -> 
			let new_chan = channel_creation_processes nnode chan_env proca in
			let new_chan1 = channel_creation_processes nnode new_chan procb in
			channel_creation_processes nnode new_chan1 res_proc
	| Assignment(_,_, proc) ->
			channel_creation_processes nnode chan_env proc
	| ActivateActuator(_,_, proc) -> 
			channel_creation_processes nnode chan_env proc
	| POpenIter(proc) -> channel_creation_processes nnode chan_env proc
	| PCloseIter -> chan_env
	| Wait(_, proc) ->
			channel_creation_processes nnode chan_env proc
	| Release(_, proc) ->
		channel_creation_processes nnode chan_env proc
	| Used(_, proc) ->
		channel_creation_processes nnode chan_env proc
	| Prepare(_, proc) ->
		channel_creation_processes nnode chan_env proc
	| Encrypt(_, proc) ->
		channel_creation_processes nnode chan_env proc
	| Decrypt(_, proc) ->
		channel_creation_processes nnode chan_env proc


let rec channel_creation_components (nnode : int) (chan_env) (ast : component) =
	match ast with
	| ParallelComponent(compa, compb) -> 
			let new_chan_env = channel_creation_components nnode chan_env compa in
			channel_creation_components nnode new_chan_env compb
	| Process(name, proc) ->
			channel_creation_processes nnode chan_env proc
	| _ -> chan_env


let rec channel_creation_nodes (chan_env) (ast : node) =
	match ast with
	| ParallelNodes(nodea, nodeb) ->
			let new_chan_env = channel_creation_nodes chan_env nodea in
			channel_creation_nodes new_chan_env nodeb
	| Node(nnode, comp) ->
			channel_creation_components nnode chan_env comp
	| InactNode -> chan_env

(*
let rec pprint_resultant_channels channels =
	match channels with
	| (src, dst, _)::res_channels -> "(" ^ (string_of_int src) ^ ", " ^ (string_of_int dst) ^ ")" ^
											if res_channels = [] then ""
											else ", " ^ pprint_resultant_channels res_channels
	| [] -> "\n Channel Completed \n"
*)

let rec channel_creation (chan_env) (ast : iot_structure)=
	match ast with
	| IoTStructure(_,_,nodedef) -> channel_creation_nodes chan_env nodedef



(* ************ UTILITY FUNCTIONS **************)
let rec find_function (name : string) (fun_env : fun_env) : (string * (string list) * term) =
	match fun_env with
	| [] -> raise(ErrNonExistentFun name)
	| (funname, args, body)::res_fun ->
			if (funname = name) then (funname, args, body)
			else find_function name res_fun  


let rec convert_terms (terms : term) : term list =
	match terms with
	| ParallelTerms(terma, termb) -> (convert_terms terma) @ (convert_terms termb)
	| _ as t -> t::[]


let rec find_value (args : string list) (values : value list) (var : string) : value =
	match (args, values) with
	| ([], _) -> failwith("Error: the variable used in the body of the function is not defined in the arguments")
	| (_, []) -> failwith("Error: the number of actual parameters values is not the same of the formal parameters")
	| ((frst_arg::res_args),(frst_val::res_vals)) ->
		if (frst_arg = var) then frst_val
		else find_value res_args res_vals var


let rec fun_body_exe (args : string list) (values : value list) (functions : fun_env) (body : term) : value =
	match body with
	| Value(a) -> a
	| Add(terma, termb) ->
			let eval_terma = fun_body_exe args values functions terma in
			let eval_termb = fun_body_exe args values functions termb in
			begin
				match (eval_terma, eval_termb) with
				| (Eint(a), Eint(b)) -> Eint(a + b)
				| (_,_) -> failwith("Error: the value used as argument have bad type")
			end
	| Sub(terma, termb) ->
			let eval_terma = fun_body_exe args values functions terma in
			let eval_termb = fun_body_exe args values functions termb in
			begin
				match (eval_terma, eval_termb) with
				| (Eint(a), Eint(b)) -> Eint(a - b)
				| (_,_) -> failwith("Error: the value used as argument have bad type")
			end
	| Mul(terma, termb) ->
			let eval_terma = fun_body_exe args values functions terma in
			let eval_termb = fun_body_exe args values functions termb in
			begin
				match (eval_terma, eval_termb) with
				| (Eint(a), Eint(b)) -> Eint(a * b)
				| (_,_) -> failwith("Error: the value used as argument have bad type")
			end
	| Div(terma, termb) ->
			let eval_terma = fun_body_exe args values functions terma in
			let eval_termb = fun_body_exe args values functions termb in
			begin
				match (eval_terma, eval_termb) with
				| (Eint(a), Eint(b)) -> Eint(a / b)
				| (_,_) -> failwith("Error: the value used as argument have bad type")
			end
	| Variable(x) -> find_value args values x
	| Funct(funname, funargs) ->
			let new_fun = find_function funname functions in
			let converted_args = convert_terms funargs in
			let new_fun_values = List.map (fun_body_exe args values functions) converted_args in
			begin
				match new_fun with
				| (new_funname, new_formal_funargs, new_body) -> 
						fun_body_exe new_formal_funargs new_fun_values functions new_body
			end
	| _ -> failwith("Error: access to sensor information denied for functions")


let rec return_senloc_value (loc : string) (sens_store : (string * value) list) : value =
	match sens_store with
	| [] -> failwith("Error: the indicated sensor doesn't exist")
	| (senloc, value)::res_lst ->
			if (senloc = loc) then value
			else return_senloc_value loc res_lst
	

let rec return_variable_value (name : string) (var_store : (string * value * encryption) list) : value =
	match var_store with
	| [] -> raise(ErrReturnVariableValue name)
	| (var_name, value, enc)::res_lst ->
			if (var_name = name) then value
			else return_variable_value name res_lst
	

let rec eval_term (store : store) (functions : fun_env) (ast : term) : value =
	match ast with
	| Value(value) -> value
	| SensorLoc(loc) -> return_senloc_value loc store.sensors
	| Variable(name) -> return_variable_value name store.variables
	| Add(terma, termb) ->
			let value1 = eval_term store functions terma in
			let value2 = eval_term store functions termb in
			begin
			match (value1, value2) with
				| (Eint(i1), Eint(i2)) -> Eint(i1 + i2)
				| (None, Eint(i2)) -> Eint(i2)
				| (Eint(i1), None) -> Eint(i1)
				| (None, None) -> Eint(0) 
				| (_,_) -> raise(ErrAdd(value1, value2))
			end
	| Sub(terma, termb) ->
			let value1 = eval_term store functions terma in
			let value2 = eval_term store functions termb in
			begin
			match (value1, value2) with
				| (Eint(i1), Eint(i2)) -> Eint(i1 - i2)
				| (None, Eint(i2)) -> Eint(0-i2)
				| (Eint(i1), None) -> Eint(i1)
				| (None, None) -> Eint(0) 
				| (_,_) -> raise(ErrSub(value1, value2))
			end
	| Mul(terma, termb) ->
			let value1 = eval_term store functions terma in
			let value2 = eval_term store functions termb in
			begin
			match (value1, value2) with
				| (Eint(i1), Eint(i2)) -> Eint(i1 * i2)
				| (None, Eint(i2)) -> Eint(0)
				| (Eint(i1), None) -> Eint(0)
				| (None, None) -> Eint(0) 
				| (_,_) -> raise(ErrMul(value1, value2))
			end
	| Div(terma, termb) ->
			let value1 = eval_term store functions terma in
			let value2 = eval_term store functions termb in
			begin
			match (value1, value2) with
				| (Eint(i1), Eint(i2)) -> Eint(i1 / i2)
				| (None, Eint(i2)) -> Eint(0)
				| (Eint(i1), None) -> failwith("Divison per zero occurred during execution")
				| (None, None) -> failwith("Division per zero occured during execution") 
				| (_,_) -> raise(ErrDiv(value1, value2))
			end
	| Funct(name, args) ->
			let funct = find_function name functions in
			let converted_args = convert_terms args in
			let values = List.map (eval_term store functions) converted_args in
			begin
				match funct with
				| (funname, form_args, body) -> fun_body_exe form_args values functions body
			end
	| ParallelTerms(terma, termb) -> 
			failwith("Error: used parallel terms in cases where it is not possible to use them")


let rec check_sen_storage_existance (storage : (string * value) list)  (loc : string) : bool =
	match storage with
	| [] -> false
	| (name, value)::res_storage -> 
			if (loc = name) then true
			else check_sen_storage_existance res_storage loc


let rec check_var_storage_existance (storage : (string * value * encryption) list)  (loc : string) : bool =
	match storage with
	| [] -> false
	| (name, value, enc)::res_storage -> 
			if (loc = name) then true
			else check_var_storage_existance res_storage loc


let rec check_actuator_existance (storage : (string * (string list)) list) (loc : string) : bool =
	match storage with
	| [] -> false
	| (name, values)::res_storage ->
			if (loc = name) then true
			else check_actuator_existance res_storage loc


let rec update_node_store (nnode : int) (new_store : store) (env : nodes_env): nodes_env =
	match env with
	| [] -> failwith("Error: updating a non existent node store")
	| (num, store)::res_env ->
			if   (num = nnode) then (num, new_store)::res_env
			else (num, store)::(update_node_store nnode new_store res_env)


let rec add_new_sensor (store : store) (loc : string) : unit =
	if (not(check_sen_storage_existance store.sensors loc)) then
		store.sensors <- (loc, None)::store.sensors
	else failwith("Error, sensor already existant")


let rec check_node_existance (env : nodes_env) (nnode : int) : bool =
	match env with
	| [] -> false
	| (num, store)::res_env ->
			if (num = nnode) then true
			else check_node_existance res_env nnode


let rec add_new_actuator (store : store) (loc : string) : unit =
	if (not(check_actuator_existance store.actuators loc)) then
		store.actuators <- (loc, [])::store.actuators
	else failwith("Error, actuator already existant")


let rec take_input_channel (src : int) (const : string) (des : int) (input_channels) =
	match input_channels with 
	| (src_node, des_node, ch_const, ip_addr, port, sock)::res_channels ->
			if ((src_node = src) && (des_node = des) && (const = ch_const)) then sock
			else take_input_channel src const des res_channels
	| [] -> raise (ErrNotExistentChannel(src, des, const))


let rec take_output_channel (src : int) (const : string) (des : int) (output_channels) =
	match output_channels with 
	| (src_node, des_node, ch_const, ip_addr, port)::res_channels ->
			if ((src_node = src) && (des_node = des) && (const = ch_const)) then (ip_addr, port)
			else take_output_channel src const des res_channels
	| [] -> raise (ErrNotExistentChannel(src, des, const))


let rec find_and_bind_channels (nnode : int) (channels : (int * int * string * string * int) list) (input_chan : (int * int * string * string * int * Unix.file_descr) list) (output_chan : (int * int * string * string * int) list) =
	match channels with
	| (src, dst, const, ip_addr, port)::res_chan_env ->
			if (src = nnode) then
				find_and_bind_channels nnode res_chan_env input_chan ((src, dst, const, ip_addr, port)::output_chan)
			else if (dst = nnode) then (*Bind a socket to receive communication requests*)
				let new_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
				let _ = Unix.bind new_sock (Unix.ADDR_INET((Unix.inet_addr_of_string "0.0.0.0") , port)) in
				let _ = Unix.listen new_sock 1 in
				find_and_bind_channels nnode res_chan_env ((src, dst, const, ip_addr, port, new_sock)::input_chan) output_chan
			else find_and_bind_channels nnode res_chan_env input_chan output_chan
	| [] -> (input_chan, output_chan)


let rec take_functions (env : iot_env) : fun_env =
	match env with
	| (nodes_env, fun_env) -> fun_env


let rec find_store (nnode : int) (env : nodes_env) : store =
	match env with
	| [] -> raise(ErrNotExistingStore nnode)
	| (num, store)::res_env ->
			if (num = nnode) then store
			else find_store nnode res_env


let rec add_new_variable (store : store) (varname : string) : unit =
	if (not(check_var_storage_existance store.variables varname)) then
			store.variables <- (varname, None, Clear)::(store.variables)
	else failwith("Error, variable already existent")


let rec activate_act_action (action : string) (lst : string list) = 
	match lst with
	| [] -> failwith("The indicated action is not one of the possible one for the actuator")
	| opt::res_lst -> 
			if (opt = action) then opt::res_lst
			else List.append (activate_act_action action res_lst) [opt]


let rec update_act_action (loc : string) (action : string) (actuator_lst : (string * (string list))  list) : ((string * (string list)) list) =
	match actuator_lst with
	| [] -> raise(ErrNotExistentActuator loc)
	| (act_loc, act_lst)::res_lst ->
			if (act_loc = loc) then
				let new_act_lst = activate_act_action action act_lst in
				(act_loc, new_act_lst)::res_lst
			else (act_loc, act_lst)::(update_act_action loc action res_lst)


let random_sensor (min : int) (max : int) : int =
	let bound = max - min in
		let rand_value = (Random.int bound) in min + rand_value
			

let rec update_sen_store (sloc : string) (min : int) (max : int) (sen_store : (string * value) list) : (string * value) list =
	match sen_store with
		| [] -> failwith("The sensor that should be updated is not been declared")
		| (loc, value)::res_lst ->
				if (loc = sloc) then
					let new_value = random_sensor min max in
					(loc, Eint(new_value))::res_lst
				else (loc, value)::(update_sen_store sloc min max res_lst)


let rec add_command (loc : string) (cmd : string) (actuator_list : (string * (string list)) list) : (string * (string list)) list =
	match actuator_list with
	| [] -> failwith("Error: the node that should be updated is not existent")
	| (actloc, old_commands)::res_lst ->
			if (actloc = loc) then
				(actloc, cmd::old_commands)::res_lst
			else (actloc, old_commands)::(add_command loc cmd res_lst)


let rec update_actuator_commands (loc : string) (commands : term) (act_store : (string * (string list)) list) : (string * (string list)) list =
	match commands with
	| ParallelTerms(terma, termb) ->
			let new_act_store = update_actuator_commands loc terma act_store in
			update_actuator_commands loc termb new_act_store
	| Variable(value) -> add_command loc value act_store
	| _ -> raise(ErrActuatorOptionDef loc)


let rec list_of_terms (terms : term) (store : store) (functions : fun_env): (value list) =
	match terms with
	| ParallelTerms(terma, termb) ->
			let first_element = list_of_terms terma store functions in
			let second_element = list_of_terms termb store functions in
			first_element @ second_element
	| Value(value) ->
			if (value = None) then failwith("A none value cannot be used")
			else value::[]
	| SensorLoc(loc) ->  
			let result = return_senloc_value loc store.sensors in
			result::[]
	| Variable(name) ->
			let result = return_variable_value name store.variables in
			result::[]
	| Funct(name, args) -> 
			let funct = find_function name functions in
			let values = list_of_terms args store functions in
			begin
				match funct with
				| (funname, args, body) -> (fun_body_exe args values functions body)::[]
			end
	| Add(terma, termb) ->
			let value1 = list_of_terms terma store functions in
			let value2 = list_of_terms termb store functions in
			begin
			match (value1, value2) with
				| (Eint(i1)::res_list1, Eint(i2)::res_list2) -> 
					if (res_list1 = [] && res_list2 = []) then
						Eint(i1 + i2)::[]
					else raise(ErrParallelTerrmsNotPermitted (value1, value2))
				| (_,_) -> raise(ErrAddList(value1, value2))
			end
	| Sub(terma, termb) ->
			let value1 = list_of_terms terma store functions in
			let value2 = list_of_terms termb store functions in
			begin
			match (value1, value2) with
				| (Eint(i1)::res_list1, Eint(i2)::res_list2) -> 
					if (res_list1 = [] && res_list2 = []) then
						Eint(i1 - i2)::[]
					else raise(ErrParallelTerrmsNotPermitted (value1, value2))
				| (_,_) -> raise(ErrSubList(value1, value2))
			end
	| Mul(terma, termb) ->
			let value1 = list_of_terms terma store functions in
			let value2 = list_of_terms termb store functions in
			begin
			match (value1, value2) with
				| (Eint(i1)::res_list1, Eint(i2)::res_list2) -> 
					if (res_list1 = [] && res_list2 = []) then
						Eint(i1 * i2)::[]
					else raise(ErrParallelTerrmsNotPermitted (value1, value2))
				| (_,_) -> raise(ErrMulList(value1, value2))
			end
	| Div(terma, termb) ->
			let value1 = list_of_terms terma store functions in
			let value2 = list_of_terms termb store functions in
			begin
			match (value1, value2) with
				| (Eint(i1)::res_list1, Eint(i2)::res_list2) -> 
					if (res_list1 = [] && res_list2 = []) then
						Eint(i1 / i2)::[]
					else raise(ErrParallelTerrmsNotPermitted (value1, value2))
				| (_,_) -> raise(ErrDivList(value1, value2))
			end


let rec return_variable_encryption (name : string) (var_store : (string * value * encryption) list) : encryption =
	match var_store with
	| [] -> raise(ErrReturnVariableValue name)
	| (var_name, value, enc)::res_lst ->
			if (var_name = name) then enc
			else return_variable_encryption name res_lst


let rec list_of_output_terms (terms : term) (store : store) (functions : fun_env) : (value * encryption) list =
	match terms with
	| ParallelTerms(terma, termb) ->
			let first_element = list_of_output_terms terma store functions in
			let second_element = list_of_output_terms termb store functions in
			first_element @ second_element
	| Value(value) ->
			if (value = None) then failwith("A none value cannot be used")
			else (value, Clear)::[]
	| SensorLoc(loc) ->  
			let result = return_senloc_value loc store.sensors in
			(result, Clear)::[]
	| Variable(name) ->
			let result = return_variable_value name store.variables in
			let enc = return_variable_encryption name store.variables in
			(result, enc)::[]
	| Funct(name, args) -> 
			let funct = find_function name functions in
			let values = list_of_terms args store functions in
			begin
				match funct with
				| (funname, args, body) -> ((fun_body_exe args values functions body), Clear)::[]
			end
	| Add(terma, termb) ->
			let value1 = list_of_terms terma store functions in
			let value2 = list_of_terms termb store functions in
			begin
			match (value1, value2) with
				| (Eint(i1)::res_list1, Eint(i2)::res_list2) -> 
					if (res_list1 = [] && res_list2 = []) then
						(Eint(i1 + i2), Clear)::[]
					else raise(ErrParallelTerrmsNotPermitted (value1, value2))
				| (_,_) -> raise(ErrAddList(value1, value2))
			end
	| Sub(terma, termb) ->
			let value1 = list_of_terms terma store functions in
			let value2 = list_of_terms termb store functions in
			begin
			match (value1, value2) with
				| (Eint(i1)::res_list1, Eint(i2)::res_list2) -> 
					if (res_list1 = [] && res_list2 = []) then
						(Eint(i1 - i2), Clear)::[]
					else raise(ErrParallelTerrmsNotPermitted (value1, value2))
				| (_,_) -> raise(ErrSubList(value1, value2))
			end
	| Mul(terma, termb) ->
			let value1 = list_of_terms terma store functions in
			let value2 = list_of_terms termb store functions in
			begin
			match (value1, value2) with
				| (Eint(i1)::res_list1, Eint(i2)::res_list2) -> 
					if (res_list1 = [] && res_list2 = []) then
						(Eint(i1 * i2), Clear)::[]
					else raise(ErrParallelTerrmsNotPermitted (value1, value2))
				| (_,_) -> raise(ErrMulList(value1, value2))
			end
	| Div(terma, termb) ->
			let value1 = list_of_terms terma store functions in
			let value2 = list_of_terms termb store functions in
			begin
			match (value1, value2) with
				| (Eint(i1)::res_list1, Eint(i2)::res_list2) -> 
					if (res_list1 = [] && res_list2 = []) then
						(Eint(i1 / i2), Clear)::[]
					else raise(ErrParallelTerrmsNotPermitted (value1, value2))
				| (_,_) -> raise(ErrDivList(value1, value2))
			end


let rec pprint_value_list (values : value list) =
	match values with
	| value::res_value ->
		if res_value = [] then pprint_value value
		else pprint_value value ^ ", " ^ pprint_value_list res_value
	| _ -> ""


let rec pprint_encr_value_list (values : (value * encryption) list) =
	match values with
	| (value, enc)::res_value ->
		if res_value = [] then pprint_value value ^ "[" ^ pprint_encryption enc ^ "]"
		else pprint_value value ^ "[" ^ pprint_encryption enc ^ "], " ^ pprint_encr_value_list res_value
	| _ -> ""


let rec pprint_output (name : string) (const : string) (src : int) (values : (value * encryption) list) (des : int) =
	begin
		let path = "./IoT-System/nodes/" ^ (string_of_int src) ^ "/" ^ name ^ ".txt" in
		let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
		let _ = Printf.fprintf oc "%s" ("Output " ^ (sprintf "%s" const) ^ ": " ^ (pprint_encr_value_list values) ^ " -> " ^ (string_of_int des) ^ "\n") in
		close_out oc
	end

let rec serialize (values : (value * encryption) list) : string =
	match values with
	| value::res_values ->
		begin
			match value with
			| (None, Encrypted) -> "NE::" ^ (serialize res_values)
			| (None, Clear) -> "NC::" ^ (serialize res_values)
			| (Eint(a), Encrypted) -> "IE" ^ (string_of_int a) ^ "::" ^ (serialize res_values)
			| (Eint(a), Clear) -> "IC" ^ (string_of_int a) ^ "::" ^ (serialize res_values)
			| (Ebool(a), Encrypted) -> "BE" ^ (string_of_bool a) ^ "::" ^ (serialize res_values)
			| (Ebool(a), Clear) -> "BC" ^ (string_of_bool a) ^ "::" ^ (serialize res_values)
			| (Estring(a), Encrypted) -> "SE" ^ a ^ "::" ^ (serialize res_values)
			| (Estring(a), Clear) -> "SC" ^ a ^ "::" ^ (serialize res_values)
			| (Echar(a), Encrypted) -> "CE" ^ (String.make 1 a) ^ "::" ^ (serialize res_values)
			| (Echar(a), Clear) -> "CC" ^ (String.make 1 a) ^ "::" ^ (serialize res_values)
		end
	| [] -> ""


let explode s =
	let rec expl i l =
		if i < 0 then l 
		else expl (i-1) (s.[i] :: l) in
	expl (String.length s-1) []


let rec implode (l : char list) (res : string) : string =
	match l with
	| [] -> res
	| a::res_chars -> implode res_chars (res ^ (String.make 1 a))
		

let rec from_strlist_to_vallist (str_list : string list) : (value * encryption) list =
	match str_list with
	| str::res_list ->
		let char_list = explode str in
		begin
			match char_list with
			| 'I'::'E'::a ->
					(Eint(int_of_string (implode a "")), Encrypted)::(from_strlist_to_vallist res_list)
			| 'I'::'C'::a ->
					(Eint(int_of_string (implode a "")), Clear)::(from_strlist_to_vallist res_list)
			| 'B'::'E'::a->
				let bool_value =
					begin
						if ((implode a "") = "false") then false
						else if ((implode a "") = "true") then true
						else failwith("Message received for bool is not a true nor a false")
					end in
					(Ebool(bool_value), Encrypted)::(from_strlist_to_vallist res_list)	
			| 'B'::'C'::a->
				let bool_value =
					begin
						if ((implode a "") = "false") then false
						else if ((implode a "") = "true") then true
						else failwith("Message received for bool is not a true nor a false")
					end in
					(Ebool(bool_value), Clear)::(from_strlist_to_vallist res_list)	
			| 'S'::'E'::a ->
					(Estring(implode a ""), Encrypted)::(from_strlist_to_vallist res_list)
			| 'S'::'C'::a ->
					(Estring(implode a ""), Clear)::(from_strlist_to_vallist res_list)
			| 'C'::'E'::a::[] ->
					(Echar(a), Encrypted)::(from_strlist_to_vallist res_list)
			| 'C'::'C'::a::[] ->
					(Echar(a), Clear)::(from_strlist_to_vallist res_list)
			| 'N'::'E'::a ->
					(None, Encrypted)::(from_strlist_to_vallist res_list)
			| 'N'::'C'::a ->
					(None, Clear)::(from_strlist_to_vallist res_list)
			| _ -> raise (ErrDeserialization(str))
		end
	| [] -> []			


let rec deserialize (buffer : bytes) : (value  * encryption) list =
	let str_buff = Bytes.to_string buffer in
	let str_list = Str.split (Str.regexp "::") str_buff in
	from_strlist_to_vallist str_list


let set_dim_bytes (dim : int) : (bytes) =
	let _ =
		begin
			if dim >= 9999 then failwith("Message dimension too big to handle") 
			else ()
		end in
	let dim_str = string_of_int dim in
	let dim_str_len = String.length dim_str in
	let buffer = Bytes.create 5 in
	let _ = Bytes.fill buffer 0 (5 - dim_str_len) '0' in
	let _ = Bytes.blit_string dim_str 0 buffer (5-dim_str_len) dim_str_len in
	buffer 


let rec output_management (name : string) (outputs : term) (src : int) (const : string) (dest : node_number) (node_store : store) (channels) (functions : fun_env) : bool =
	match dest with
	| NoNode -> true
	| NodeNumber(des) ->
		begin
			match channels with
			| (input_channels, output_channels) ->
				let (out_ip_addr, out_port) = take_output_channel src const des output_channels in
				let values = list_of_output_terms outputs node_store functions in
				let str_values = serialize values in
				let ser_values = Bytes.of_string str_values in
				let msg_dim = String.length str_values in
				let msg_dim_bytes = set_dim_bytes msg_dim in
				let tmp_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
				let _ = Unix.connect tmp_sock (Unix.ADDR_INET((Unix.inet_addr_of_string out_ip_addr), out_port)) in
				let _ = Unix.send tmp_sock msg_dim_bytes 0 5 [] in
				let _ = Unix.send tmp_sock ser_values 0 msg_dim [] in
				let _ = Unix.close tmp_sock in
				let _ = pprint_output name const src values des in 
				true
		end
	| ParallelNodeNumber(num1, num2) ->
			(output_management name outputs src const num1 node_store channels functions) &&
			(output_management name outputs src const num2 node_store channels functions)
		

let rec list_of_variables (vars : term) : (string list) =
	match vars with
	| Variable(var) -> var::[]
	| ParallelTerms(terma, termb) ->
			let first_element = list_of_variables terma in
			let second_element = list_of_variables termb in
			first_element @ second_element
	| _ -> failwith("Error: list of variables contains at least one element that is not a variable")


let rec insert_single_value (variable : string) (new_value : value) (new_encr : encryption) (var_store : (string * value * encryption) list) : (string * value * encryption) list =
	match var_store with
	| (frst_var, old_value, old_encr)::res_var ->
		if (frst_var = variable) then
			(variable, new_value, new_encr)::res_var
		else (frst_var, old_value, old_encr)::(insert_single_value variable new_value new_encr res_var)
	| [] -> failwith("Error: the variable indicated in input doesn't exist")

let rec insert_values (input_vars : string list) (values : (value * encryption) list) (var_store : (string * value * encryption) list) : ((string * value * encryption) list) =
	match input_vars with
	| var::res_var -> 
		begin
			match values with
			| (frst_val, frst_enc)::res_val -> 
					let new_var_store = insert_single_value var frst_val frst_enc var_store in
					insert_values res_var res_val new_var_store
			| [] -> failwith("Error in the input function the variables exceeds the number of input")
		end
	| [] -> var_store


let rec take_channel_inputs (src : int) (const : string) (dst : int) (channels) : bytes =
	match channels with
	| (input_channels, output_channels) ->
		let inp_sock = take_input_channel src const dst input_channels in
		let (sender_sock, sender_sockaddr) = Unix.accept inp_sock in
		let dim_buffer = Bytes.create 5 in
		let _ = Unix.recv sender_sock dim_buffer 0 5 [] in
		let _ = Printf.printf "%s\n" (Bytes.to_string dim_buffer) in
		let msg_dim = int_of_string(Bytes.to_string dim_buffer) in
		let buffer = Bytes.create msg_dim in
		let _ = Unix.recv sender_sock buffer 0 msg_dim [] in
		let _ = Printf.printf "%s\n" (Bytes.to_string buffer) in 
		buffer


let rec take_value (variable : string) (var_store : (string * value * encryption) list) : value =
	match var_store with
	| (name, value, enc)::res_store ->
			if(name = variable) then value
			else take_value variable res_store
	| [] -> failwith("Error during taking variable value: something wrong during the input function") 


let rec take_old_values (variables : string list) (var_store : (string * value * encryption) list) : (value list) =
	match variables with
	| var::res_var ->
			(take_value var var_store)::(take_old_values res_var var_store)
	| [] -> []  


let rec pprint_string_list (strings : string list) =
	match strings with
	| str::res_str ->
			if (res_str = []) then str
			else str ^ ", " ^ (pprint_string_list res_str)
	| [] -> ""	


let rec pprint_input (name : string) (const : string) (nnode : int) (variables : string list) (old_values : value list) (new_values : (value * encryption) list) (source : int) =
	begin	
		let path = "./IoT-System/nodes/" ^ (string_of_int nnode) ^ "/" ^ name ^ ".txt" in
		let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
		let _ = Printf.fprintf oc "%s" ("Input " ^ (sprintf "%s" const) ^ ": " ^ (pprint_string_list variables) ^ " : " ^ (pprint_value_list old_values) ^ " -> " ^ (pprint_encr_value_list new_values) ^ " : " ^ (string_of_int source) ^ "\n") in
		close_out oc
	end


let input_management (name : string) (src_node : int) (const : string) (des_node : int) (inputs : string list) (node_store : store) (channels) : unit =
	let ser_value_list = take_channel_inputs src_node const des_node channels in
	let value_list = deserialize ser_value_list in
	let old_values = take_old_values inputs node_store.variables in
	let _ =	node_store.variables <- (insert_values inputs value_list node_store.variables) in
	pprint_input name const des_node inputs old_values value_list src_node


let rec generate_next_input (len : int) (constants : string list) : value list =
	match constants with
	| [] -> if len = 0 then []
					else Eint(0)::(generate_next_input (len-1) constants)
	| frst::res_const -> if len = 0 then failwith("The len of the values is less than the number of constants")
											 else Estring(frst)::(generate_next_input (len-1) res_const)  


let rec update_storage (variable : string)  (new_value : value) (var_store : (string * value * encryption) list) : ((string * value * encryption) list) =
	match var_store with
	| (varname, old_val, enc)::res_store ->
			if (varname = variable) then
				(variable, new_value, Clear):: res_store
			else (varname, old_val, enc)::(update_storage variable new_value res_store)
	| [] -> raise(ErrNotExistentVar variable)








(* ******************** EVALUATION FUNCTIONS ******************)
let rec eval_sensor (nsen : string) (store : store) (ast : sensor) : unit =
	match ast with 
	| InactiveSensor -> ()
	| SensorIntAction(sens) -> eval_sensor nsen store sens
	| SOpenIter(sens) -> eval_sensor nsen store sens
	| SCloseIter -> ()
	| SensorStore(loc, minlim, maxlim, sens) ->
		let _ =
			store.sensors <- update_sen_store nsen minlim maxlim store.sensors in
			eval_sensor nsen store sens


let rec eval_condition (store : store) (ast : condition) (functions : fun_env) : bool =
	match ast with
	| Bool(b) -> b
	| Greater(x, y) ->
			let x1 = eval_term store functions x in
			let y1 = eval_term store functions y in
			let nx1 = 
			begin
				if (x1 = None) then Eint(0)
				else x1
			end in
			let ny1 =
			begin
				if (y1 = None) then Eint(0)
				else y1
			end in
			(nx1 > ny1)
	| Lower(x, y) -> 
			let x1 = eval_term store functions x in
			let y1 = eval_term store functions y in
			let nx1 = 
				begin
					if (x1 = None) then Eint(0)
					else x1
				end in
				let ny1 =
				begin
					if (y1 = None) then Eint(0)
					else y1
				end in
			(nx1 < ny1)
	| Equal(x, y) ->
			let x1 = eval_term store functions x in
			let y1 = eval_term store functions y in
			let nx1 = 
				begin
					if (x1 = None) then Eint(0)
					else x1
				end in
				let ny1 =
				begin
					if (y1 = None) then Eint(0)
					else y1
				end in
			(nx1 = ny1)
	| EqGr(x, y) ->
			let x1 = eval_term store functions x in
			let y1 = eval_term store functions y in
			let nx1 = 
				begin
					if (x1 = None) then Eint(0)
					else x1
				end in
				let ny1 =
				begin
					if (y1 = None) then Eint(0)
					else y1
				end in
			(nx1 >= ny1)
	| EqLw(x, y) ->
			let x1 = eval_term store functions x in
			let y1 = eval_term store functions y in
			let nx1 = 
				begin
					if (x1 = None) then Eint(0)
					else x1
				end in
				let ny1 =
				begin
					if (y1 = None) then Eint(0)
					else y1
				end in
			(nx1 >= ny1)
	| And(x, y) ->
			let x1 = eval_condition store x functions in
			let y1 = eval_condition store y functions in
			(x1 && y1)
	| Or(x, y) ->
			let x1 = eval_condition store x functions in
			let y1 = eval_condition store y functions in
			(x1 || y1)


let rec take_release_semaphore (var : string) (semaphores : (string * Binary.t * Binary.t) list) : Binary.t =
	match semaphores with
	| (name, release_sem, usage_sem)::res_sems ->
		if (name = var) then release_sem
		else take_release_semaphore var res_sems
	| _ -> failwith("There is a variable that doesn't have the semaphore for the management. Something bad happened during the setting phase.")


let rec take_usage_semaphore (var : string) (semaphores : (string * Binary.t * Binary.t) list) : Binary.t =
	match semaphores with
	| (name, release_sem, usage_sem)::res_sems ->
		if (name = var) then usage_sem
		else take_usage_semaphore var res_sems
	| _ -> failwith("There is a variable that doesn't have the semaphore for the management. Something bad happened during the setting phase.")


let rec pprint_assignment (name : string) (nnode : int) (var : string) (old_value : value) (new_value : value) =
	begin
		let path = "./IoT-System/nodes/" ^ (string_of_int nnode) ^ "/" ^ name ^ ".txt" in
		let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
		let _ = Printf.fprintf oc "%s" ("Assignment : " ^ var ^ " : " ^ (pprint_value old_value) ^ " -> " ^ (pprint_value new_value) ^ "\n") in
		close_out oc
	end


let rec pprint_act_actuator (name : string) (nnode : int) (actloc : string) (action : string) =
	begin
		let path = "./IoT-System/nodes/" ^ (string_of_int nnode) ^ "/" ^ name ^ ".txt" in
		let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
		let _ = Printf.fprintf oc "%s" ("ActuatorAction : " ^ actloc ^ " : " ^ action ^ "\n") in
		close_out oc
	end


let rec update_encryption (variable : string) (var_store : (string * value * encryption) list) : ((string * value * encryption) list) =
	match var_store with
	| (varname, old_val, enc)::res_store ->
			if (varname = variable) then
				(variable, old_val, Encrypted):: res_store
			else (varname, old_val, enc)::(update_encryption variable res_store)
	| [] -> raise(ErrNotExistentVar variable)


let rec update_decryption (variable : string) (var_store : (string * value * encryption) list) : ((string * value * encryption) list) =
	match var_store with
	| (varname, old_val, enc)::res_store ->
			if (varname = variable) then
				(variable, old_val, Clear):: res_store
			else (varname, old_val, enc)::(update_decryption variable res_store)
	| [] -> raise(ErrNotExistentVar variable)


let rec pprint_encryption (name : string) (nnode : int) (var : string) =
	begin
		let path = "./IoT-System/nodes/" ^ (string_of_int nnode) ^ "/" ^ name ^ ".txt" in
		let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
		let _ = Printf.fprintf oc "%s" ("Encryption : " ^ var ^ "\n") in
		close_out oc
	end


let rec pprint_decryption (name : string) (nnode : int) (var : string) =
	begin
		let path = "./IoT-System/nodes/" ^ (string_of_int nnode) ^ "/" ^ name ^ ".txt" in
		let oc = open_out_gen [Open_append; Open_creat] 0o666 path in
		let _ = Printf.fprintf oc "%s" ("Decryption : " ^ var ^ "\n") in
		close_out oc
	end


let rec eval_process (name : string) (nnode : int) (node_store : store) (channels) (functions : fun_env) (ast : process) : unit =
	match ast with
	| InactProcess -> ()
	| MultiOutput(const, outputs, dest, res_proc) ->
			let result = output_management name outputs nnode const dest node_store channels functions in
			if (result) then eval_process name nnode node_store channels functions res_proc
			else raise (ErrOutputGoneBad nnode)
	| InputProc(src_node, const, input_vars, res_proc) ->
			let input_var_lst = list_of_variables input_vars in
			let _ = input_management name src_node const nnode input_var_lst node_store channels in
			eval_process name nnode node_store channels functions res_proc
	| ConditionProc(cond, proca, procb, res_proc) ->
			let x = eval_condition node_store cond functions in
			let _ =
			begin
				if (x = true) then eval_process name nnode node_store channels functions proca
				else eval_process name nnode node_store channels functions procb
			end in
			eval_process name nnode node_store channels functions res_proc
	| Assignment(x, y, res_proc) ->
			let y1 = eval_term node_store functions y in
			let old_value = take_value x node_store.variables in
			let _ = node_store.variables <- (update_storage x y1 node_store.variables) in
			let _ = pprint_assignment name nnode x old_value y1 in
			eval_process name nnode node_store channels functions res_proc
	| ActivateActuator(actloc, action, res_proc) ->
			let _ = node_store.actuators <- (update_act_action actloc action node_store.actuators) in
			let _ = pprint_act_actuator name nnode actloc action in
			eval_process name nnode node_store channels functions res_proc
	| PCloseIter -> ()
	| POpenIter(res_proc) ->
			eval_process name nnode node_store channels functions res_proc
	| Wait(var, res_proc) ->
			let rel_sem = take_release_semaphore var node_store.semaphores in 
			let _ = Binary.acquire rel_sem in
			eval_process name nnode node_store channels functions res_proc
	| Used(var, res_proc) ->
			let usg_sem = take_usage_semaphore var node_store.semaphores in
			let _ = Binary.release usg_sem in
			eval_process name nnode node_store channels functions res_proc
	| Prepare(var, res_proc) ->
			let usg_sem = take_usage_semaphore var node_store.semaphores in
			let _ = Binary.acquire usg_sem in
			eval_process name nnode node_store channels functions res_proc
	| Release(var, res_proc) ->
			let rel_sem = take_release_semaphore var node_store.semaphores in
			let _ = Binary.release rel_sem in
			eval_process name nnode node_store channels functions res_proc
	| Encrypt(var, res_proc) -> 
			let _ = node_store.variables <- (update_encryption var node_store.variables) in
			let _ = pprint_encryption name nnode var in
			eval_process name nnode node_store channels functions res_proc
	| Decrypt(var, res_proc) ->
			let _ = node_store.variables <- (update_decryption var node_store.variables) in
			let _ = pprint_decryption name nnode var in
			eval_process name nnode node_store channels functions res_proc


let rec sensor_processing (name : string) (nnode : int) (node_store : store) (ast : sensor) : unit =
	let _ = eval_sensor name node_store ast in
	begin
		Unix.sleep 1;
		sensor_processing name nnode node_store ast
	end


let rec proc_processing (name : string) (nnode : int) (node_store : store) (channels) (functions : fun_env) (ast : process) : unit =
	let _ = eval_process name nnode node_store channels functions ast in
	begin
		(*pprint_proc_status name nnode node_store;*)
		let _ = Unix.sleep 1 in
		proc_processing name nnode node_store channels functions ast
	end


let rec check_thread_existance (ast : component) : bool =
	match ast with
	| ParallelComponent(compa, compb) -> (check_thread_existance compa) || (check_thread_existance compb)
	| Sensor(_,_) -> true
	| Process(_,_) -> true
	| _ -> false  


let rec eval_component (nnode : int) (node_store : store) (channels) (functions : fun_env) (ast : component) : unit =
	match ast with 
	| InactiveComponent -> ()
	| ParallelComponent(compa, compb) ->
		(* CASES MANAGEMENT *)
		begin
			match compa with
			| ParallelComponent(_,_) ->
				begin
					match compb with
					| Process(_,_) | Sensor(_,_) ->
							let _ = Domain.spawn (fun _ -> eval_component nnode node_store channels functions compa) in
							eval_component nnode node_store channels functions compb
					| ParallelComponent(_,_) -> 
							if (check_thread_existance compb) then 
								begin
									let _ = Domain.spawn (fun _ -> eval_component nnode node_store channels functions compa) in
									eval_component nnode node_store channels functions compb
								end
							else eval_component nnode node_store channels functions compa
					| _ -> eval_component nnode node_store channels functions compa
				end
			| Process(_,_) | Sensor(_,_) -> 
					let _ = Domain.spawn (fun _ -> eval_component nnode node_store channels functions compb) in
					eval_component nnode node_store channels functions compa
			| _ -> eval_component nnode node_store channels functions compb
		end
	| Process(name, proc) ->
			proc_processing name nnode node_store channels functions proc
	| Sensor(loc, sens) ->
			sensor_processing loc nnode node_store sens
	| Actuator(loc, act) -> ()


let rec eval_node_definition (env : iot_env) (ast : node) (channels) (mode : int): unit = 
	match ast with
	| ParallelNodes(nodea, nodeb) ->
			let _ =  eval_node_definition env nodeb channels mode in
			eval_node_definition env nodea channels mode
	| Node(nnode, comp) ->
		if (mode = 0) then (*Management of the nodes containers*)
			begin
				let _ = Sys.command ("sh nodeContainer.sh -n " ^ (string_of_int nnode)) in ()
			end
		else if (nnode = mode) then
		begin
			match env with
			| (nodes_env, fun_env) -> 
					let node_store = find_store nnode nodes_env in
					let node_ch = find_and_bind_channels nnode channels [] [] in 
					let functions = take_functions env in
					let _ = Unix.sleep wait_container_setting in
					eval_component nnode node_store node_ch functions comp
		end
		else () 
	| InactNode -> ()


let rec eval_IoTStructure (env : iot_env) (channels) (ast : iot_structure) (mode : int) = 
	match ast with
	| IoTStructure(decl, func, def) ->
			eval_node_definition env def channels mode





(* ********************** STATIC ANALYSIS FOR THE CONFIGURATION ****************)
let rec frst_eval_actuator (loc : string) (store : store) (ast : actuator) : store =
	match ast with
	| InactiveActuator -> store
	| ActuatorIntAction(act) -> frst_eval_actuator loc store act
	| ActuatorCommand(commands, act_proc) ->
			let _ = store.actuators <- (update_actuator_commands loc commands store.actuators) in
			frst_eval_actuator loc store act_proc
	| ACloseIter -> store
	| AOpenIter(act) -> frst_eval_actuator loc store act


let rec frst_eval_sensor (nsen : string) (store : store) (ast : sensor) : store =
	match ast with
	| InactiveSensor -> store
	| SensorIntAction(sens) -> frst_eval_sensor nsen store sens
	| SOpenIter(sens) -> frst_eval_sensor nsen store sens
	| SCloseIter -> store
	| SensorStore(loc, minlim, maxlim, sens) ->
			if (minlim > maxlim) then raise (ErrMinHigherThanMaxSensor nsen)
			else let _ = store.sensors <- (update_sen_store nsen minlim maxlim store.sensors) in
				frst_eval_sensor nsen store sens


let rec add_semaphores (store : store) (proc : process) : store =
	match proc with
	| InactProcess -> store
	| MultiOutput(_,_,_,res_proc) -> add_semaphores store res_proc
	| InputProc(_,_,_,res_proc) -> add_semaphores store res_proc
	| ConditionProc(_, proca, procb, res_proc) -> 
			let new_store = add_semaphores store proca in
			add_semaphores new_store res_proc
	| Assignment(_,_,res_proc) -> add_semaphores store res_proc
	| ActivateActuator(_,_,res_proc) -> add_semaphores store res_proc
	| PCloseIter -> store
	| POpenIter(res_proc) -> add_semaphores store res_proc
	| Wait(_, res_proc) -> add_semaphores store res_proc
	| Release(var, res_proc) ->
			let _ = store.semaphores <- (var, (Binary.make false), (Binary.make true))::store.semaphores in store
	| Used(_, res_proc) -> add_semaphores store res_proc
	| Prepare(_, res_proc) -> add_semaphores store res_proc
	| Encrypt(_, res_proc) -> add_semaphores store res_proc
	| Decrypt(_, res_proc) -> add_semaphores store res_proc


let rec frst_eval_component (nnode : int) (env : iot_env) (ast : component) : iot_env =
	match ast with
	| InactiveComponent -> env
	| ParallelComponent(compa, compb) ->
			let new_env = frst_eval_component nnode env compa in
			frst_eval_component nnode new_env compb
	| Process(name, proc) ->
		begin
			match env with
			| (nodes_env, fun_env) ->
				let store = find_store nnode nodes_env in
				let new_store = add_semaphores store proc in
				((update_node_store nnode new_store nodes_env), fun_env)
		end
	| Sensor(loc, sens) ->
		begin
			match env with
			| (nodes_env, fun_env) -> 
				let store = find_store nnode nodes_env in 
				let new_store = frst_eval_sensor loc store sens in
				((update_node_store nnode new_store nodes_env), fun_env)
		end
	| Actuator(loc, act) ->
		begin
			match env with
			| (nodes_env, fun_env) ->
			let store = find_store nnode nodes_env in 
			let new_store = frst_eval_actuator loc store act in
			((update_node_store nnode new_store nodes_env), fun_env)
		end


let rec frst_eval_node_definition (env : iot_env) (ast : node) : (iot_env) =
	match ast with
	| ParallelNodes(nodea, nodeb) ->
			let new_env = frst_eval_node_definition env nodea in
			frst_eval_node_definition new_env nodeb
	| Node(nnode, compa) ->
			frst_eval_component nnode env compa
	| InactNode -> env


let rec eval_function_definition (env : iot_env) (ast : funct_definition) : iot_env =
	match ast with
	| InactFunction -> env
	| ParallelFunctions(fun1, fun2) ->
		let new_env = eval_function_definition env fun1 in
		eval_function_definition new_env fun2
	| FunctionDefinition(name, args, body) ->
			let arg_lst = list_of_variables args in
			begin
				match env with
				| (nodes_env, fun_env) ->
					(nodes_env, (name, arg_lst, body)::fun_env)
			end
	
	
let rec eval_declaration (store : store) (ast : declaration) : store =
	match ast with
	| ParallelDeclarations(decla, declb) ->
			let new_store = eval_declaration store decla in
			eval_declaration new_store declb
	| SensorDeclaration(loc) ->
			let _ = add_new_sensor store loc in store
	| ActuatorDeclaration(loc) ->
			let _ = add_new_actuator store loc in store
	| VariableDeclaration(name) ->
			let _ = add_new_variable store name in store


let rec eval_node_declaration (env : iot_env) (ast : node_declaration) : iot_env =
	match ast with
	| ParallelNodesDeclaration(decla, declb) ->
			let new_env = eval_node_declaration env decla in
			eval_node_declaration new_env declb
	| Store(nnode, decl) ->
		begin
			match env with
			| (nodes_env, fun_env) ->
				let new_store =
					{
						variables   = [];
						sensors     = [];
						actuators   = [];
						semaphores  = [];
					} in
				if (check_node_existance nodes_env nnode) then
					failwith("Error, node already existant")
				else (((nnode, (eval_declaration new_store decl))::nodes_env), fun_env)
		end


let rec frst_eval_IotStructure (env : iot_env) (ast : iot_structure) =
	Random.init 1312414;
	match ast with
	| IoTStructure(decl, func, def) ->
		let new_env = eval_node_declaration env decl in
		let new_env2 = eval_function_definition new_env func in
		frst_eval_node_definition new_env2 def