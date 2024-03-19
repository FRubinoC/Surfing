(* Abstract Syntax Tree *)
open Printf
open List
open Random
open Unix

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
	| MultiOutput of  term * node_number * process
	| InputProc of int * term * term * process
	| ConditionProc of condition * process * process
	| Assignment of string * term * process
	| ActivateActuator of string * string * process
	| PCloseIter
	| POpenIter of process


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
exception ErrAdd of value * value
exception ErrSub of value * value
exception ErrMul of value * value
exception ErrDiv of value * value
exception ErrAddList of (value list) * (value list)
exception ErrSubList of (value list) * (value list)
exception ErrMulList of (value list) * (value list)
exception ErrDivList of (value list) * (value list)



(* Environmental structures *)
type store = 
{
	variables   : (string * value) list;
	sensors     : (string * value) list;
	actuators   : (string * (string list))  list;
	prev_inputs : (int * (value list)) list;
	next_inputs : (int * (value list)) list; 
}

type fun_env = (string * (string list) * term) list
type nodes_env = (int * store) list
type iot_env = nodes_env * fun_env;;
let empty_env = [];;





(* Print functions for Abstract Syntax Tree*)

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
	| MultiOutput(terms, nodn, proc) ->
			"\n\t\t\tMultiOutput(" ^ pprint_term terms ^ ", " ^ pprint_nodeNum nodn ^ ", " ^ pprint_process proc ^ ")"
	| InputProc(src_node, const, var, proc) ->
			"\n\t\t\tInputProc(" ^ sprintf "%i" src_node ^ ", " ^ pprint_term const ^ ", " ^ pprint_term var ^ ", " ^ pprint_process proc ^ ")"
	| ConditionProc(cond, proca, procb) ->
			"\n\t\t\tConditionProc(" ^ pprint_condition cond ^ ", " ^ pprint_process proca ^ ", " ^ pprint_process procb ^ ")"
	| Assignment(var, value, proc) ->
			"\n\t\t\tAssignment(" ^ sprintf "%s" var ^ ", " ^ pprint_term value ^ ", " ^ pprint_process proc ^ ")"
	|	ActivateActuator(actloc, action, proc) ->
			"\n\t\t\tActivateActuator(" ^ sprintf "%s" actloc ^ ", " ^ sprintf "%s" action ^ ", " ^ pprint_process proc ^ ")"
	| PCloseIter -> "\n\t\t\tPCloseIter"
	| POpenIter(proc) -> "\n\t\t\tPOpenIter(" ^ pprint_process proc ^ ")"


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



(* Print environment functions *)
let rec pprint_node_variables (var_store : (string * value) list) : string =
	match var_store with
	| [] -> ""
	| (name, value)::res_store ->
			"(" ^ name ^ " : " ^ pprint_value value ^ "); " ^ pprint_node_variables res_store


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


let rec pprint_node_prev_inputs (pinp_store : (int * (value list)) list) : string =
	match pinp_store with
	| [] -> ""
	| (origin, old_inp_list)::res_store ->
			string_of_int origin ^ ": (" ^ pprint_input_list old_inp_list ^ "); " ^ pprint_node_prev_inputs res_store 


let rec pprint_node_next_inputs (ninp_store : (int * (value list)) list) : string =
	match ninp_store with
	| [] -> ""
	| (origin, ninp_list)::res_store ->
			string_of_int origin ^ ": (" ^ pprint_input_list ninp_list ^ ");" ^ pprint_node_next_inputs res_store


let rec pprint_node_store (nodenum : int) (store : store) : string =
	"Node number " ^ string_of_int nodenum ^
	"\n\tVariables -> " ^ pprint_node_variables store.variables ^ 
	";\n\tSensors -> " ^ pprint_node_variables store.sensors ^
	";\n\tActuators -> " ^ pprint_node_actuators store.actuators ^
	";\n\tPrevInputs -> " ^ pprint_node_prev_inputs store.prev_inputs ^
	";\n\tNextInputs -> " ^ pprint_node_next_inputs store.next_inputs ^ ";\n"


let rec pprint_nodes_env (environment : nodes_env) : string =
	match environment with 
	| [] -> ""
	| (nodenum, store)::res_env -> 
			pprint_node_store nodenum store ^ "\n\n" ^ pprint_nodes_env res_env

let rec pprint_function_env (environment : fun_env) : string =
	match environment with
	| [] -> ""
	| (funname, args, body)::res_env ->
			sprintf "%s" funname ^ if res_env = [] then ""
														 else ", " ^ pprint_function_env res_env

let pprint_status (iot_env : iot_env) =
	match iot_env with
	| (nodes_env, fun_env) ->
	begin
		Printf.printf "\n\n************************************\nEnvironment:\n";
		Printf.printf "%s" (pprint_nodes_env nodes_env);
		Printf.printf "\n\nFunctions:\n";
		Printf.printf "%s" (pprint_function_env fun_env);
		Printf.printf "\n************************************\n\n\n\n\n";
	end






(* Utility functions*)
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
			let new_fun_values = map (fun_body_exe args values functions) converted_args in
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
	

let rec return_variable_value (name : string) (var_store : (string * value) list) : value =
	match var_store with
	| [] -> raise(ErrReturnVariableValue name)
	| (var_name, value)::res_lst ->
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
				| (_,_) -> raise(ErrAdd(value1, value2))
			end
	| Sub(terma, termb) ->
			let value1 = eval_term store functions terma in
			let value2 = eval_term store functions termb in
			begin
			match (value1, value2) with
				| (Eint(i1), Eint(i2)) -> Eint(i1 - i2)
				| (_,_) -> raise(ErrSub(value1, value2))
			end
	| Mul(terma, termb) ->
			let value1 = eval_term store functions terma in
			let value2 = eval_term store functions termb in
			begin
			match (value1, value2) with
				| (Eint(i1), Eint(i2)) -> Eint(i1 * i2)
				| (_,_) -> raise(ErrMul(value1, value2))
			end
	| Div(terma, termb) ->
			let value1 = eval_term store functions terma in
			let value2 = eval_term store functions termb in
			begin
			match (value1, value2) with
				| (Eint(i1), Eint(i2)) -> Eint(i1 / i2)
				| (_,_) -> raise(ErrDiv(value1, value2))
			end
	| Funct(name, args) ->
			let funct = find_function name functions in
			let converted_args = convert_terms args in
			let values = map (eval_term store functions) converted_args in
			begin
				match funct with
				| (funname, form_args, body) -> fun_body_exe form_args values functions body
			end
	| ParallelTerms(terma, termb) -> 
			failwith("Error: used parallel terms in cases where it is not possible to use them")


let rec check_storage_existance (storage : (string * value) list)  (loc : string) : bool =
	match storage with
	| [] -> false
	| (name, value)::res_storage -> 
			if (loc = name) then true
			else check_storage_existance res_storage loc


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


let rec add_new_sensor (store : store) (loc : string) : store =
	if (not(check_storage_existance store.sensors loc)) then
			{
				variables = store.variables;
				sensors   = (loc, None)::store.sensors;
				actuators = store.actuators;
				prev_inputs = store.prev_inputs;
				next_inputs = store.next_inputs;
			}
	else failwith("Error, sensor already existant")


let rec check_node_existance (env : nodes_env) (nnode : int) : bool =
	match env with
	| [] -> false
	| (num, store)::res_env ->
			if (num = nnode) then true
			else check_node_existance res_env nnode


let rec add_new_actuator (store : store) (loc : string) : store =
	if (not(check_actuator_existance store.actuators loc)) then
			{
				variables = store.variables;
				sensors   = store.sensors;
				actuators = (loc, [])::store.actuators;
				prev_inputs = store.prev_inputs;
				next_inputs = store.next_inputs;
			}
	else failwith("Error, actuator already existant")


let rec find_store (nnode : int) (env : nodes_env) : store =
	match env with
	| [] -> raise(ErrNotExistingStore nnode)
	| (num, store)::res_env ->
			if (num = nnode) then store
			else find_store nnode res_env


let rec add_new_variable (store : store) (varname : string) : store =
	if (not(check_storage_existance store.variables varname)) then
			{
				variables = (varname, None)::(store.variables);
				sensors   = store.sensors;
				actuators = store.actuators;
				prev_inputs = store.prev_inputs;
				next_inputs = store.next_inputs;
			}
	else failwith("Error, variable already existant")


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
	begin
		Random.init 1312414;
		let rand_value = Random.int bound in min + rand_value
	end
			

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
		

let rec output_management (outputs : term) (nnode : int) (env : iot_env) (nodes : node_number) : iot_env =
	match nodes with
	| NoNode -> env
	| NodeNumber(num) ->
		begin 
			match env with
				| (nodes_env, fun_env) ->
					let target_store = find_store num nodes_env in
					let own_store = find_store nnode nodes_env in
					let functions = fun_env in
					let new_trg_store =
						{
							variables = target_store.variables;
							sensors   = target_store.sensors;
							actuators = target_store.actuators;
							prev_inputs = target_store.prev_inputs;
							next_inputs = (nnode, (list_of_terms outputs own_store functions))::target_store.next_inputs;
						} in
					((update_node_store num new_trg_store nodes_env), fun_env)
		end
	| ParallelNodeNumber(num1, num2) ->
			let new_env = output_management outputs nnode env num1 in
			output_management outputs nnode new_env num2
		

let rec list_of_variables (vars : term) : (string list) =
	match vars with
	| Variable(var) -> var::[]
	| ParallelTerms(terma, termb) ->
			let first_element = list_of_variables terma in
			let second_element = list_of_variables termb in
			first_element @ second_element
	| _ -> failwith("Error: list of variables contain at least one element that is not a variable")


let rec check_input (values : value list) (constants : string list) : (bool * value list)= 
	match constants with
	| [] -> (true, values)
	| const::res_const ->
			begin
				match values with
					| frst::res_values ->
							begin
								match (const, frst) with
								| (s1, Estring(s2)) -> if (s1 = s2) then check_input res_values res_const
																			 else (false, values) 
								| (_,_) -> (false, values)
							end
					| [] -> (false, values)
			end

let rec insert_single_value (variable : string) (new_value : value) (var_store : (string * value) list) : (string * value) list =
	match var_store with
	| (frst_var, old_value)::res_var ->
		if (frst_var = variable) then
			(variable, new_value)::res_var
		else (frst_var,old_value)::(insert_single_value variable new_value res_var)
	| [] -> failwith("Error: the variable indicated in input doesn't exist")

let rec insert_values (input_vars : string list) (values : value list) (var_store : (string * value) list) : ((string * value) list) =
	match input_vars with
	| var::res_var -> 
		begin
			match values with
			| frst_val::res_val -> 
					let new_var_store = insert_single_value var frst_val var_store in
					insert_values res_var res_val new_var_store
			| [] -> failwith("Error in the input function the variables exceeds the number of input")
		end
	| [] -> var_store


let rec find_inputs (node : int) (constants : string list) (last_inputs : (int * (value list)) list) : value list = 
	match last_inputs with
	| (src_node, values)::res_inputs ->
		if (src_node = node) then
			begin
				let (res, value_lst) = check_input values constants in
				if (res = true) then
					value_lst
				else find_inputs node constants res_inputs
			end
		else find_inputs node constants res_inputs 
	| _ -> raise (ErrPatternMismatch(constants))


let input_management (src_node : int) (inputs : string list) (store : store) (constants : string list) : store =
	match constants with
	| [] -> failwith("Error: the constants of an input should have at least one element") 
	| _  -> let value_list = find_inputs src_node constants store.prev_inputs in
					{
						variables = insert_values inputs value_list store.variables;
						sensors   = store.sensors;
						actuators = store.actuators;
						prev_inputs = store.prev_inputs;
						next_inputs = store.next_inputs;
					}


let rec generate_next_input (len : int) (constants : string list) : value list =
	match constants with
	| [] -> if len = 0 then []
					else Eint(0)::(generate_next_input (len-1) constants)
	| frst::res_const -> if len = 0 then failwith("The len of the values is less than the number of constants")
											 else Estring(frst)::(generate_next_input (len-1) res_const)  


let rec input_configuration (src_node : int) (inputs : string list) (store : store) (constants : string list) : store =
	match constants with
	| [] -> failwith("Error during the configuration of the inputs")
	| _ -> let vector = generate_next_input ((List.length inputs) + (List.length constants)) constants in
					{
						variables = store.variables;
						sensors   = store.sensors;
						actuators = store.actuators;
						prev_inputs = (src_node, vector)::store.prev_inputs;
						next_inputs = store.next_inputs;
					}

let rec update_storage (variable : string)  (new_value : value) (var_store : (string * value) list) : ((string * value) list) =
	match var_store with
	| (varname, old_val)::res_store ->
			if (varname = variable) then
				(variable, new_value):: res_store
			else (varname, old_val)::(update_storage variable new_value res_store)
	| [] -> raise(ErrNotExistentVar variable)








(* Evaluation function of the abstract syntax tree*)
let rec eval_sensor (nsen : string) (store : store) (ast : sensor) : store =
	match ast with 
	| InactiveSensor -> store
	| SensorIntAction(sens) -> eval_sensor nsen store sens
	| SOpenIter(sens) -> eval_sensor nsen store sens
	| SCloseIter -> store
	| SensorStore(loc, minlim, maxlim, sens) ->
		let new_store =
			{
				variables = store.variables;
				sensors   = update_sen_store nsen minlim maxlim store.sensors;
				actuators = store.actuators;
				prev_inputs = store.prev_inputs;
				next_inputs = store.next_inputs;
			}	in
			eval_sensor nsen new_store sens


let rec eval_condition (store : store) (ast : condition) (functions : fun_env) : bool =
	match ast with
	| Bool(b) -> b
	| Greater(x, y) ->
			let x1 = eval_term store functions x in
			let y1 = eval_term store functions y in
			(x1 > y1)
	| Lower(x, y) -> 
			let x1 = eval_term store functions x in
			let y1 = eval_term store functions y in
			(x1 < y1)
	| Equal(x, y) ->
			let x1 = eval_term store functions x in
			let y1 = eval_term store functions y in
			(x1 = y1)
	| EqGr(x, y) ->
			let x1 = eval_term store functions x in
			let y1 = eval_term store functions y in
			(x1 >= y1)
	| EqLw(x, y) ->
			let x1 = eval_term store functions x in
			let y1 = eval_term store functions y in
			(x1 >= y1)
	| And(x, y) ->
			let x1 = eval_condition store x functions in
			let y1 = eval_condition store y functions in
			(x1 && y1)
	| Or(x, y) ->
			let x1 = eval_condition store x functions in
			let y1 = eval_condition store y functions in
			(x1 || y1)


let rec eval_process (nnode : int) (env : iot_env) (ast : process) : iot_env =
	match ast with
	| InactProcess -> env
	| MultiOutput(outputs, dest, res_proc) ->
			let new_env = output_management outputs nnode env dest in
			eval_process nnode new_env res_proc
	| InputProc(src_node, constants, input_vars, res_proc) ->
			begin
				match env with
				| (nodes_env, fun_env) ->
					let store = find_store nnode nodes_env in
					let input_var_lst = list_of_variables input_vars in
					let constants_lst = list_of_variables constants in
					let new_store = input_management src_node input_var_lst store constants_lst in
					let new_env = ((update_node_store nnode new_store nodes_env), fun_env) in
					eval_process nnode new_env res_proc
			end
	| ConditionProc(cond, proca, procb) ->
			begin
				match env with
				| (nodes_env, fun_env) ->
					let store = find_store nnode nodes_env in 
					let x = eval_condition store cond fun_env in
					if (x = true) then eval_process nnode env proca
					else eval_process nnode env procb 
			end
	| Assignment(x, y, res_proc) ->
			begin
				match env with
				| (nodes_env, fun_env) ->
					let store = find_store nnode nodes_env in
					let y1 = eval_term store fun_env y in
					let new_store =
						{
							variables = update_storage x y1 store.variables;
							sensors   = store.sensors;
							actuators = store.actuators;
							prev_inputs = store.prev_inputs;
							next_inputs = store.next_inputs;
						} in
					let new_env = ((update_node_store nnode new_store nodes_env), fun_env) in
					eval_process nnode new_env res_proc
			end
	| ActivateActuator(actloc, action, res_proc) ->
		begin 
			match env with
			| (nodes_env, fun_env) ->
				let store = find_store nnode nodes_env in
				let new_store =
					{
						variables = store.variables;
						sensors   = store.sensors;
						actuators = update_act_action actloc action store.actuators;
						prev_inputs = store.prev_inputs;
						next_inputs = store.next_inputs; 
					} in
				let new_env = ((update_node_store nnode new_store nodes_env), fun_env)	in
				eval_process nnode new_env res_proc
		end
	| PCloseIter -> env
	| POpenIter(res_proc) ->
			eval_process nnode env res_proc


let rec eval_component (nnode : int) (env : iot_env) (ast : component) : iot_env =
	match ast with 
	| InactiveComponent -> env
	| ParallelComponent(compa, compb) ->
			let new_env = eval_component nnode env compa in
			eval_component nnode new_env compb
	| Process(name, proc) ->
			eval_process nnode env proc
	| Sensor(loc, sens) ->
		begin
			match env with
			| (nodes_env, fun_env) -> 
				let store = find_store nnode nodes_env in 
				let new_store = eval_sensor loc store sens in
				((update_node_store nnode new_store nodes_env), fun_env)
		end
	| Actuator(loc, act) -> env


let rec eval_node_definition (env : iot_env) (ast : node) : iot_env = 
	match ast with
	| ParallelNodes(nodea, nodeb) ->
			let new_env = eval_node_definition env nodea in
			eval_node_definition new_env nodeb
	| Node(nnode, compa) ->
			eval_component nnode env compa
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
			add_new_sensor store loc
	| ActuatorDeclaration(loc) ->
			add_new_actuator store loc
	| VariableDeclaration(name) ->
			add_new_variable store name


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
						prev_inputs = [];
						next_inputs = [];
					} in
				if (check_node_existance nodes_env nnode) then
					failwith("Error, node already existant")
				else (((nnode, (eval_declaration new_store decl))::nodes_env), fun_env)
		end


let rec eval_IoTStructure (env : iot_env) (ast : iot_structure) = 
	match ast with
	| IoTStructure(decl, func, def) ->
			eval_node_definition env def


let rec configure_sequential_env (env : (int * store) list) : (int * store) list =
	match env with
	| (nnode, node_store)::res_nodes_env ->
		(nnode,
		 {
			variables = node_store.variables;
			sensors = node_store.sensors;
			actuators = node_store.actuators;
			prev_inputs = node_store.next_inputs;
			next_inputs = [];
		 })::(configure_sequential_env res_nodes_env)
	| [] -> []



let rec recursive_eval_IoTStructure (env : iot_env) (ast : iot_structure) =
	pprint_status env;
	match ast with
	| IoTStructure(_,_,_) ->
		let new_env = eval_IoTStructure env ast in
		match new_env with
		| (nodes_env, fun_env) -> let conf_env = ((configure_sequential_env nodes_env), fun_env) in
															recursive_eval_IoTStructure conf_env ast




(* First evaluation for the configuration of the environment *)
let rec frst_eval_process (nnode : int) (env : iot_env) (ast : process) : iot_env =
	match ast with
	| InactProcess -> env
	| MultiOutput(outputs, dest, res_proc) -> frst_eval_process nnode env res_proc
	| InputProc(src_node, constants, input_vars, res_proc) ->
			begin
				match env with
				| (nodes_env, fun_env) ->
					let store = find_store nnode nodes_env in
					let input_var_lst = list_of_variables input_vars in
					let constants_lst = list_of_variables constants in
					let new_store = input_configuration src_node input_var_lst store constants_lst in
					let new_env = ((update_node_store nnode new_store nodes_env), fun_env) in
					frst_eval_process nnode new_env res_proc
			end
	| ConditionProc(cond, proca, procb) ->
			let new_env = frst_eval_process nnode env proca in
			frst_eval_process nnode new_env procb
	| Assignment(x, y, res_proc) -> frst_eval_process nnode env res_proc
	| ActivateActuator(actloc, action, res_proc) ->
		begin 
			match env with
			| (nodes_env, fun_env) ->
				let store = find_store nnode nodes_env in
				let new_store =
					{
						variables = store.variables;
						sensors   = store.sensors;
						actuators = update_act_action actloc action store.actuators;
						prev_inputs = store.prev_inputs;
						next_inputs = store.next_inputs; 
					} in
				let new_env = ((update_node_store nnode new_store nodes_env), fun_env)	in
				frst_eval_process nnode new_env res_proc
		end
	| PCloseIter -> env
	| POpenIter(res_proc) ->
			frst_eval_process nnode env res_proc


let rec frst_eval_actuator (loc : string) (store : store) (ast : actuator) : store =
	match ast with
	| InactiveActuator -> store
	| ActuatorIntAction(act) -> frst_eval_actuator loc store act
	| ActuatorCommand(commands, act_proc) ->
			let new_store =
			{
				variables = store.variables;
				sensors   = store.sensors;
				actuators = update_actuator_commands loc commands store.actuators;
				prev_inputs = store.prev_inputs;
				next_inputs = store.next_inputs;
			}	in
			frst_eval_actuator loc new_store act_proc
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
			else let new_store =
				{
					variables = store.variables;
					sensors   = update_sen_store nsen minlim maxlim store.sensors;
					actuators = store.actuators;
					prev_inputs = store.prev_inputs;
					next_inputs = store.next_inputs;
				}	in
				frst_eval_sensor nsen new_store sens


let rec frst_eval_component (nnode : int) (env : iot_env) (ast : component) : iot_env =
	match ast with
	| InactiveComponent -> env
	| ParallelComponent(compa, compb) ->
			let new_env = frst_eval_component nnode env compa in
			frst_eval_component nnode new_env compb
	| Process(name, proc) ->
			frst_eval_process nnode env proc
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


let rec frst_eval_IotStructure (env : iot_env) (ast : iot_structure) =
	match ast with
	| IoTStructure(decl, func, def) ->
		let new_env = eval_node_declaration env decl in
		let new_env2 = eval_function_definition new_env func in
		frst_eval_node_definition new_env2 def