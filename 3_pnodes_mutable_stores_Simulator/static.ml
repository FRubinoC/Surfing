open Ast

type op = Input | Output
type state = Hot | Cold
type io_op = int * ((op * int) list) * state
type nodes_io =  io_op list

exception ErrNodeUndefined of int
exception ErrNotDefinedSensor of int * string
exception ErrNotDefinedRangeSen of int * string
exception ErrNotDefinedActuatorOptions of int * string
exception ErrNotDefinedActuator of int * string
exception ErrIODeadNode of int
exception ErrDeadNodeOp of int
exception ErrDeadlock of int * op * int


let rec take_node_ast (nnode : int) (ast : Ast.node) : (bool * Ast.component) =
  match ast with
  | Node(num, comp) -> if (num = nnode) then (true, comp)
                       else (false, comp)
  | ParallelNodes(node1, node2) ->
      let (res2, comp2) = take_node_ast nnode node2 in
      if (res2 = true) then (true, comp2)
      else take_node_ast nnode node1
  | InactNode -> (false, InactiveComponent)



(* ******************** NODE CHECKER ********************)
let rec check_node_existence (nnode : int) (ast : Ast.node) : bool =
  match ast with
  | InactNode -> false
  | Node(num, node_def) -> 
      if (nnode = num) then true else false
  | ParallelNodes(nodea, nodeb) ->
      (check_node_existence nnode nodea) || (check_node_existence nnode nodeb)


let rec check_node_definition (nodes_store : Ast.nodes_env) (ast : Ast.iot_structure) : bool =
  match nodes_store with 
  | (nnode, node_store)::res_stores -> 
      check_node_definition res_stores ast &&
      begin 
        match ast with
        | IoTStructure(_,_,def) -> 
          if (check_node_existence nnode def) then true
          else raise (ErrNodeUndefined(nnode))
      end
  | [] -> true 




(* ****************** SENSOR CHECKER *******************)
let rec check_sensor_range_def (ast : Ast.sensor) : bool =
  match ast with
  | SensorIntAction(sen) -> check_sensor_range_def sen
  | SOpenIter(sen) -> check_sensor_range_def sen
  | SCloseIter -> false
  | SensorStore(_, _, _, _) -> true
  | InactiveSensor -> false


let rec find_sen_def (nnode : int) (senname : string) (ast : Ast.component) : bool =
  match ast with
  | ParallelComponent(compa, compb) ->
      find_sen_def nnode senname compa || find_sen_def nnode senname compb
  | Sensor(name, sen) ->
      if (name = senname) then
        begin 
          if (check_sensor_range_def sen) then true
          else raise (ErrNotDefinedRangeSen(nnode, senname))
        end
      else false
  | _ -> false


let rec check_node_sen_def (nnode : int) (sen_store : (string * Ast.value) list) (ast : Ast.component) : bool =
  match sen_store with
  | (senname, value)::res_sens ->
      check_node_sen_def nnode res_sens ast &&
      begin
        if (not (find_sen_def nnode senname ast)) then
          raise (ErrNotDefinedSensor (nnode, senname))
        else true
      end
  | [] -> true


let rec check_sensors_definition (env : Ast.nodes_env) (ast : Ast.iot_structure) : bool =
  match env with
  | ((nnode, node_store)::res_stores) -> 
      check_sensors_definition res_stores ast &&
      begin
        match ast with
        | IoTStructure(_,_,node_def) ->
          let (res, node_ast) = take_node_ast nnode node_def in
          if (res = false) then raise (ErrNodeUndefined(nnode))
          else check_node_sen_def nnode node_store.sensors node_ast
      end 
  | [] -> true





(* ********************** ACTUATOR CHECKER **********************)
let rec check_actuator_options_def (ast : Ast.actuator) : bool =
  match ast with
  | InactiveActuator -> false
  | ActuatorIntAction(act) -> check_actuator_options_def act
  | ActuatorCommand(_, _) -> true
  | ACloseIter -> false
  | AOpenIter(act) -> check_actuator_options_def act


let rec find_act_def (nnode : int) (actname : string) (ast : Ast.component) : bool =
  match ast with
  | ParallelComponent(compa, compb) ->
      find_act_def nnode actname compa || find_act_def nnode actname compb
  | Actuator(name, act) ->
      if (name = actname) then
        begin
          if (check_actuator_options_def act) then true
          else raise (ErrNotDefinedActuatorOptions(nnode, actname))
        end
      else false
  | _ -> false


let rec check_node_act_def (nnode : int) (act_store : (string * (string list)) list) (ast : Ast.component) : bool=
  match act_store with
  | (actname, actions)::res_acts ->
      check_node_act_def nnode res_acts ast &&
      begin
        if (not (find_act_def nnode actname ast)) then
          raise (ErrNotDefinedActuator (nnode, actname))
        else true
      end
  | [] -> true


let rec check_actuator_definition (env : Ast.nodes_env) (ast : Ast.iot_structure) : bool =
  match env with
  | ((nnode, node_store)::res_stores) ->
      check_actuator_definition res_stores ast &&
      begin
        match ast with
        | IoTStructure(_,_,node_def) -> 
          let (res, node_ast) = take_node_ast nnode node_def in
          if (res = false) then raise (ErrNodeUndefined(nnode))
          else check_node_act_def nnode node_store.actuators node_ast
      end
  | [] -> true



(* *************** DEADLOCK CHECKER *************** *)

let rec build_node_list (env : Ast.nodes_env) : (int list) =
  match env with
  | (nnode, store)::res_nodes ->
      nnode::(build_node_list res_nodes)
  | [] -> []


let rec build_deadlock_outputs (nums : Ast.node_number) : (op * int) list =
  match nums with
  | NoNode -> []
  | ParallelNodeNumber(num1, num2) ->
      (build_deadlock_outputs num1) @ (build_deadlock_outputs num2)
  | NodeNumber(num) -> (Output, num) :: [] 


let rec take_process_io_list (ast : Ast.process) : (op * int) list =
  match ast with
  | InactProcess -> []
  | PCloseIter -> []
  | POpenIter(proc) -> take_process_io_list proc
  | Assignment(_,_,proc) -> take_process_io_list proc
  | ActivateActuator(_,_,proc) -> take_process_io_list proc
  | ConditionProc(_, proca, procb) -> take_process_io_list proca
  | InputProc(src,_,_,proc) -> (Input, src)::(take_process_io_list proc)
  | MultiOutput(_, nums, proc) -> (build_deadlock_outputs nums) @ (take_process_io_list proc) 


let rec take_io_list (ast : Ast.component) : (op * int) list =
  match ast with
  | Process(name, proc) -> take_process_io_list proc
  | ParallelComponent(compa, compb) -> 
      (take_io_list compa) @ (take_io_list compb)
  | _ -> []


let rec build_deadlock_env (nodes_list : int list) (ast : Ast.node) : (nodes_io) =
  match nodes_list with
  | node_num::res_list ->
      let (found, node_ast) = take_node_ast node_num ast in
      if (found = false) then failwith("There exists a node that is been declared but not defined")
      else
        begin
          let io_list = take_io_list node_ast in
          (node_num, io_list, Cold)::(build_deadlock_env res_list ast)
        end
  | [] -> []


let rec take_node_io (nnode : int) (env : nodes_io) : io_op =
  match env with
  | (num, ops, state)::res_env ->
      if (nnode = num) then (num, ops, state)
      else take_node_io nnode res_env
  | [] -> raise (ErrIODeadNode(nnode))


let rec read_state (dst_env : io_op) : state =
  match dst_env with
  | (num, ops, state) -> state 


let rec take_first_op (node_env : io_op) : (op * int) =
  match node_env with
  | (num, ops, state) ->
    begin
      match ops with
      | (op, dst)::res_ops -> (op, dst)
      | [] -> raise (ErrDeadNodeOp(num))
    end


let rec remove_node_op (ops : (op * int) list) : (op * int) list =
  match ops with
  | (op, node2)::res_ops -> res_ops
  | [] -> failwith("There is a list of io operation that is empty")


(* One effect of this function is to set as Cold the state of the node *)
let rec remove_frst_op (dst : int) (env : nodes_io) : nodes_io =
  match env with
  | (io_dst, ops, state)::res_nodes_io ->
      if io_dst = dst then
        begin 
          let new_ops = remove_node_op ops in
          if (new_ops = []) then res_nodes_io
          else (dst, new_ops, Cold)::res_nodes_io
        end
      else (io_dst, ops, state)::(remove_frst_op dst res_nodes_io)
  | [] -> raise (ErrDeadNodeOp(dst))
    

let rec cut_env (env : nodes_io) (dst : int) : nodes_io =
  match env with
  | (node, ops, state)::res_env ->
      if (node = dst) then res_env
      else (node, ops, state)::(cut_env res_env dst)
  | [] -> failwith("Deadlock checking: the environment can't be cut because the destination doesn't exist") 
  

let rec take_new_node (env : nodes_io) : (int * ((op * int) list) * state * nodes_io) =
  match env with
  | (node, ops, state)::res_env -> (node, ops, state, res_env) 
  | [] -> failwith("Deadlock checking: the environment of the nodes is empty")  


let rec check_deadlock_ops (nnode : int) (ops : (op * int) list) (state : state) (env : nodes_io) : bool =
  match ops with
  | (op, dst)::res_ops ->
      begin
        let dst_env = take_node_io dst env in
        begin
          let (dst_frst_op, dst_op_dst) = take_first_op dst_env in
          match op with
          | Input -> 
              if (dst_frst_op = Output) && (dst_op_dst = nnode) then
                let new_env = remove_frst_op dst env in
                check_deadlock_ops nnode res_ops state new_env
              else 
                if (read_state dst_env = Hot) then raise (ErrDeadlock(nnode, op, dst))
                else 
                  let new_env = (nnode, ops, Hot)::(cut_env env dst) in
                  begin
                    match dst_env with
                      | (num, dst_ops, dst_state) ->
                          check_deadlock_ops num dst_ops dst_state new_env
                  end
          | Output ->
              if (dst_frst_op = Input) && (dst_op_dst = nnode) then
                let new_env = remove_frst_op dst env in
                check_deadlock_ops nnode res_ops state new_env
              else 
                if (read_state dst_env = Hot) then raise (ErrDeadlock(nnode, op, dst))
                else 
                  let new_env = (nnode, ops, Hot)::(cut_env env dst) in
                  begin
                    match dst_env with
                      | (num, dst_ops, dst_state) ->
                          check_deadlock_ops num dst_ops dst_state new_env 
                  end
        end
      end
  | [] ->
      begin
        if (env = []) then true (* There are not deadlock in the network *)
        else
          let (new_node, new_ops, new_state, new_env) = take_new_node env in
          check_deadlock_ops new_node new_ops new_state new_env
      end
        




let rec deadlock_analysis (env : nodes_io) : bool =
  match env with
  | (nnode, ops, state)::res_env ->
      check_deadlock_ops nnode ops state res_env 
  | [] -> true


let check_deadlock (env : Ast.nodes_env) (ast : Ast.iot_structure) : bool =
  match ast with
  | IoTStructure(_,_,def) ->
    let nodes_list = build_node_list env in 
    let deadlock_env = build_deadlock_env nodes_list def in
    deadlock_analysis deadlock_env 



(* ************** STATIC ANALYZER MAIN CODE ***************)
let static_analyzer (env : Ast.iot_env) (ast : Ast.iot_structure) : bool =
  match env with
  | (nodes_store, fun_env) ->
      check_node_definition nodes_store ast &&
      check_sensors_definition nodes_store ast &&
      check_actuator_definition nodes_store ast &&
      check_deadlock nodes_store ast