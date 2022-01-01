open Printf
open String
open Ast

let perror_and_exit msg pos =
  failwith (msg ^ ". Print, line: " ^ string_of_int pos)

let analyse_op = function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Mod -> ( mod )


let rec analyse_expr vars good_vars = function
  | Var name -> if Names.mem name good_vars
    then vars,good_vars
    else Names.add name vars, good_vars
  | Op (op,e1,e2) -> let vars1,gvars1 = analyse_expr vars good_vars e1 in 
    let vars2,gvars2 = analyse_expr vars1 good_vars e2 in vars2,good_vars
  | _ -> vars,good_vars

let analyse_comp = function
  | Eq -> ( = )
  | Ne -> ( <> )
  | Lt -> ( < )
  | Le -> ( <= )
  | Gt -> ( > )
  | Ge -> ( >= )


let analyse_cond cond vars = 
  let ex1, comp, ex2 = cond in 
  ((analyse_comp comp) (analyse_expr vars ex1) (analyse_expr vars ex2))


let rec analyse_instr vars good_vars = function
  | Set (name, e) -> Names.add name vars,Names.add name good_vars
  | Read name -> Names.add name vars,Names.add name good_vars
  | Print e -> analyse_expr vars good_vars e
  | If (c,b1,b2) -> let vars1,good_vars1 = analyse_block  vars good_vars b1 
    in let vars2,good_vars2 = analyse_block vars1 good_vars1 b2
    in vars2,Names.inter good_vars2 good_vars1
  | While (c,b) as w -> analyse_block vars good_vars b
and analyse_block  vars good_vars =  function
  | [] -> vars,good_vars
  | (pos,instr)::xs -> let new_vars,new_good_vars = analyse_instr vars good_vars instr
    in analyse_block new_vars new_good_vars xs
(* in aux vars good_vars block *)
(* print_int (Names.cardinal vars);vars *)
(* 
let rec analyse_instr vars = function
  | Set (name, e) -> let n = analyse_expr vars e in Names.add name n vars 
  | Read name -> let n = read_int() in Names.add name n vars 
  | Print e -> let n = analyse_expr vars e in printf "%d\n" n; vars
  | If (c,b1,b2) -> if analyse_cond c vars then analyse_block b1 vars
  else analyse_block b2 vars
  | While (c,b) as w-> while_aux c b vars
  and while_aux cond block vars =
if analyse_cond cond vars then (let new_vars = analyse_block block vars in while_aux cond block new_vars)
 else vars
and analyse_block b vars = 
    let rec aux vars = function
    | [] -> vars
    | (pos,instr)::xs -> let new_vars = analyse_instr vars instr in aux new_vars xs
    in aux vars b *)
