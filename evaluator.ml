open Printf
open String
open Ast

let perror_and_exit msg pos =
  failwith (msg ^ ". Print, line: " ^ string_of_int pos)

let eval_op = function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Mod -> ( mod )
;;

let rec eval_expr env = function
  | Num n -> n
  | Var x -> Env.find x env
  | Op(op, ex1, ex2) -> (eval_op op) (eval_expr env ex1) (eval_expr env ex2)

let eval_comp = function
  | Eq -> ( = )
  | Ne -> ( <> )
  | Lt -> ( < )
  | Le -> ( <= )
  | Gt -> ( > )
  | Ge -> ( >= )
;;

let eval_cond cond env = 
  let ex1, comp, ex2 = cond in 
  (eval_comp comp) ex1 ex2



let rec eval_instr env = function
  | Set (name, e) -> let n = eval_expr env e in print_string name ;print_int n; Env.add name n env 
  | Read name -> let n = read_int() in Env.add name n env 
  | Print e -> let n = eval_expr env e in printf "%d\n" n; env
  | If (c,b1,b2) -> if eval_cond c env then eval_block b1 env 
  else eval_block b2 env
  | While (c,b) -> while_aux c b env
  | Comment str -> env 
and while_aux cond block env =
if eval_cond cond env then (let new_env = eval_block block env in while_aux cond block new_env)
 else env
and eval_block b env = 
    let rec aux env = function
    | [] -> env
    | (pos,instr)::xs -> let new_env = eval_instr env instr in aux new_env xs
    in aux env b
;;
let eval_polish program =
  let block,env = program in eval_block block env
  (*let print_intruction = failwith "TODO P"

    let print_block = failwith "TODO Print"
  *)
  
