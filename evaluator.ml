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

let rec eval_expr = function
  | Num n -> n
  | Var x-> 1
  | Op(op, ex1, ex2)-> (eval_op op) (eval_expr ex1) (eval_expr ex2)

let eval_comp = function
  | Eq -> ( = )
  | Ne -> ( <> )
  | Lt -> ( < )
  | Le -> ( <= )
  | Gt -> ( > )
  | Ge -> ( >= )
;;

let eval_cond pos cond = 
  let ex1, comp, ex2 = cond in 
  (eval_comp comp) ex1 ex2
(*let print_intruction = failwith "TODO P"

  let print_block = failwith "TODO Print"
*)
