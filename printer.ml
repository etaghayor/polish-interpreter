open Printf
open String
open Ast

let perror_and_exit msg pos =
  failwith (msg ^ ". Print, line: " ^ string_of_int pos)

let print_op pos op =
  match op with 
  | Add -> "+ "
  | Sub -> "- "
  | Mul -> "* "
  | Div -> "/ "
  | Mod -> "// "
  (* | _ -> perror_and_exit "syntax error: not an operator" pos *)
;;

let rec print_expr pos expr =
  match expr with
  |Num n-> string_of_int n
  |Var x-> x
  |Op(op, ex1, ex2)-> (print_op pos op) ^ "(" ^ (print_expr pos ex1) ^ ")" ^ " (" ^ (print_expr pos ex2) ^ ")"
  (* | _ -> perror_and_exit "syntax error: not an expression" pos *)
;;

let print_comp pos = function
  | Eq -> " = "
  | Ne -> " <> "
  | Lt -> " < "
  | Le -> " <= "
  | Gt -> " > "
  | Ge -> " >= "
  (* | _ -> perror_and_exit "syntax error: not a comparator" pos *)
;;

let print_cond pos cond = 
  let ex1, comp, ex2 = cond in 
  (print_expr pos ex1) ^ (print_comp pos comp) ^ (print_expr pos ex2)
;;
(*let print_intruction = failwith "TODO P"

let print_block = failwith "TODO Print"
*)


let comp = Eq;;
printf "%s\n" (print_comp 0 comp);;

let exp1 = Op(Add, Num 2, Num 1);;
let exp2 = Op(Mul, Num 3, exp1);;
printf "%s\n" (print_expr 0  exp1);;
printf "%s\n" (print_expr 0 exp2);;

let cond = exp1, Lt, exp2;;
printf "%s\n" (print_cond 0 cond);;