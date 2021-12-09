open Printf
open String
open Ast

let print_op = function
  | Add -> "+ "
  | Sub -> "- "
  | Mul -> "* "
  | Div -> "/ "
  | Mod -> "// "
;;

let rec print_expr = function
  |Num n-> string_of_int n
  |Var x-> x
  |Op(op, ex1, ex2)-> (print_op op) ^ "(" ^ (print_expr ex1) ^ ")" ^ " (" ^ (print_expr ex2) ^ ")"
;;

let print_comp = function
  | Eq -> " = "
  | Ne -> " <> "
  | Lt -> " < "
  | Le -> " <= "
  | Gt -> " > "
  | Ge -> " >= "
;;

let print_cond cond = 
  let ex1, comp, ex2 = cond in 
  (print_expr ex1) ^ (print_comp comp) ^ (print_expr ex2)
;;

let rec print_indent indent = match indent with
  | 0 -> ""
  | _ -> " " ^ print_indent (indent - 1)
;;

let rec print_intruction indent = function
  | Set (name, expr) -> print_indent indent ^ name ^ " := " ^ print_expr expr ^ "\n"
  | Read name -> print_indent indent ^ "READ " ^ name ^ "\n"
  | Print expr -> print_indent indent ^ "PRINT " ^ print_expr expr ^ "\n"
  | If (cond, block1, block2) -> print_indent indent ^ "IF " ^ print_cond cond ^ "\n" ^
    print_block (indent+2) block1 ^ "\n" ^
    print_indent indent ^ "ELSE " ^ "\n" ^ 
    print_block (indent+2) block2 ^ "\n"
  | While (cond, block) ->
    print_indent indent ^ "WHILE " ^ print_cond cond ^ "\n" ^
    print_block (indent+2) block
  | Comment name -> print_indent indent ^ "COMMENT" ^ name ^ "\n"

and print_block indent = function
  |[] -> ""
  |(pos, ins) :: tl -> print_intruction indent ins ^ print_block indent tl
;;


(*let comp = Eq;;
(*printf "%s\n" (print_comp comp);;*)

let exp1 = Op(Add, Num 2, Num 1);;
let exp2 = Op(Mul, Num 3, exp1);;
(*printf "%s\n" (print_expr   exp1);;
printf "%s\n" (print_expr  exp2);;*)

let cnd = exp1, Lt, exp2;;
(*printf "%s\n" (print_cond  cond);;*)


let ins1 = Print (exp1);;
let ins2 = Set ("x", exp1);;
let ins3 = While (cnd, [(4, ins2); (5, ins1)]);;
let blk = [(1, ins1); (2, ins3); (3, ins2)];;
printf "%s" (print_block 0 blk);;*)