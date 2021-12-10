open Printf
open String
open Ast

let print_op = function
  | Add -> "+ "
  | Sub -> "- "
  | Mul -> "* "
  | Div -> "/ "
  | Mod -> "% "
;;

let rec print_expr = function
  |Num n-> string_of_int n
  |Var x-> x
  |Op(op, ex1, ex2)-> (print_op op) ^ (print_expr ex1) ^ " " ^ (print_expr ex2)
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
  | If (cond, block1, block2) -> let ins2 = print_block (indent+2) block2 in
    if (ins2 != "") then
      print_indent indent ^ "IF " ^ print_cond cond ^ "\n" ^
      print_block (indent+2) block1 ^
      print_indent indent ^ "ELSE" ^ "\n" ^ 
      ins2
    else 
      print_indent indent ^ "IF " ^ print_cond cond ^ "\n" ^
      print_block (indent+2) block1 
  | While (cond, block) ->
    print_indent indent ^ "WHILE " ^ print_cond cond ^ "\n" ^
    print_block (indent+2) block
  | Comment name -> print_indent indent ^ "COMMENT " ^ name ^ "\n"

and print_block indent = function
  |[] -> ""
  |(pos, ins) :: tl -> print_intruction indent ins ^ print_block indent tl
;;

let print_polish p = let block, env = p in Printf.printf "%s" (print_block 0 block);;
