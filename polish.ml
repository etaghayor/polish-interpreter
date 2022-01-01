open Ast
open Reader
open Evaluator
open Printer
open Simplifier 
open Analyser

let print_set = Names.iter (Printf.printf "%s, ")

let eval_polish p =
  let block,env = p in eval_block block env

let print_polish p = 
  let block, env = p in Printf.printf "%s" (print_block 0 block);;

let simpl_polish p = 
  let block, env = p in Printf.printf "%s" (print_block 0 (simpl_block block));;

let vars_polish p =
  let block,env = p in
  analyse_block Names.empty Names.empty Names.empty block

(* 
let sign_polish p = 
  let block,env = p in
  let vars,good_vars,set_vars = analyse_block Names.empty Names.empty Names.empty block
  in Names.iter (Printf.printf "%s, ") vars;
  Names.iter (Printf.printf "%s, ") good_vars;
  Names.iter (Printf.printf "%s, ") set_vars;
  let bad_vars = (Names.diff good_vars set_vars)
  in vars,bad_vars *)

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: \n-reprint : reafficher le program\n-eval : evaluer le program"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> let env = eval_polish (read_polish file) in ()
  | [|_;"-simpl";file|] -> simpl_polish (read_polish file)
  | [|_;"-vars";file|] ->  let all,bvars,_ = vars_polish (read_polish file) 
    in Names.iter (Printf.printf "%s, ") all;
    print_string "\nbads: ";
    Names.iter (Printf.printf "%s, ") bvars;
    print_string "\n"
  (* |  [|_;"-sign";file|] -> let v,v2 = sign_polish (read_polish file) in () *)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
