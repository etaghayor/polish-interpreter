open Ast
open Reader
open Evaluator
open Printer
open Simplifier

let eval_polish program =
  let block,env = program in eval_block block env

let print_polish p = 
  let block, env = p in Printf.printf "%s" (print_block 0 block);;

let simpl_polish p = 
  let block, env = p in Printf.printf "%s" (print_block 0 (simpl_block block));;

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: \n-reprint : reafficher le program\n-eval : evaluer le program"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> let env = eval_polish (read_polish file) in ()
  | [|_;"-simpl";file|] -> simpl_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
