open Ast
open Reader
open Evaluator
open Printer
open Simplifier 
open Analyser
open Signer

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

let sign_polish p = 
  let block,_ = p in
  let env = sign_block block Env.empty in
  Env.iter (fun key v -> Printf.printf "%s : " key;
             print_type v;
             print_string "\n") env
;;

let copy p dest = 
  let block, env = p in 
  let to_write = print_block 0 block in
  let oc = open_out dest in 
    Printf.fprintf oc "%s" to_write; (* write something *)   
    close_out oc;   
;;

let simpl_copy p dest = 
  let block, env = p in
  let to_write = print_block 0 (simpl_block block) in
  let oc = open_out dest in 
    Printf.fprintf oc "%s" to_write; (* write something *)   
    close_out oc;   
;;

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
    print_string "\n";
  | [|_;"-sign";file|] -> sign_polish (read_polish file)
  | [|_;"-copy";file; dest|] -> copy (read_polish file) dest
  | [|_;"-simpCopy"; file; dest|] -> simpl_copy (read_polish file) dest
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
