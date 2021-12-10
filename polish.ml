open Ast
open Reader
open Evaluator
open Printer

(** Note : cet embryon de projet est pour l'instant en un seul fichier*)

let print_polish p = let block, env = p in Printf.printf "%s" (print_block 0 block);;

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> let env = eval_polish (read_polish file) in ()
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
