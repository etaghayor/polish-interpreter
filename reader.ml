open Ast
open String

let read_op = function
  | _ -> failwith "TODO"

let read_expr line = failwith "TODO"


let check_indent lb lines =
  let rec aux k lines = match k,lines with
    | 0,x::xs -> (x != " ",xs)
    | n ,x::xs -> if x = " " then aux (k-1) xs else (false,[])
    | n, [] -> (false,[])
  in aux lb lines

let read_instr lb line =
  let predicate,lines = check_indent lb line in
  if not predicate then failwith "indentation bug" else
    match List.hd lines with
    | "READ" -> failwith "TODO"
    | "PRINT" -> failwith "TODO"
    | "IF" -> failwith "TODO"
    | "WHILE" -> failwith "TODO"
    | "COMMENT" -> failwith "TODO"
    | _ -> failwith "TODO SET"


let read_block b = failwith "TODO"

let read_lines (filename:string) =
  let ic = open_in filename in
  let read() = try Some (input_line ic) with End_of_file -> None in
  let rec loop position res = match read() with
    | Some line -> loop (position + 1) ((position, line)::res)
    | None -> res
  in loop 1 []

let read_polish (filename:string) (*: program*) = 
  let lines = read_lines filename in
  let rec aux last_b res = function
    | (p,x)::xs -> let lb,instr = read_instr last_b (split_on_char ' ' x) in 
      aux lb (instr::res) xs
    | [] -> res
  in aux 0 [] lines