open Ast
open String

let read_op = function
  | _ -> failwith "TODO"

let read_expr line = failwith "TODO"


let check_indent depth pos line =
  let rec aux k l = match k,l with
    | 0 , x::xs -> ((if x <> "" then depth else depth+1) ,pos, List.filter (fun x -> x <> "") xs)
    | n , x::xs -> if x = "" then aux (n-1) xs else (depth-n,pos,line)
    | n , [] -> (depth-n,pos,line)
  in aux depth line

let check_name name = List.length name = 1 

let read_instr lb pos line =
  let depth,pos,line = check_indent lb pos line in
  if depth > lb then failwith ("indentation bug: line " ^ (string_of_int pos)) else
    match List.hd line with
    | "READ" -> let name = List.tl line in 
      if check_name name then (depth,pos,Read (List.hd name)) else failwith "NOT INSTRUCTION"
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
    | (p,x)::xs -> let depth,lb,instr = read_instr last_b p (split_on_char ' ' x) in 
      aux lb ((p,instr)::res) xs
    | [] -> res
  in aux 0 [] lines