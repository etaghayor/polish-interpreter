open Ast
open String

let read_op = function
  | _ -> failwith "TODO"

let read_expr line = failwith "TODO"


let check_indent lb pos line =
  let rec aux k l = match k,l with
    | 0 , x::xs -> ((if x <> "" then lb else lb+1) ,pos, List.filter (fun x -> x <> "") xs)
    | n , x::xs -> if x = "" then aux (n-1) xs else (n,pos,line)
    | n , [] -> (n,pos,line)
  in aux lb line

let check_name name = failwith "TODO"

let read_instr lb pos line =
  let depth,pos,line = check_indent lb pos line in
  if depth > lb then failwith ("indentation bug: line " ^ (string_of_int pos)) else
    match List.hd line with
    | "READ" -> (lb,List.tl line)
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
    | (p,x)::xs -> let lb,instr = read_instr last_b p (split_on_char ' ' x) in 
      aux lb ((p,instr)::res) xs
    | [] -> res
  in aux 0 [] lines