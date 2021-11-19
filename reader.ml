open Ast
open String
open Str

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

let read_op op =
  match op with 
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _ -> Not_op

 
let read_expression expr res=
  let splitted = split_on_char " " expr in
  let res = None in
  match splitted with
  |None -> res
  |x::y -> if (read_op x) <> Not_op then 
    match int_of_string x
    | n -> Num n
    | exception _ -> Var x
else
  Op read_op x match y with
  | [] | [x] -> failwith "NOOOOO"
      | x1::x2::xs -> read_expression x1 

  (read_expression [List.hd y]) (read_expression (List.tl y))

let read_instr lb pos line =
  let depth,pos,line = check_indent lb pos line in
  if depth > lb then failwith ("indentation bug: line " ^ (string_of_int pos)) else
    match List.hd line with
    | "READ" -> let name = List.tl line in 
      if check_name name then (depth,Read (List.hd name)) else failwith "NOT INSTRUCTION"
    | "PRINT" -> let expr = List.tl line in
     depth, Print(List.hd expr)
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
    | (p,x)::xs -> let depth,instr = read_instr last_b p (split_on_char ' ' x) in 
      aux lb ((p,instr)::res) xs
    | [] -> res
  in aux 0 [] lines