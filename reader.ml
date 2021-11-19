open Ast
open String
open Str


let contains_sub s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

let read_var_name name = 
  if List.length name = 1 then List.hd name else failwith "syntax error: not a valid name"


let check_indent depth pos line =
  let rec aux k l = match k,l with
    | 0 , x::xs -> ((if x <> "" then depth else depth+1) ,pos, List.filter (fun x -> x <> "") xs)
    | n , x::xs -> if x = "" then aux (n-1) xs else (depth-n,pos,line)
    | n , [] -> (depth-n,pos,line)
  in aux depth line


let read_op op =
  match op with 
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _ -> failwith "Not_op"


let read_expr expr =
  let splitted = split_on_char ' ' expr in
  let rec aux = function
    | [] -> failwith "not a valid excpression"
    | x::y -> match read_op x with
      | exception _ -> (match x with
          | n -> Num (int_of_string n)
          | exception _ -> Var x)
      | _ -> Op (read_op x,aux y,aux y)
  in aux splitted

let read_comp = function
  | " = " -> Eq
  | " <> " -> Ne
  | " < " -> Lt
  | " <= " -> Le
  | " > " -> Gt
  | " >= " -> Ge
  | _ -> failwith "NOT A VALID OPERATOR"

let aux_read_cond e d = 
  let elist = split (regexp_string d) e in
  read_expr (List.hd elist), d, read_expr (List.nth elist 1)

let read_cond e = 
  let e1,c,e2 = aux_read_cond e
      (if contains_sub e " = " then " = "  else 
       if contains_sub e " <> " then " <> " else
       if contains_sub e " < " then " < " else
       if contains_sub e " <= " then " <= " else
       if contains_sub e " > " then " > " else
       if contains_sub e " => " then " => " else
         failwith "NOT A VALID CONDITION" )
  in (e1,read_comp c, e2) 

let read_instr lb pos line =
  let depth,pos,line = check_indent lb pos line in
  if depth > lb then failwith ("indentation bug: line " ^ (string_of_int pos)) else
    match List.hd line with
    | "READ" -> let name = List.tl line in (depth,Read (read_var_name name))
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
    | (p,x)::xs -> let depth,instr = read_instr last_b p (split_on_char ' ' x) in 
      aux depth ((p,instr)::res) xs
    | [] -> res
  in aux 0 [] lines