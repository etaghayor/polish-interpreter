open Ast
open String
open Str


(* auxilary functions *)
let perror_and_exit msg pos =
  failwith (msg ^ ". line: " ^ string_of_int pos)

let contains_sub s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false


(* functions to read from file *)

let read_var_name pos name = 
  if List.length name = 1 then List.hd name else
    perror_and_exit "syntax error: not a valid name" pos


let check_indent pos depth line =
  let rec aux k l = match k,l with
    | 0 , x::xs -> ((if x <> "" then depth else depth+1) ,pos, List.filter (fun x -> x <> "") xs)
    | n , x::xs -> if x = "" then aux (n-1) xs else (depth-n,pos,line)
    | n , [] -> (depth-n,pos,line)
  in aux depth line


let read_op pos op =
  match op with 
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _ -> perror_and_exit "syntax error: not an operator" pos


let read_expr pos expr =
  let splitted = split_on_char ' ' expr in
  let rec aux = function
    | [] -> perror_and_exit "not a valid expression" pos
    | x::y -> match read_op pos x with
      | exception _ -> (match x with
          | n -> Num (int_of_string n),y
          | exception _ -> Var x,y)
      | op -> (let ex1,rest1 = aux y in 
               let ex2,rest2 = aux rest1 in
               Op (op,ex1,ex2),rest2)
  in let res,rest = aux splitted in
  if rest <> [] then perror_and_exit "too many arguments for operator" pos else res

let read_comp pos = function
  | " = " -> Eq
  | " <> " -> Ne
  | " < " -> Lt
  | " <= " -> Le
  | " > " -> Gt
  | " >= " -> Ge
  | _ -> perror_and_exit "syntax error: not a comparison function" pos

let aux_read_cond pos e d = 
  let elist = split (regexp_string d) e in
  read_expr pos (List.hd elist) , d, read_expr pos (List.nth elist 1) 

let read_cond pos e = 
  let e1,c,e2 = aux_read_cond pos e
      (if contains_sub e " = " then " = "  else 
       if contains_sub e " <> " then " <> " else
       if contains_sub e " < " then " < " else
       if contains_sub e " <= " then " <= " else
       if contains_sub e " > " then " > " else
       if contains_sub e " => " then " => " else
         perror_and_exit "not a valid condition" pos)
  in (e1,read_comp pos c, e2) 

let read_instr pos lb line =
  let depth,pos,line = check_indent pos lb line in
  if depth > lb then perror_and_exit "indentation bug" pos else
    match List.hd line with
    | "READ" -> let name = List.tl line in (depth,Read (read_var_name pos name))
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
    | (p,x)::xs -> let depth,instr = read_instr p last_b (split_on_char ' ' x) in 
      aux depth ((p,instr)::res) xs
    | [] -> res
  in aux 0 [] lines