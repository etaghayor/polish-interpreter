open Ast
open String
open Str


(* auxilary functions *)
let perror_and_exit msg pos =
  failwith (msg ^ ". Read, line: " ^ string_of_int pos)

let contains_sub s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false


(* functions to read from file *)

let read_name pos name =
  if List.length name = 1 then List.hd name else
    perror_and_exit "syntax error: not a valid name" pos

let read_string words_list = 
  let rec aux res = function
    | [] -> res
    | [x] -> res ^ x
    | x::xs -> aux (res^x^" ") xs
  in aux "" (List.tl words_list)


let check_indent pos depth line =
  let rec aux res = function
    | [] -> res
    | x::xs -> if x <> "" then res else aux (res+1) xs in 
  let indent = aux 0 line in
  if indent <= depth then indent else
    perror_and_exit "indentation problem" pos


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
      | exception _ -> (match Num (int_of_string x),y with
          | n -> n
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

let read_instr pos lb words_list =
  let depth = check_indent pos lb words_list in
  (* if depth > lb then perror_and_exit "indentation bug" pos else *)
  match List.hd words_list with
  | "READ" -> let name = List.tl words_list in (depth,Read (read_name pos name))
  | "PRINT" -> (depth,Print(read_expr pos (read_name pos (List.tl words_list))))
  | "IF" -> (depth+2, If (read_cond pos (read_string words_list),[],[]))
  | "WHILE" -> (depth+2, While (read_cond pos (read_string words_list),[]))
  | "COMMENT" -> (depth, Comment (read_string words_list))
  | _ -> failwith "TODO SET"



let read_block b = failwith "TODO"

let read_lines (filename:string) =
  let ic = open_in filename in
  let read() = try Some (input_line ic) with End_of_file -> None in
  let rec loop position res = match read() with
    | Some line -> loop (position + 1) ((position, line)::res)
    | None -> List.rev res
  in loop 1 []


let read_polish (filename:string) (*: program*) = 
  let lines = read_lines filename in
  let rec aux last_b res = function
    | (p,x)::xs -> let (depth,instr) = read_instr p last_b (split_on_char ' ' x) in 
      aux depth ((p,instr)::res) xs
    | [] -> res
  in aux 0 [] lines