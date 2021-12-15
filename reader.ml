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

let read_name pos name =
  if List.length name = 1 then List.hd name else
    perror_and_exit "syntax error: not a valid name" pos

let read_string words_list = 
  let rec aux res = function
    | [] -> res
    | [x] -> res ^ x
    | x::xs -> aux (res^x^" ") xs
  in aux "" words_list


let check_indent pos depth line =
  let rec aux res rest = function
    | [] -> res,rest
    | x::xs -> if x <> "" then res,(x::xs) else aux (res+1) xs xs in 
  let indent,rest = aux 0 [] line in
  if indent <= depth then indent,rest else
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

let fst_line_list lb lines =
  let (pos,line) = List.hd lines in
  let line_l = split_on_char ' ' line in
  let (depth,r) = check_indent pos lb line_l in
  pos,depth,List.filter (fun x -> x <> "") r

let rec read_instr lb lines = 
  let pos,depth,words_list =  fst_line_list lb lines in
  let tail = List.tl lines in
  match List.hd words_list with
  | "READ" -> let _var_name = read_name pos (List.tl words_list)
              in Some (pos,depth,Read _var_name, tail)
  | "PRINT" -> let exp = read_expr pos (read_string (List.tl words_list))
              in Some (pos,depth,Print exp, tail)
  | "IF" ->
    let block1,rest = (read_block pos (depth+2) (tail)) 
    in let p,depth,l = (fst_line_list depth rest) 
    in let block2,rest2 = if List.length l > 0 && List.hd l = "ELSE" 
      then (read_block p (depth+2) (List.tl rest)) else [],rest in Some
    (pos,depth+2, If (read_cond pos (read_string (List.tl words_list)), block1 ,block2),rest2)
  | "WHILE" -> let block,rest = (read_block pos (depth+2) (tail)) in Some
    (pos,depth+2, While (read_cond pos (read_string (List.tl words_list)),block),rest)
  | "COMMENT" -> None
  | "ELSE" -> None
  | name -> (match List.hd (List.tl words_list) with
      | ":=" -> 
        let expr_string = read_string (List.tl (List.tl words_list)) in Some
        (pos,depth, Set(name, read_expr pos expr_string),tail)
      | _ -> perror_and_exit "syntax error in set" pos)



and read_block pos lb lines =
  let rec aux res lb ls = 
    match ls with 
    | [] -> List.rev res,[]
    | l -> let res_read = read_instr lb l in 
      if Option.is_some res_read then
        let (pos,depth,instr,rest) = Option.get res_read in
        if depth >= lb then  
          aux ((pos,instr)::res) lb rest
        else (List.rev res,l)
      else 
        let p,d,words_list =  fst_line_list lb ls in
        if (List.hd words_list = "ELSE") then 
          List.rev res, l
        else 
          aux res lb (List.tl ls)
  in aux [] lb lines



let read_lines (filename:string) =
  let ic = open_in filename in
  let read() = try Some (input_line ic) with End_of_file -> None in
  let rec loop position res = match read() with
    | Some line -> loop (position + 1) ((position, line)::res)
    | None -> List.rev res
  in loop 1 []


let read_polish (filename:string) (*: program*) = 
  let lines = read_lines filename
  in let block,rest = read_block 0 0 lines 
  in let env = Env.empty
  in block,env