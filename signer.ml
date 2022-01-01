open Printf
open String
(* open Evaluator *)
open Ast

let perror_and_exit msg pos =
  failwith (msg ^ ". Print, line: " ^ string_of_int pos)

let reverse_sign sign_list = 
  List.filter (fun x -> not (List.mem x sign_list)) [Pos;Neg;Zero;Error] 

let sign_op sign1 sign2 = function
  | Add -> (match sign1 with
      | [Pos] -> (match sign2 with 
          | [Zero] -> [Pos]
          | [Neg] -> [Neg;Zero; Pos]
          | t -> t)
      | [Neg] -> (match sign2 with 
          | [Zero] -> [Neg]
          | [Pos] -> [Neg;Zero; Pos]
          | t -> t)
      | [Error] -> [Error]
      | _ -> sign2
    )
  | Sub -> (match sign1 with
      | [Pos] -> (match sign2 with 
          | [Zero] -> [Pos]
          | [Pos] -> [Neg;Zero; Pos]
          | t -> t)
      | [Neg] -> (match sign2 with 
          | [Zero] -> [Neg]
          | [Pos] -> [Neg;Zero; Pos]
          | t -> t)
      | [Error] -> [Error]
      | _ -> sign2
    )
  | Mul -> (match sign1 with
      | [Error] -> [Error]
      | [Zero] -> [Zero]
      | _ -> sign2
    )
  | Div -> (match sign1 with
      | [Pos] -> (match sign2 with 
          | [Zero] -> [Error]
          | t -> t)
      | [Neg] -> (match sign2 with 
          | [Zero] -> [Error]
          | t -> t)
      | [Error] -> [Error]
      | [Zero] -> (match sign2 with 
          | [Zero] -> [Error]
          | _ -> [Zero])
      | _ -> sign2
    )
  | Mod -> (match sign2 with 
      | [Zero] -> [Error]
      | _ -> [Zero; Pos])


let rec sign_expr env = function
  | Num n -> if n > 0 then [Pos] else if n < 0 then [Neg] else [Zero]
  | Var x -> Env.find x env
  | Op(op, ex1, ex2) ->
    let sign1 = sign_expr env ex1 in let sign2 = sign_expr env ex2 in
    sign_op sign1 sign2 op


let sign_comp = function
  | Eq -> ( = )
  | Ne -> ( <> )
  | Lt -> ( < )
  | Le -> ( <= )
  | Gt -> ( > )
  | Ge -> ( >= )

let greater_than sign_list =
  let all = [Neg;Zero;Pos;Error] in
  match sign_list with
  | [Neg] -> all
  | [Zero] -> reverse_sign [Neg]
  | [Pos] -> [Pos;Error]
  | x -> x

let inter_not_possible sign1 sign2 = 
  let inter = List.filter (fun x -> List.mem x sign2) sign1 in 
  inter = [Error] || inter = []

let is_cond_possible sign1 comp sign2 env =
  if inter_not_possible sign1 sign2 = true then false else
    match comp with
    | Lt -> inter_not_possible (greater_than sign1 ) sign2
    | Gt -> true
    | Ne -> true
    | _-> false

let sign_cond cond env = 
  let ex1, comp, ex2 = cond in 
  let sign2 = sign_expr env ex2 in
  match comp with
  | Ne -> (match sign2 with
      | [Zero] -> reverse_sign sign2
      | t -> t)
  | Lt -> (match sign2 with
      | [Zero;Neg] -> [Neg]
      | t -> t)
  | Gt -> (match sign2 with
      | [Zero;Pos] -> [Pos]
      | t -> t)
  | _ -> sign2
(* ((sign_comp comp) (sign_expr env ex1) (sign_expr env ex2)) *)



let rec sign_instr env = function
  | Set (name, e) -> let n = sign_expr env e in Env.add name n env 
  | Read name -> let n = read_int() in let sign = sign_expr env (Num n) in
    Env.add name sign env 
  | Print e -> env
  | If (c,b1,b2) -> let sign_list = sign_cond c env in
    let env1 = sign_block b1 (Env.add "ohh" sign_list env) in
    let env2 = sign_block b2 (Env.add "ohh" (reverse_sign sign_list) env) in
    env
  | While (c,b) as w-> env

and sign_block b env = 
  match b with
  | [] -> env
  | (pos,instr)::xs -> let new_env = sign_instr env instr in sign_block xs new_env
(* in aux env b *)
