open Printf
open String
open Evaluator
open Ast

let perror_and_exit msg pos =
  failwith (msg ^ ". Print, line: " ^ string_of_int pos)

let union_sign sign1 sign2 =
  (sign2 @ (List.filter (fun x -> not(List.mem x sign2)) sign1))

let reverse_sign sign_list = 
  List.filter (fun x -> not (List.mem x sign_list)) [Pos;Neg;Zero;Error] 

let reverse_comp = function
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt

let rec sign_op env ex1 ex2 op = 
  let sign1 = sign_expr env ex1 in let sign2 = sign_expr env ex2 in
  match op with
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
  | Sub -> 
    if ex1 = ex2 then Zero::(List.filter ((=) Error) sign2) else
      (match sign1 with
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
  | Div -> 
    if (List.mem Zero sign2 ) then union_sign [Error] sign1 else union_sign sign1 sign2
  | Mod -> (match sign2 with 
      | [Zero] -> [Error]
      | _ -> [Zero; Pos])


and sign_expr env = function
  | Num n -> if n > 0 then [Pos] else if n < 0 then [Neg] else [Zero]
  | Var x -> Env.find x env
  | Op(op, ex1, ex2) -> sign_op env ex1 ex2 op

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

let is_cond_possible cond env =
  let ex1, comp, ex2 = cond in 
  let sign2 = sign_expr env ex2 in
  let sign1 = sign_expr env ex1 in
  if inter_not_possible sign1 sign2 = true then false else
    match comp with
    | Lt | Le -> inter_not_possible (greater_than sign1) sign2
    | Gt | Ge -> inter_not_possible (greater_than sign2) sign1
    | Ne -> if sign1 = [Error] && sign2 = [Error] then false else true
    | Eq -> not (inter_not_possible sign1 sign2 )

let sign_cond cond env = 
  let ex1, comp, ex2 = cond in 
  let e1,e2 = match ex1 with 
    | Var _ -> ex1,ex2
    | _ -> ex2,ex1 
  in let sign2 = sign_expr env e2 in
  let sign1 = sign_expr env e1 in
  let sign = 
    (match comp with
     | Ne -> (match sign2 with
         | [Zero] -> reverse_sign sign2
         | t -> t)
     | Lt -> (match sign2 with
         | [Zero;Neg] -> [Neg]
         | t -> t)
     | Gt -> (match sign2 with
         | [Zero;Pos] -> [Pos]
         | t -> t)
     | _ -> sign2 )
  in match e1 with
  | Var name -> Env.add name sign env
  | _ -> env



let rec sign_instr env = function
  | Set (name, e) -> Env.add name (sign_expr env e)  env 
  | Read name ->  let sign = [Pos;Neg;Zero] in
    Env.add name sign env 
  | Print e -> env
  | If (c,b1,b2) -> let e1,comp,e2  = c in 
    let cond_else = e1,reverse_comp comp,e2 in
    let if_env = sign_block b1 env in 
    let else_env = sign_block b2 env  in
    if not (is_cond_possible c env) then else_env
    else if not (is_cond_possible cond_else env) then if_env
    else let f = fun x val1 val2 -> Some (union_sign val1 val2)
      in Env.union (f) if_env else_env
  | While (c,b) as w -> env

and sign_block b env = 
  match b with
  | [] -> env
  | (pos,instr)::xs -> let new_env = sign_instr env instr in sign_block xs new_env
