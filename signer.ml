open Printf
open String
open Evaluator
open Ast


let print_type = 
  List.iter (fun x -> print_string 
                (match x with
                 | Zero -> "0 "
                 | Pos -> "+ " 
                 | Neg -> "- "
                 | _ -> "! "))

let compare s1 s2 =
  if s1 = s2 then 0 else
    match s1 with 
    | Neg ->  -1
    | Zero -> if s2 = Neg then 1 else -1
    | Pos -> if s2 = Neg || s2 = Zero then 1 else -1
    | _ -> 1

let union_sign sign1 sign2 =
  List.sort_uniq (compare) (sign2 @ sign1)

let intersection sign1 sign2 =
  List.filter (fun x -> List.mem x sign2) sign1

let reverse_sign sign_list = 
  List.filter (fun x -> not (List.mem x sign_list)) [Neg;Zero;Pos;Error] 

let reverse_comp = function
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt

let sign_add sign1 sign2 =
  let uni = union_sign sign1 sign2 in
  if(List.mem Pos uni && List.mem Neg uni) then union_sign uni [Zero] else uni


let sign_sub sign1 sign2 =
  let uni = union_sign sign1 sign2 in
  if (List.mem Pos uni && List.mem Pos uni)|| (List.mem Neg uni && List.mem Neg uni) 
  then union_sign uni [Zero] else uni


let sign_mudi sign1 sign2=
  let uni = union_sign sign1 sign2 in
  if (List.mem Zero sign2) then union_sign [Error] uni else
  if (List.mem Pos sign1 && List.mem Neg sign1)
  || (List.mem Pos sign2 && List.mem Neg sign2) 
  || (List.mem Pos sign1 && List.mem Pos sign2) then uni else
  if (List.mem Pos sign1 && List.mem Neg sign2)
  ||(List.mem Pos sign2 && List.mem Neg sign1) then
    List.filter ((<>) Pos) uni else uni

let sign_mul sign1 sign2 =
  if (sign1 = [Error] || sign2 = [Error]) then [Error]
  else 
    match sign1 with
    | [Zero] -> if List.mem Error sign2 then [Zero;Error] else [Zero]
    | _ -> (match sign2 with
        | [Zero] -> if List.mem Error sign1 then [Zero;Error] else [Zero] 
        |_ -> sign_mudi sign1 sign2)


let sign_div sign1 sign2 =
  if (sign1 = [Error] || sign2 = [Zero] || sign2 = [Error]) then [Error]
  else 
    match sign1 with
    | [Zero] -> if List.mem Error sign2 then [Zero;Error] else [Zero]
    | _ -> sign_mudi sign1 sign2

let sign_mod sign1 sign2 =
  match sign2 with 
  | [Zero] -> [Error]
  | _ -> [Zero; Pos]

let rec sign_op env ex1 ex2 op = 
  let sign1 = sign_expr env ex1 in let sign2 = sign_expr env ex2 in
  match op with
  | Add -> sign_add sign1 sign2
  | Sub -> if ex1 = ex2 then
      if sign2 = [Error] then [Error] else Zero::(List.filter ((=) Error) sign2)
    else sign_sub sign1 sign2
  | Mul -> sign_mul sign1 sign2
  | Div -> sign_div sign1 sign2
  | Mod -> sign_mod sign1 sign2


and sign_expr env = function
  | Num n -> if n > 0 then [Pos] else if n < 0 then [Neg] else [Zero]
  | Var x -> Env.find x env
  | Op(op, ex1, ex2) -> sign_op env ex1 ex2 op

let greater_than equal sign_list =
  let all = [Neg;Zero;Pos;Error] in
  if List.mem Neg sign_list then 
    (if equal then all else reverse_sign [Neg])
  else  if List.mem Zero sign_list then 
    if equal || List.mem Neg sign_list then sign_list else
      [Pos]
  else sign_list


let inter_not_possible sign1 sign2 = 
  let inter = intersection sign1 sign2 in 
  inter = [Error] || inter = []

let is_cond_possible cond env =
  let ex1, comp, ex2 = cond in 
  let sign2 = sign_expr env ex2 in
  let sign1 = sign_expr env ex1 in
  match comp with
  | Lt -> not(inter_not_possible (greater_than false sign1) sign2)
  | Le -> not(inter_not_possible (greater_than true sign1) sign2)
  | Gt -> not(inter_not_possible (greater_than false sign2) sign1)
  | Ge -> not(inter_not_possible (greater_than true sign2) sign1)
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
         | [Neg;Zero] | [Zero] -> [Neg]
         | [Zero;Error] | [Neg;Zero;Error] -> [Neg;Error]
         | t -> t)
     | Gt -> (match sign2 with
         | [Zero;Pos] | [Zero] -> [Pos]
         | [Zero;Error] | [Zero;Pos;Error] -> [Pos;Error]
         | t -> t)
     | Le -> (match sign2 with
         | [Zero] -> [Zero;Neg]
         | [Zero;Error] -> [Neg;Zero;Error]
         | t -> t)
     | Ge -> (match sign2 with
         | [Zero] -> [Zero;Pos]
         | [Zero;Error] -> [Zero;Pos;Error]
         | t -> t)
     | _ -> sign2 )
  in match e1 with
  | Var name -> Env.add name (union_sign sign sign1) env
  | _ -> env

let rec sign_while env (c,b)= 
  if not(is_cond_possible c env) then env else
    let env1 = sign_cond c env in
    let env2 = sign_block b env1 in
    if env2 <> env then sign_while env2 (c,b)
    else let e1,comp,e2 = c in 
      let env_prop = sign_cond (e1,(reverse_comp comp),e2) env in
      let f = fun x val1 val2 -> Some (intersection val1 val2) in
      Env.union f env env_prop


and sign_instr env = function
  | Set (name, e) -> Env.add name (sign_expr env e)  env 
  | Read name ->  let sign = [Neg;Zero;Pos] in
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
  | While (c,b) -> sign_while env (c,b)

and sign_block b env = 
  match b with
  | [] -> env
  | (pos,instr)::xs -> let new_env = sign_instr env instr in sign_block xs new_env
