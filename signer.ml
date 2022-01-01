open Printf
open String
open Ast

let perror_and_exit msg pos =
  failwith (msg ^ ". Print, line: " ^ string_of_int pos)


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
      | [Pos] -> (match sign2 with 
          | [Zero] -> [Zero]
          | t -> t)
      | [Neg] -> (match sign2 with 
          | [Zero] -> [Zero]
          | t -> t)
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


let sign_cond cond env = 
  let ex1, comp, ex2 = cond in 
  ((sign_comp comp) (sign_expr env ex1) (sign_expr env ex2))



let rec sign_instr env = function
  | Set (name, e) -> let n = sign_expr env e in Env.add name n env 
  | Read name -> let n = read_int() in let sign = sign_expr env (Num n) in
    Env.add name sign env 
  | Print e -> env
  | If (c,b1,b2) -> if sign_cond c env then sign_block b1 env
    else sign_block b2 env
  | While (c,b) as w-> while_aux c b env
and while_aux cond block env =
  if sign_cond cond env then (let new_env = sign_block block env in while_aux cond block new_env)
  else env
and sign_block b env = 
  let rec aux env = function
    | [] -> env
    | (pos,instr)::xs -> let new_env = sign_instr env instr in aux new_env xs
  in aux env b
