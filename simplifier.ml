open Ast

let calc n1 op n2 = 
  match op with 
    |Add -> n1 + n2
    |Sub -> n1 - n2
    |Mul -> n1 * n2
    |Div -> n1 / n2
    |Mod -> n1 mod n2

let rec simpl_expr_Aux expr =
  match expr with 
  | Num n -> Some (n)
  | Var x -> None
  | Op(op, ex1, ex2) -> 
    let exp1_simp = simpl_expr_Aux ex1 in let exp2_simp = simpl_expr_Aux ex2 in 
    if (Option.is_some(exp1_simp) && Option.is_some (exp2_simp) ) then 
      Some  (calc (Option.get exp1_simp) op (Option.get exp2_simp)) else None

let rec simpl_expr expr = 
  match expr with 
    |Num n -> expr 
    |Var x -> expr 
    |Op(op, ex1, ex2) -> 
      if      op = Add && ex1 = Num 0 then simpl_expr ex2 
      else if op = Add && ex2 = Num 0 then simpl_expr ex1
      else if op = Mul && ex1 = Num 1 then simpl_expr ex2 
      else if op = Mul && ex2 = Num 1 then simpl_expr ex1
      else 
        match simpl_expr_Aux expr with 
          |Some n -> Num n
          |None -> Op(op, simpl_expr ex1, simpl_expr ex2)



let get_num expr = 
  match expr with 
    |Num n -> Some n
    |Var x -> None 
    |Op(op, e1, e2) -> None 

let compare e1 comp e2 = 
  match comp with
    | Eq -> e1 = e2
    | Ne -> e1 <> e2
    | Lt -> e1 < e2
    | Le -> e1 <= e2
    | Gt -> e1 > e2
    | Ge -> e1 >= e2

let simpl_cond cond = 
  let ex1, comp, ex2 = cond in 
  let ex1_simp = get_num ex1 in let ex2_simp = get_num ex2 in
  if Option.is_some(ex1_simp ) && Option.is_some (ex2_simp) then 
    Some (compare (Option.get ex1_simp) comp (Option.get ex2_simp))
  else None 



let rec simpl_instr instr = 
  match instr with 
  | Set (name, expr) -> Some([-10, Set (name , simpl_expr expr)])
  | Read name -> None
  | Print expr -> None
  | If (cond, block_1, block_2) -> 
    let cond_s = simpl_cond cond in
    (match cond_s with 
      |Some (bool) -> if bool = true then Some(block_1) else Some(block_2)
      |None -> Some([-10, If (cond, simpl_block block_1, simpl_block block_2)])
    )
  | While (cond, block) -> 
    let cond_s = simpl_cond cond in
    (match cond_s with 
      |Some (bool) -> 
        if bool = true then Some([-10, While (cond, simpl_block block)]) 
        else Some([-20, While ((Num 0, Eq, Num 0), block)])
      |None -> Some([-10, While (cond, simpl_block block)])
    )

and simpl_block = function
|[] -> []
|(pos, ins) :: tl -> 
  match simpl_instr ins with 
    |Some b -> let (p, i) = List.hd b in 
      if p = -10 then [(pos, i)] @ simpl_block tl 
      else if p = -20 then simpl_block tl
      else (b @ simpl_block tl)
    |None -> [(pos, ins)] @ simpl_block tl
  