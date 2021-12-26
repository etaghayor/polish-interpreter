open Ast
open Printer
open Evaluator

let rec simpl_expr expr =
  match expr with 
  | Num n -> Some (n)
  | Var x -> None
  | Op(op, ex1, ex2) -> 
    let exp1_simp = simpl_expr ex1 in let exp2_simp = simpl_expr ex2 in 
    if (Option.is_some(exp1_simp) && Option.is_some (exp2_simp) ) then 
      Some  (Option.get exp1_simp + Option.get exp2_simp) else None

 let rec simpl_cond cond = 
  let ex1, comp, ex2 = cond in 
    (match simpl_expr ex1 with 
      | Some n -> 
        (match simpl_expr ex2 with
          | Some n2 -> Some ( eval_cond(Num n,comp,Num n2) )
          | None -> None
        )
      | None -> None
    ) 


let ex2 = Op(Add, Num 3, Num 6)
let ex1 = Op(Add, ex2, Num 5);;

let ex1Simpl = simpl_expr ex1;;
print_expr ex1;;
match ex1Simpl with 
  |Some ex -> Some (Printf.printf "%d" (ex))
  |None -> None
;;

let cond = Num 3, Lt, Num 2;;
let simpl_cond1 = simpl_cond cond;;
(* match simpl_cond1 with
|Some env -> let e, b = env in  Printf.printf "%d" b
|None -> None *)
