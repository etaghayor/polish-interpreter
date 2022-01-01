open Printf
open String
open Ast



let rec analyse_expr all_vars bvars gvars = function
  | Var name -> if Names.mem name gvars
    then Names.add name all_vars, bvars, gvars
    else Names.add name all_vars, Names.add name bvars,gvars
  | Op (op,e1,e2) -> let all1,bvars1,gvars1 = analyse_expr all_vars bvars gvars e1 in 
    let all2,bvars2,gvars2 = analyse_expr all1 bvars1 gvars e2 in all2,bvars2,gvars
  | _ -> all_vars,bvars,gvars

let analyse_cond cond all_vars bvars gvars = 
  let ex1, comp, ex2 = cond in 
  let all1,bvars1,gvars1 = analyse_expr all_vars bvars gvars ex1 in 
  let all2,bvars2,gvars2 = analyse_expr all1 bvars1 gvars ex2 in
  all2,bvars2,gvars


let rec analyse_instr all_vars bvars gvars = function
  | Set (name, e) -> let all1,bvars1,gvars1 = analyse_expr all_vars bvars gvars e in
    Names.add name all1, bvars1, Names.add name gvars1
  | Read name -> Names.add name all_vars, bvars, Names.add name gvars
  | Print e ->  analyse_expr all_vars bvars gvars e
  | If (c,b1,b2) -> let all1,bvars1,_ = analyse_cond c all_vars bvars gvars 
    in let all2,bvars2,gvars2 = analyse_block all1 bvars1 gvars b1 
    in let all3,bvars3,gvars3 = analyse_block all2 bvars2 gvars b2
    in let gvars_inter = Names.inter gvars2 gvars3 
    in all3, bvars3, Names.union gvars_inter gvars
  | While (c,b) as w -> let all1,bvars1,gvars = analyse_cond c all_vars bvars gvars 
    in let all2,bvars2,_ = analyse_block all1 bvars1 gvars b
    in all2, bvars2,gvars
and analyse_block all_vars bvars gvars = function
  | [] -> all_vars, bvars,gvars 
  | (pos,instr)::xs -> 
    let all, new_vars,new_gvars  = analyse_instr all_vars bvars gvars instr
    in analyse_block all new_vars new_gvars xs
