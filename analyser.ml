open Printf
open String
open Ast


let rec analyse_expr vars gvars = function
  | Var name -> (*if Names.mem name gvars
                  then vars,gvars
                  elsePrintf.printf "var: %s\n" name; *)
    Names.add name vars, gvars
  | Op (op,e1,e2) -> let vars1,gvars1 = analyse_expr vars gvars e1 in 
    let vars2,gvars2 = analyse_expr vars1 gvars e2 in vars2,gvars
  | _ -> vars,gvars

let analyse_cond cond vars gvars = 
  let ex1, comp, ex2 = cond in 
  let vars1,gvars1 = analyse_expr vars gvars ex1 in 
  let vars2,gvars2 = analyse_expr vars1 gvars ex2 in
  vars2,gvars


let rec analyse_instr vars gvars setvars = function
  | Set (name, e) -> let vars1,gvars1 = analyse_expr vars gvars e in
    Names.add name vars1,Names.add name gvars,Names.add name setvars
  | Read name -> Names.add name vars,Names.add name gvars,Names.add name setvars
  | Print e -> let vars1,gvars1 = analyse_expr vars gvars e
    in vars1,gvars1,setvars
  | If (c,b1,b2) -> let vars1,gvars1 = analyse_cond c vars gvars 
    in let vars2,gvars2,_ = analyse_block vars1 gvars setvars b1 
    in let vars3,gvars3,_ = analyse_block vars2 gvars setvars b2
    in vars3,Names.union gvars2 gvars3,setvars
  | While (c,b) as w ->  let vars1,gvars1 = analyse_cond c vars gvars 
    in let vars2,gvars2,_ = analyse_block vars1 gvars setvars b
    in vars2, gvars2,setvars
and analyse_block vars gvars setvars =  function
  | [] -> vars,gvars,setvars
  | (pos,instr)::xs -> 
    let new_vars,new_gvars,new_setvars = analyse_instr vars gvars setvars instr
    in analyse_block new_vars new_gvars new_setvars xs
