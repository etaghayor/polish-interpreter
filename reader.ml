open Ast


let read_op = function
  | _ -> "TODO"

let read_expr line = "TODO"

let read_instr line = 
  if (String.starts_with ~prefix:"Set" line) then Set ("TODO",Num 2) else Set ("TODO", Num 2)
;;

let read_block b = "TODO"


let read_polish (filename:string) : program = 
  let ic = open_in filename in
  let read() = try Some (input_line ic) with End_of_file -> None in
  let rec loop position res = match read() with
    | Some line -> (match String.get line 0 with
        | ' ' -> loop (position+1) ((1,Set ("x",Num 1))::res)
        | x -> loop (position+1) ((position, read_instr line)::res)
      )
    | None -> res
  in loop 1 []