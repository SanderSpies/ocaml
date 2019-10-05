(* because I'm bad at mixing things *)
open Ast

let memory = (Hashtbl.create 100 : (string, int) Hashtbl.t)

let rec handle instr = 
  match instr with 
  Load (name, op) ->
    let location = ...
  Store (name, op) ->
    let location = ...
  _ -> 
let check (_m: Ast.module_) = 
  List.iter (fun (f:Ast.func) -> 
    print_endline f.name;
    print_endline "----";
    handle f.body
  ) _m.funcs
