(* because I'm bad at mixing things *)
open Ast

type block_stack = string Stack.t

let block_stack : block_stack = Stack.create()

let rec handle instr = 
  match instr with 
  | Block (name, _, block) :: remaining ->
    Stack.push name block_stack;
    handle block;
    ignore(Stack.pop block_stack);
    handle remaining;
  | Loop (_, i) :: remaining ->
    Stack.push "loop" block_stack;
    handle i;
    ignore(Stack.pop block_stack);
    handle remaining
  | If (_, then_, else_) :: remaining ->
    Stack.push "if" block_stack;
    handle then_;
    handle else_;
    ignore(Stack.pop block_stack);
    handle remaining
  | Br (name, pos) :: remaining  ->
    let i = ref 0 in    
    let found = ref false in
    Stack.iter (fun f ->     
      if (name = f && !i == (Int32.to_int pos)) then 
          found := true;
      i := !i + 1
    ) block_stack;
    if not(!found) then (
      print_endline ("Br to " ^ name ^ "(" ^ Int32.to_string pos ^ ") not found");
      print_endline "== block stack ==";
      let i = ref 0 in
      Stack.iter (fun f -> 
        print_endline (" - " ^ f ^ " (" ^ string_of_int !i ^ ")");
        i := !i + 1;
      ) block_stack
    ) else (
      print_endline ("Found: " ^ name ^ "(" ^ (Int32.to_string pos) ^ ")")
    ); 
    handle remaining
  | _ :: remaining ->
    handle remaining
  | _ -> ()
;;

let check (_m: Ast.module_) = 
  List.iter (fun (f:Ast.func) -> 
    print_endline f.name;
    print_endline "----";
    handle f.body
  ) _m.funcs
