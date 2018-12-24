open Values
open Ast
open Wasm_types

let func f = 
  let stackframe_size = (List.length f.locals + 1) * 4 in

  let prefix = [
    GetGlobal "__stack_pointer";
    Const (I32 (I32.of_int_s stackframe_size));    
    Binary (I32 I32Op.Sub)
  ]
  in
  let no_of_args = f.no_of_args in
  let push_stack = prefix
  @
  [
    Const (I32 (I32.of_int_s stackframe_size));
    Store {ty = I32Type; align = 0; offset = 0l; sz = None}
  ]
  @  
  List.concat (List.mapi (fun i l ->  
    (if i < no_of_args then 
      let (_, ty) = l in
      prefix 
      @
      [Const (I32 (I32.of_int_s ((i + 1) * 4)));    
       Binary (I32 I32Op.Add);
       GetLocal (Int32.of_int i);
       Store {ty; align = 0; offset = 0l; sz = None}]
    else 
      []
    )
  ) f.locals)
  @
  [
    Const (I32 (I32.of_int_s stackframe_size));    
    SetGlobal "__stack_pointer"
  ]
  in

  let rec fix_set_locals (il:Ast.instr list) = 
    match (il: Ast.instr list) with 
    | Loop (t, e) :: remaining -> [Loop (t, fix_set_locals e)] @ (fix_set_locals remaining)    
    | Block (t, e) :: remaining -> [Block (t, fix_set_locals e)] @ (fix_set_locals remaining)
    | If (t, e1, e2) :: remaining -> [If (t, fix_set_locals e1, fix_set_locals e2)] @ (fix_set_locals remaining)
    | SetLocal x  :: remaining -> 
      let ty = ref I32Type in
      List.iteri (fun i (_, ty_) ->
        if Int32.of_int i = x then
          ty := ty_
      ) f.locals;
      [SetLocal x;
       GetGlobal "__stack_pointer";
       Const (I32 (I32.mul (I32.add x 1l) 4l)); 
       Binary (I32 I32Op.Add);
       GetLocal x;
       Store {ty = !ty; align = 0; offset = 0l; sz = None}
      ] 
      @  
      (fix_set_locals remaining)
    | other :: remaining -> [other] @ (fix_set_locals remaining)
    | [] -> []
  in   
  let last_item = List.hd (List.rev f.body) in
  let pop_stack = 
  [GetGlobal "__stack_pointer";
   GetGlobal "__stack_pointer";
   Load {ty = I32Type; align = 0; offset = 0l; sz = None};
   Binary (I32 I32Op.Add);
   SetGlobal "__stack_pointer"]
  @ 
  [last_item]
  in
  {f with 
    body = push_stack @ List.rev (List.tl (List.rev (fix_set_locals f.body))) @ pop_stack
  }

let add_shadow_stack w = (
  Ast.{w with 
    globals = w.globals @ [{
      name = "__stack_pointer";
      gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
      value = [Const (I32 4l)]
    }];
    funcs = List.map func w.funcs
  }
)