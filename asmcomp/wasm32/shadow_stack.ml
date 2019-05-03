open Values
open Ast
open Wasm_types

(**
 * In the WebAssembly MVP there is no access to the stack. This module adds a 
 * shadow stack to make it possible to implement GC and exception handling.
 *
 *                      ----------------------------------------------
 * Stack frame layout: | return address | local variables | arguments |
 *                      ----------------------------------------------
 *
 * This module will add most of the locals and arguments inside the shadow 
 * stack.
 *
 * This is mostly an experiment, and it's not guaranteed to be fast. Most 
 * likely WebAssembly GC will eventually be faster.
 *
 * There is also a trade off between file size and performance. In this case we
 * chose performance and inline all the stack operations.
 *
 * The shadow stack should ideally only be used when it's really needed. There
 * is most likely room for improvement here.
 *)

let is_external_call name =
  String.length name > 5 && (String.sub name 0 5) = "caml_"

let create_stack_frame stackframe_size = 
  let local_sp = 0l in
  let move_stack_pointer = [
    GetGlobal "__stack_pointer";
    TeeLocal local_sp;
    Const (I32 (I32.of_int_s stackframe_size));  
    Binary (I32 I32Op.Sub);  
    SetGlobal "__stack_pointer"]
  in
  let set_return_addr = [ 
    GetGlobal "__stack_pointer";
    GetLocal local_sp;
    Store {ty = I32Type; align = 0; offset = 0l; sz = None}]
  in
  move_stack_pointer @
  set_return_addr

let add_shadow_stack w fns = (
  let func (f:Ast.func) = 
    let arg_type name index = (
      let found_function = List.find_opt (fun (f_name, _, _) -> 
        f_name = name
      ) fns in
      match found_function with 
      | Some (_fname, _, args) -> (
        (match (List.nth_opt args index) with
        | Some [I64Type] -> I64Type
        | Some [I32Type] -> I32Type
        | Some [F32Type] -> F32Type
        | Some [F64Type] -> F64Type
        | Some _ -> assert false
        | None -> I32Type)
      )
      | None -> I32Type
    )
    in
    if f.name = "caml_program" then 
      f
    else (
      let stackframe_size = (List.length f.locals + 1) * 4 in
      let push_stackframe = create_stack_frame stackframe_size in

      let rec fix_body result (il:Ast.instr list) =
        match (il: Ast.instr list) with 
        | Loop (t, e) :: remaining -> 
          fix_body (result @ [Loop (t, fix_body [] e)]) remaining
        | Block (t, e) :: remaining -> 
          fix_body (result @ [Block (t, fix_body [] e)]) remaining
        | If (t, e1, e2) :: remaining -> 
          fix_body (result @ [If (t, fix_body [] e1, fix_body [] e2)]) remaining        
        | CallIndirect (t, args) :: remaining -> 
          let rev_args = List.rev args in
          let modified_args = List.mapi (fun i a -> 
            [GetGlobal "__stack_pointer"; 
             Const (I32 (I32.of_int_s ((i + 1) * 4))); 
             Binary (I32 I32Op.Sub)] 
             @ 
            (fix_body [] a) 
            @ 
            [Store {ty = arg_type t i ; align = 0; offset = 0l; sz = None}]
          ) (List.rev (List.tl rev_args))
          @ 
          [fix_body [] (List.hd rev_args)]
          in
          fix_body (result @ [CallIndirect (t, modified_args)]) remaining        
        | Call (function_name, args) :: remaining when function_name <> "caml_alloc" ->
            let modified_args = List.mapi (fun i a -> 
              [GetGlobal "__stack_pointer"; 
                Const (I32 (I32.of_int_s ((i + 1) * 4))); 
                Binary (I32 I32Op.Sub)] 
              @
              (fix_body [] a)
              @ 
              [Store {ty = arg_type function_name i ; align = 0; offset = 0l; sz = None}]
              @ 
              (
                if is_external_call function_name then  
                (* TODO: don't do this for DROP *)
                (
                  [GetGlobal "__stack_pointer"; 
                  Const (I32 (I32.of_int_s ((i + 1) * 4))); 
                  Binary (I32 I32Op.Sub)] 
                  @                   
                  (match List.hd (List.rev a) with 
                  | GetLocal x ->
                    let ty = ref I32Type in
                    List.iteri (fun i (_, ty_) ->
                      if Int32.of_int i = x then
                        ty := ty_
                    ) f.locals;
                    [Load {ty = !ty; align = 0; offset = 0l; sz = None}]
                  | Load {ty = F32Type; _} -> [Load {ty = F32Type; align = 0; offset = 0l; sz = None}]
                  | Const (I64 _) -> [Load {ty = I64Type; align = 0; offset = 0l; sz = None}]
                  | _ -> [Load {ty = I32Type; align = 0; offset = 0l; sz = None}])
                )
                else
                  []
              )
            ) args 
            in
            fix_body (result @ [Call (function_name, modified_args)]) remaining
        | GetLocal x :: remaining -> 
          let ty = ref I32Type in
          List.iteri (fun i (_, ty_) ->
            if Int32.of_int i = x then
              ty := ty_
          ) f.locals;
          let i = Int32.to_int x in
          fix_body
          (result 
          @
          (if i >= f.no_of_args then 
          [
            GetGlobal "__stack_pointer";
            Const (I32 (I32.of_int_s ((i - f.no_of_args + 1) * 4)));    
            Binary (I32 I32Op.Add);
            Load {ty = !ty; align = 0; offset = 0l; sz = None}]
          else 
            [
              GetGlobal "__stack_pointer";
              Const (I32 (I32.of_int_s ((stackframe_size - (i + 1) * 4) * 4)));    
              Binary (I32 I32Op.Add);
              Load {ty = !ty; align = 0; offset = 0l; sz = None}
            ]
          )
          )
          remaining
        | SetLocal (x, arg)  :: remaining -> 
          let pos = Int32.to_int x in
          
          fix_body 
          (result 
          @
          (if pos >= f.no_of_args then 
          [
            GetGlobal "__stack_pointer";
            Const (I32 (I32.of_int_s ((pos - f.no_of_args + 1) * 4)));    
            Binary (I32 I32Op.Add)]
          else 
            [
              GetGlobal "__stack_pointer";
              Const (I32 (I32.of_int_s ((stackframe_size - (pos + 1) * 4) * 4)));    
              Binary (I32 I32Op.Add);
            ])
          @
          (fix_body [] arg)
          @
          [Store {ty = I32Type; align = 0; offset = 0l; sz = None}])
          remaining  
        | Return :: remaining -> 
          fix_body (result @ [
            GetGlobal "__stack_pointer";
            Load {ty = I32Type; align = 0; offset = 0l; sz = None};
            SetGlobal "__stack_pointer";
            Return
          ]) remaining
        | other :: remaining -> 
          fix_body (result @ [other]) remaining
        | [] -> result
      in   
      let pop_stackframe = 
      [
        GetGlobal "__stack_pointer";
        Load {ty = I32Type; align = 0; offset = 0l; sz = None};
        SetGlobal "__stack_pointer";
        ]
      in
      {f with 
        locals = [("__local_sp", I32Type)];
        no_of_args = 0;
        body = push_stackframe @ (fix_body [] f.body) @ pop_stackframe
      }
    )
  in
  let type_ t = 
    if t.tname <> "caml_program" && (is_external_call t.tname) = false  then     
      {t with 
        tdetails = FuncType ([], [I32Type])
      }
    else
      t
  in
  let symbol s = 
    if (String.sub s.name 0 5) = "caml_" then 
      s
    else 
      match s.details with 
      | Function
      | Import _ -> {s with details = Import ([], [I32Type])}
      | _-> s 
  in
  (Ast.{w with 
    globals = [{
      name = "__stack_pointer";
      gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
      value = [Const (I32 4l)]
    }] @ w.globals;
    funcs = List.map func w.funcs;
    types = List.map type_ w.types;
    symbols = List.map symbol w.symbols;  
  }, List.map (fun (name, rt, args) -> 
    if (is_external_call name) = false then
      (name, [I32Type], []) 
    else
      (name, rt, args)
  ) fns)
)