open Values
open Ast
open Wasm_types

(**
 * In the WebAssembly MVP there is no access to the stack. This module adds a 
 * shadow stack to make it possible to implement GC and exception handling.
 *
 *                      -----------------------------
 * Stack frame layout: | local variables | arguments |
 *                      -----------------------------
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
  (String.length name > 5 && (String.sub name 0 5) = "caml_" && 
  ((String.length name < 12) || (String.sub name 0 12) <> "caml_tuplify") &&
  ((String.length name < 10) || (String.sub name 0 10) <> "caml_apply")) || 
  (String.length name > 4 && (String.sub name 0 4) <> "caml") 

let pointer_size = 8


let add_shadow_stack w fns = (
  let func (f:Ast.func) = (
    let counter = ref 0 in
    let unique () =
      counter := !counter + 1;
      string_of_int !counter
    in
    let find_function name = 
      List.find_opt (fun (f_name, _, _) -> 
          f_name = name
        ) fns
    in
    let get_local_position name = (
      let rec find counter = function
        | (local_, _) :: _ when local_ = name -> counter
        | _ ::  rest -> find (counter + 1) rest
        | [] -> assert false
      in
      find 0 f.locals )
    in
    if f.body = [] then 
      f
    else (
      let locals = ref ([("__local_sp", I32Type)] @ f.locals) in
      let add_local local = 
        locals := !locals @ [local] 
      in
      let arg_type name index = (
        let found_function = find_function name in
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
      let stackframe_size = (List.length f.locals) * pointer_size in

      let push_stackframe =
        (if stackframe_size > 0 then 
          [ GetGlobal "__stack_pointer";
            Const (I32 (I32.of_int_u stackframe_size)); 
            Binary (I32 I32Op.Sub);
            TeeLocal "__local_sp";
            SetGlobal "__stack_pointer"]  
        else 
          [ SetLocal ("__local_sp", [GetGlobal "__stack_pointer"]) ]
        )
      in      
      let rec fix_body result (il:Ast.instr list) =
        match (il: Ast.instr list) with 
        | Loop (t, e) :: remaining -> 
          fix_body  (result @ [Loop (t, fix_body  [] e)]) remaining
        | Block (t, e) :: remaining -> 
          fix_body   (result @ [Block (t, fix_body  [] e)]) remaining        
        | If (t, e1, e2) :: remaining -> 
          fix_body   (result @ [If (t, fix_body  [] e1, fix_body  [] e2)]) remaining              
        | CallIndirect (t, args) :: remaining -> 
          let rev_args = List.rev args in         
          let modified_args = List.mapi (fun i a -> 
              let arg_pos = (i + 1) * pointer_size in              
              let local_name = "shadow_stack_ft_" ^ t ^ "_" ^ (string_of_int i) ^ (unique()) in
              add_local (local_name, arg_type t i);
              ([SetLocal (local_name, (fix_body [] a))],
               [
                 GetLocal "__local_sp";
                  Const (I32 (I32.of_int_u arg_pos)); 
                  Binary (I32 I32Op.Sub);
                  GetLocal local_name;
                 Store {ty = arg_type t i ; align = 0; offset = 0l; sz = None}]              
              ) 
            ) (List.tl rev_args)
          in
          fix_body  (result @ [CallIndirect ("re_i32", (List.map fst modified_args) @ (List.map snd modified_args) @ ([fix_body  [] (List.hd rev_args)]))]) remaining
        | Call (function_name, args) :: remaining ->            
            let modified_args = List.mapi (fun i a -> (
                let arg_pos = (i + 1) * pointer_size in    
                let local_name = "shadow_stack_" ^ function_name ^ "_" ^ (string_of_int i ^ unique()) in
                add_local (local_name, arg_type function_name i);
                if is_external_call function_name then 
                  (fix_body [] a, [])
                else
                  (                              
                    ([SetLocal (local_name, (fix_body [] a))],
                     [
                      GetLocal "__local_sp";
                      Const (I32 (I32.of_int_u arg_pos)); 
                      Binary (I32 I32Op.Sub);
                      GetLocal local_name;
                      Store {ty = arg_type function_name i ; align = 0; offset = 0l; sz = None}
                     ]
                    )
                  )
              )              
            ) args 
            in
            fix_body  (result @ [Call (function_name, (List.map fst modified_args) @ (List.map snd modified_args))]) remaining        
        | TryCatch (s, then_, x, catch_) :: remaining ->
          fix_body (result @ [TryCatch (s, fix_body [] then_, x, fix_body [] catch_)]) remaining
        | GetLocal x :: remaining -> 
          let i = get_local_position x in
          let offset = I32.of_int_u (stackframe_size - ((i + 1) * pointer_size)) in
          let (_, ty) = (List.nth f.locals i) in
          fix_body          
          (result 
          @          
            [ 
              GetLocal "__local_sp";
              Load {ty; align = 0; offset; sz = None}
            ]   
          )
          remaining
        | SetLocal (x, arg)  :: remaining ->           
          let pos = get_local_position x in  
          let offset = I32.of_int_u (stackframe_size - ((pos + 1) * pointer_size)) in        
          fix_body           
          (result 
          @               
              [GetLocal "__local_sp"]
              @                 
              (fix_body [] arg)
              @
             (let (_, ty) = List.nth f.locals pos in
            [Store {ty; align = 0; offset; sz = None}])
          )          
          remaining  
        | Return :: remaining -> 
          fix_body  (result @ 
            if stackframe_size > 0 then
              [            
                GetLocal "__local_sp";
                Const (I32 (I32.of_int_u stackframe_size));  
                Binary (I32 I32Op.Add);  
                SetGlobal "__stack_pointer";
                Return
              ]
          else 
              [Return]
          ) remaining
        | other :: remaining -> 
          fix_body  (result @ [other]) remaining
        | [] -> result    
      in   
      let pop_stackframe = 
        if stackframe_size > 0 then
          [ 
            GetGlobal "__stack_pointer";
            Const (I32 (I32.of_int_u stackframe_size));  
            Binary (I32 I32Op.Add);
            SetGlobal "__stack_pointer"
          ]  
        else  
          []
      in
        {f with           
          locals = !locals;
          no_of_args = 0;
          body = push_stackframe @ (fix_body [] f.body) @ pop_stackframe
        }  
    )
  )  
  in
  let type_ t = 
    match t with 
    | {tdetails = FuncType([], []); _} -> t
    | _ -> 
      {t with 
          tdetails = FuncType ([], [I32Type])
      }
  in
  let symbol s = 
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
    funcs = List.map (fun (f:Ast.func) -> 
      if is_external_call f.name then         
        f 
      else 
        func f
    ) w.funcs;
    types = List.map (fun t -> 
      if is_external_call t.tname then 
        t 
      else (
        type_ t 
      )
    ) w.types;
    symbols = List.map (fun s -> 
      if is_external_call s.name then 
        s
      else 
        symbol s    
    ) w.symbols;  
  }, List.map (fun (name, rt, args) -> 
    if is_external_call name then
      (name, rt, args)
    else
      (name, [I32Type], [])
  ) fns)
)