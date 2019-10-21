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
            Const (I32 (I32.of_int_s stackframe_size)); 
            Binary (I32 I32Op.Sub);
            TeeLocal "__local_sp";
            SetGlobal "__stack_pointer"]  
        else 
          [ SetLocal ("__local_sp", [GetGlobal "__stack_pointer"]) ]
        )
      in      
      let rec fix_body result (il:Ast.instr list) is_cextcall =
        match (il: Ast.instr list) with 
        | Loop (t, e) :: remaining -> 
          fix_body  (result @ [Loop (t, fix_body  [] e is_cextcall)]) remaining is_cextcall
        | Block (id, t, e) :: remaining -> 
          fix_body   (result @ [Block (id, t, fix_body  [] e is_cextcall)]) remaining is_cextcall     
        | If (t, e1, e2) :: remaining -> 
          fix_body   (result @ [If (t, fix_body [] e1 is_cextcall, fix_body  [] e2 is_cextcall)]) remaining is_cextcall
        | CallIndirect (t, args) :: remaining -> 
          let rev_args = List.rev args in         
          let modified_args = List.mapi (fun i a -> 
              let arg_pos = (i + 1) * pointer_size in              
              let local_name = "shadow_stack_ft_" ^ t ^ "_" ^ (string_of_int i) ^ (unique()) in
              add_local (local_name, arg_type t i);
              ([SetLocal (local_name, (fix_body [] a is_cextcall))],
               [
                 GetGlobal "__stack_pointer";
                  Const (I32 (I32.of_int_s arg_pos)); 
                  Binary (I32 I32Op.Sub);
                  GetLocal local_name;
                 Store ("local_var_" ^ (string_of_int arg_pos), {ty = arg_type t i ; align = 0; offset = 0l; sz = None})]              
              ) 
            ) (List.tl rev_args)
          in
          fix_body  (result @ [CallIndirect ("re_i32", (List.map fst modified_args) @ (List.map snd modified_args) @ ([fix_body  [] (List.hd rev_args) is_cextcall]))]) remaining is_cextcall
        | Call (function_name, args) :: remaining ->   
            let modified_args = List.mapi (fun i a -> (
                let arg_pos = (i + 1) * pointer_size in    
                let local_name = "shadow_stack_" ^ function_name ^ "_" ^ (string_of_int i ^ unique()) in
                add_local (local_name, arg_type function_name i);
                
                if is_external_call function_name then (                                    
                  let result = fix_body [] a true, [] in
                  result
                ) else
                  (                              
                    ([SetLocal (local_name, (fix_body [] a is_cextcall))],
                     [
                      GetGlobal "__stack_pointer";
                      Const (I32 (I32.of_int_s arg_pos)); 
                      Binary (I32 I32Op.Sub);
                      GetLocal local_name;
                      (
                        Store ("local_var_" ^ (string_of_int arg_pos), {ty = arg_type function_name i ; align = 0; offset = 0l; sz = None})
                      )
                     ]
                    )
                  )
              )              
            ) args 
            in
            fix_body  (result @ [Call (function_name, (List.map fst modified_args) @ (List.map snd modified_args))]) remaining is_cextcall     
        | TryCatch (s, then_, x, catch_) :: remaining ->
          fix_body (result @ [TryCatch (s, fix_body [] then_ is_cextcall, x, fix_body [] catch_ is_cextcall)]) remaining is_cextcall
        | GetLocal x :: remaining -> 
          let i = get_local_position x in
          let offset = I32.of_int_s (stackframe_size - ((i + 1) * pointer_size)) in
          let (_, ty) = (List.nth f.locals i) in
          fix_body          
          (result 
          @          
            [ 
              GetLocal "__local_sp";
              Load ("local_var_" ^ (Int32.to_string offset), {ty; align = 0; offset; sz = None});              
              ]
          @  
          (if is_cextcall then (
            [
              (* Load {ty; align = 0; offset = 0l; sz = None}; *)
            ]) else 
            []
          )
          )
          remaining
          is_cextcall
        | SetLocal (x, arg)  :: remaining ->           
          
          let pos = get_local_position x in  
          let offset = I32.of_int_s (stackframe_size - ((pos + 1) * pointer_size)) in        
          fix_body           
          (result 
          @               
              [GetLocal "__local_sp"]
              @                 
              (fix_body [] arg is_cextcall)
              @
             (let (_, ty) = List.nth f.locals pos in
            [Store ("local_var_" ^ (Int32.to_string offset), {ty; align = 0; offset; sz = None})])
          )          
          remaining 
          is_cextcall 
        | Return :: remaining -> 
          fix_body  (result @ 
            if stackframe_size > 0 then
              [            
                GetLocal "__local_sp";
                Const (I32 (I32.of_int_s stackframe_size));  
                Binary (I32 I32Op.Add);  
                SetGlobal "__stack_pointer";                
                Return
              ]
          else 
              [Return]
          ) remaining is_cextcall
        | other :: remaining -> 
          fix_body  (result @ [other]) remaining is_cextcall
        | [] -> result    
      in   
      let pop_stackframe = 
        if stackframe_size > 0 then
          [             
            Call ("caml_garbage_collection", []);
            GetGlobal "__stack_pointer";
            Const (I32 (I32.of_int_s stackframe_size));
            Binary (I32 I32Op.Add);
            SetGlobal "__stack_pointer";            
          ]  
        else  
          []
      in
      let add_pop_stack l =
        let reversed = List.rev l in
        let before = List.rev (List.tl reversed) in
        let last = List.hd reversed in
        match last with
        | Br (id, n) -> before @ pop_stackframe @ [Br (id, n)]
        | Return -> before @ pop_stackframe @ [Return]
        | Drop -> before @ pop_stackframe @ [Drop]
        | _ -> l @ pop_stackframe
      in
        {f with           
          locals = !locals;
          no_of_args = 0;
          body = push_stackframe @ (add_pop_stack (fix_body [] f.body false))
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
    symbols = (List.map (fun s -> 
      if is_external_call s.name then 
        s
      else 
        symbol s    
    ) w.symbols);  
  }, List.map (fun (name, rt, args) -> 
    if is_external_call name then
      (name, rt, args)
    else
      (name, [I32Type], [])
  ) fns)
)