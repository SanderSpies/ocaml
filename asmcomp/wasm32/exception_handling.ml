open Ast
open Values
open Wasm_types

let add_exception_handling w (fns: Typed_cmm.func_result list) = (
    let find_function name = 
      List.find_opt (fun (f_name, _, _) -> 
          f_name = name
        ) fns
    in
    let func (f:Ast.func) = (
         let get_local_position name = (
            let rec find counter = function
            | (local_, _) :: _ when local_ = name -> counter
            | _ ::  rest -> find (counter + 1) rest
            | [] -> assert false
            in
            find 0 f.locals )
        in
        let handle_exception = ref false in 
        let exception_depth = ref 0l in   
        let rec fix_body result (il:Ast.instr list) = (
            match il with
            | Loop (t, e) :: remaining -> 
                exception_depth := Int32.add !exception_depth 1l;
                let result = fix_body (result @ [Loop (t, fix_body  [] e)]) remaining in
                exception_depth := Int32.sub !exception_depth 1l;
                result
            | Block (t, e) :: remaining -> 
                exception_depth := Int32.add !exception_depth 1l;                
                let result = fix_body (result @ [Block (t, fix_body  [] e)]) remaining in 
                exception_depth := Int32.sub !exception_depth 1l;
                result
            | If (t, e1, e2) :: remaining -> 
                exception_depth := Int32.add !exception_depth 1l;
                let result = fix_body (result @ [If (t, fix_body  [] e1, fix_body  [] e2)]) remaining in
                exception_depth := Int32.sub !exception_depth 1l;
                result
            | TryCatch (s, then_, exception_name, catch_) :: remaining ->
                let i = get_local_position exception_name in 
                (* print_endline ("Not properly handled yet:" ^ exception_name ^ ":" ^ (string_of_int x)); *)
                let pointer_size = Shadow_stack.pointer_size in
                let stackframe_size = (List.length (List.filter (fun (name, _) -> (String.length name < 13 || String.sub name 0 13 <> "shadow_stack_" )) f.locals)) * pointer_size in
                let offset = I32.of_int_u (stackframe_size - ((i + 1) * pointer_size)) in
                exception_depth := 0l;           
                let result = fix_body (result @ 
                    [Block (s, 
                        [Block (
                            [],
                            (
                                handle_exception := true;
                                let r = fix_body [] then_ in
                                handle_exception := false;
                                r
                            )
                            @
                            [
                                Br 1l
                            ])
                        ]
                        @
                        [                            
                            GetLocal "__local_sp";
                            DataSymbol "_exception_thrown";
                            Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                            Store {ty = I32Type; align = 0; offset; sz = None};
                            DataSymbol "_exception_thrown";
                            Const (I32 0l);
                            Store {ty = I32Type; align = 0; offset = 0l; sz = None}
                        ]
                        @
                        (fix_body [] catch_)
                    )]
                ) remaining in
                result
            | CallIndirect (t, args) :: remaining -> 
                let x = find_function f.name in                         
                let rt = match x with 
                | Some (_, rt, _) -> rt 
                | None -> [I32Type]
                in
                fix_body  (result @ [
                    CallIndirect (t, List.map (fun f -> fix_body [] f) args);
                    DataSymbol "_exception_thrown";
                    Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                    If ([], 
                        (
                            if !handle_exception then 
                                [Br (Int32.add !exception_depth 1l)]
                            else (
                                [ 
                                    GetGlobal "__stack_pointer";
                                    Const (I32 (I32.of_int_u ((List.length f.locals - 1) * Shadow_stack.pointer_size)));  
                                    Binary (I32 I32Op.Add);
                                    SetGlobal "__stack_pointer"
                                ]  
                                @

                                match rt with 
                                | [I32Type] -> [
                                    Const (I32 (-1l)); (* properly set the stack here...  *)
                                    Return
                                    ]
                                | _ -> [
                                    Return
                                ])
                        ), 
                        []);
                ]) remaining   
            | Throw e :: remaining ->
                fix_body (
                    result @ 
                    [
                        DataSymbol "_exception_thrown";                      
                    ]
                    @
                    e
                    @
                    [                        
                        Store {ty = I32Type; align = 0; offset = 0l; sz = None};
                    ]
                    @
                    [ 
                        GetGlobal "__stack_pointer";
                        Const (I32 (I32.of_int_u ((List.length f.locals - 1) * Shadow_stack.pointer_size)));  
                        Binary (I32 I32Op.Add);
                        SetGlobal "__stack_pointer";
                        Const (I32 1l);
                        Return
                    ]  
                ) remaining
            | SetLocal (s, i) :: remaining ->
                fix_body (result @ [SetLocal (s, fix_body [] i)]) remaining
            | Call (function_name, args) :: remaining -> (
                let x = find_function f.name in                         
                let rt = match x with 
                | Some (_, rt, _) -> rt 
                | None -> [I32Type]
                in
                fix_body  (result @ [
                    Call (function_name, List.map (fun f -> fix_body [] f) args);
                    DataSymbol "_exception_thrown";
                    Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                    If ([], 
                        (
                            if !handle_exception then 
                                [Br (I32.add !exception_depth 1l)]
                            else
                                [                                     
                                    GetGlobal "__stack_pointer";
                                    Const (I32 (I32.of_int_u ((List.length f.locals - 1) * Shadow_stack.pointer_size)));  
                                    Binary (I32 I32Op.Add);
                                    SetGlobal "__stack_pointer"
                                ]  
                                @
                                (match rt with 
                                | [I32Type] -> [
                                    Const (I32 (-1l));
                                    Return
                                    ]
                                | _ -> [
                                    Return
                                ])
                            ), 
                        []);
                ]) remaining        
            )
            | other :: remaining -> 
                fix_body  (result @ [other]) remaining
            | [] -> result   
        )
        in
        {f with           
          body = fix_body [] f.Ast.body
        }  
    )
    in
    (Ast.{w with 
        funcs = List.map (fun (f:Ast.func) -> 
            if Shadow_stack.is_external_call f.name then         
                f 
            else 
                func f
            ) w.funcs;
        data = w.data @ [{
            index =  0l;
            offset = [Const (I32 (-1l))];
            init = {
                name = "_exception_thrown";
                detail = [Int32 0l];
            }      
        }]
    }, fns)
)
