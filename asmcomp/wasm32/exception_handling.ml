open Ast
open Values
open Wasm_types

let add_exception_handling w (fns: Typed_cmm.func_result list) = (
    let find_function name = 
      List.find_opt (fun (f_name, _, _) -> 
          f_name = name
        ) fns
    in
    let fix_raise_exception (f: Ast.func) = (
        {f with 
            body = [
                DataSymbol "_exception_thrown";
                GetLocal "value";
                Load {ty = I32Type; align = 0; offset = 0l; sz = None};                
                Store {ty = I32Type; align = 0; offset = 0l; sz = None};
            ]
        } 
    )
    in
    let func (f: Ast.func) = (
         let get_local_position name = (
            let rec find counter = function
            | (local_, _) :: _ when local_ = name -> counter
            | _ ::  rest -> find (counter + 1) rest
            | [] -> assert false
            in
            find 0 f.locals )
        in
        let handle_exception = ref false in         
        let rec fix_body type_ exception_depth result (il:Ast.instr list) = (
            match il with
            | Loop (t, e) :: remaining -> 
                fix_body type_ exception_depth (result @ [Loop (t, fix_body type_ (Int32.add exception_depth 1l) [] e)]) remaining                
            | Block (t, e) :: remaining -> 
                fix_body type_ exception_depth (result @ [Block (t, fix_body type_ (Int32.add exception_depth 1l) [] e)]) remaining
            | If (t, e1, e2) :: remaining -> 
                fix_body type_ exception_depth (result @ [If (t, fix_body type_ (Int32.add exception_depth 1l) [] e1, fix_body type_ (Int32.add exception_depth 1l) [] e2)]) remaining                
            | TryCatch (s, then_, exception_name, catch_) :: remaining ->
                let i = get_local_position exception_name in 
                let pointer_size = Shadow_stack.pointer_size in
                let stackframe_size = (List.length (List.filter (fun (name, _) -> (String.length name < 13 || String.sub name 0 13 <> "shadow_stack_" )) f.locals)) * pointer_size in
                let offset = I32.of_int_u (stackframe_size - ((i + 1) * pointer_size)) in
                let result = fix_body type_ exception_depth (result @ 
                    [Block (s, 
                        [Block (
                            [],
                            (
                                handle_exception := true;
                                let r = fix_body s 0l [] then_ in
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
                            Const (I32 1l);
                            Binary (I32 I32Op.ShrU);
                            Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                            Store {ty = I32Type; align = 0; offset; sz = None};
                            DataSymbol "_exception_thrown";
                            Const (I32 0l);
                            Store {ty = I32Type; align = 0; offset = 0l; sz = None}
                        ]
                        @
                        (fix_body type_ 0l [] catch_)
                    )]
                ) remaining in
                result
            | CallIndirect (t, args) :: remaining -> 
                let x = find_function f.name in                         
                let rt = match x with 
                | Some (_, rt, _) -> rt 
                | None -> [I32Type]
                in
                fix_body type_ exception_depth (result @ [
                    CallIndirect (t, List.map (fun f -> fix_body type_ exception_depth [] f) args);
                    DataSymbol "_exception_thrown";
                    Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                    If ([], 
                        (
                            if !handle_exception then (
                                [
                                    Const (I32 1l);
                                    Br (Int32.add exception_depth 1l)
                                ]
                            )
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
                fix_body type_ exception_depth (
                    result @ 
                    [
                        DataSymbol "_exception_thrown";                      
                    ]
                    @
                    e
                    @
                    [
                        Const (I32 1l);
                        Binary (I32 I32Op.Shl);
                        Const (I32 1l);
                        Binary (I32 I32Op.Add);
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
                fix_body type_ exception_depth (result @ [SetLocal (s, fix_body type_ exception_depth [] i)]) remaining
            | Call (function_name, args) :: remaining -> (
                let x = find_function f.name in                         
                let rt = match x with 
                | Some (_, rt, _) -> rt 
                | None -> [I32Type]
                in
                fix_body type_ exception_depth (result @ [
                    Call (function_name, List.map (fun f -> fix_body type_ exception_depth [] f) args);
                    DataSymbol "_exception_thrown";
                    Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                    If ([], 
                        (
                            if !handle_exception then
                                [
                                    Const (I32 1l);
                                    Br (I32.add exception_depth 1l)]
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
                fix_body type_ exception_depth (result @ [other]) remaining
            | [] -> result   
        )
        in
        {f with           
          body = fix_body [] 0l [] f.Ast.body
        }  
    )
    in
    (Ast.{w with 
        funcs = List.map (fun (f:Ast.func) -> 
            if f.name = "caml_raise_exception" then 
                fix_raise_exception f
            else if Shadow_stack.is_external_call f.name then         
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
