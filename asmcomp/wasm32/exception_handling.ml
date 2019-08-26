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
    let calc_stackframe_size locals = 
        (List.length (List.filter (fun (name, _) -> (String.length name < 13 || String.sub name 0 13 <> "shadow_stack_" )) locals))
    in
    let func (f: Ast.func) = (
        print_endline f.name;
        print_endline "===";
        let get_local_position name = (
            let rec find counter = function
            | (local_, _) :: _ when local_ = name -> counter
            | _ ::  rest -> find (counter + 1) rest
            | [] -> assert false
            in
            find 0 f.locals )
        in
        let incr boundaries =
            List.map (fun i -> i + 1) boundaries
        in 
        let closest_exception boundaries = 
            if List.length boundaries = 0 then 
                0
            else 
                List.hd (List.rev boundaries)
        in
        let bb boundaries depth br =
            let absolute_depth = depth - (Int32.to_int br) + 1 in            
            let result = (Int32.of_int (List.fold_left (fun increase boundary -> 
                if boundary > absolute_depth && boundary - absolute_depth > increase then boundary - absolute_depth else increase) (Int32.to_int br) boundaries)) in
            result
        in 
        let rec fix_body depth boundaries result = function
            | Loop (t, e) :: remaining -> 
                fix_body depth boundaries (result @ [Loop (t, fix_body (depth + 1) (incr boundaries) [] e)]) remaining                
            | Block (t, e) :: remaining -> 
                fix_body depth boundaries (result @ [Block (t, fix_body (depth + 1) (incr boundaries) [] e)]) remaining                
            | If (t, e1, e2) :: remaining -> 
                fix_body depth boundaries (result @ [If (t, fix_body (depth + 1) (incr boundaries) [] e1, fix_body (depth + 1) (incr boundaries) [] e2)]) remaining                
            | Br x :: remaining ->
                (* do we escape the boundaries? add the boundary! *)
                fix_body depth boundaries (result @ [Br (bb boundaries depth x)]) remaining
            | TryCatch (s, then_, exception_name, catch_) :: remaining ->
                let i = get_local_position exception_name in 
                let stackframe_size = calc_stackframe_size f.locals in
                let offset = I32.of_int_u ((stackframe_size - (i + 1)) * Shadow_stack.pointer_size) in
                fix_body depth boundaries (result @ 
                    [Block (s, 
                        [Block (
                            [],
                            (
                                let body = fix_body (depth + 2) ((incr (incr boundaries)) @ [depth]) [] then_ in
                                let reversed = List.rev body in                        
                                match (List.hd reversed) with
                                | Br _ -> body
                                | _ -> body @ [Br 1l]

                                
                            ))
                        ]
                        @
                        [                            
                            GetLocal "__local_sp";
                            DataSymbol "_exception_thrown";
                            Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                            Const (I32 1l);
                            Binary (I32 I32Op.ShrU);
                            Store {ty = I32Type; align = 0; offset; sz = None};                        
                            DataSymbol "_exception_thrown";
                            Const (I32 0l);
                            Store {ty = I32Type; align = 0; offset = 0l; sz = None}
                        ]
                        @
                        (fix_body (depth + 1) ((incr boundaries) @ [depth]) [] catch_)
                    )]
                ) remaining
            | CallIndirect (t, args) :: remaining -> 
                let x = find_function f.name in                         
                let rt = match x with 
                | Some (_, rt, _) -> rt 
                | None -> [I32Type]
                in
                fix_body depth boundaries (result @ [
                    CallIndirect (t, List.map (fun f -> fix_body depth boundaries [] f) args);
                    DataSymbol "_exception_thrown";
                    Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                    If ([], 
                        (
                            if List.length boundaries > 0 then (
                                [
                                    Const (I32 1l);
                                    Br (Int32.of_int (depth - closest_exception boundaries - 1))
                                ]
                            )
                            else (
                                [ 
                                    GetGlobal "__stack_pointer";                                    
                                    Const (I32 (I32.of_int_u (((calc_stackframe_size f.locals) - 1) * Shadow_stack.pointer_size)));  
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
            | Throw e :: _ ->                
                fix_body depth boundaries (
                    result @ 
                    [
                        DataSymbol "_exception_thrown";                      
                    ]
                    @
                    e
                    @
                    (if List.length boundaries > 0 then
                        [Br (Int32.of_int (closest_exception boundaries))]
                    else 
                    [ 
                        Const (I32 1l);
                        Binary (I32 I32Op.Shl);
                        Const (I32 1l);
                        Binary (I32 I32Op.Add);
                        Store {ty = I32Type; align = 0; offset = 0l; sz = None};
                        GetGlobal "__stack_pointer";
                        Const (I32 (I32.of_int_u (((calc_stackframe_size f.locals) - 1) * Shadow_stack.pointer_size)));  
                        Binary (I32 I32Op.Add);
                        SetGlobal "__stack_pointer";
                        Const (I32 1l);
                        Return
                    ])
                ) []
            | SetLocal (s, i) :: remaining ->
                fix_body depth boundaries (result @ [SetLocal (s, fix_body depth boundaries [] i)]) remaining
            | Call (function_name, args) :: remaining -> (
                let x = find_function f.name in                         
                let rt = match x with 
                | Some (_, rt, _) -> rt 
                | None -> [I32Type]
                in
                fix_body depth boundaries (result @ [
                    Call (function_name, List.map (fun f -> fix_body depth boundaries [] f) args);
                    DataSymbol "_exception_thrown";
                    Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                    If ([], 
                        (
                            if List.length boundaries > 0 then
                                [
                                    Const (I32 1l);
                                    Br (Int32.of_int (depth - closest_exception boundaries - 1))
                                ]
                            else
                                ([                                     
                                    GetGlobal "__stack_pointer";
                                    Const (I32 (I32.of_int_u (((calc_stackframe_size f.locals) - 1) * Shadow_stack.pointer_size)));  
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
                                ]))
                            ), 
                        []);
                ]) remaining        
            )
            | other :: remaining -> 
                fix_body depth boundaries (result @ [other]) remaining
            | [] -> result           
        in
        {f with           
          body = fix_body 0 [] [] f.Ast.body
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
