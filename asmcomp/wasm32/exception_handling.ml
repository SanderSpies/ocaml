open Ast
open Values
open Wasm_types

let recent_exception_name = ref "INVALID"

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
                Load ("value", {ty = I32Type; align = 0; offset = 0l; sz = None});
                Store ("exception", {ty = I32Type; align = 0; offset = 0l; sz = None});
            ]
        } 
    )
    in
    let calc_stackframe_size locals = 
        (List.length (List.filter (fun (name, _) -> (String.length name < 13 || String.sub name 0 13 <> "shadow_stack_" )) locals))
    in
    let func (f: Ast.func) = (
        print_endline ("eh:" ^ f.name);
        print_endline "===";
        recent_exception_name := "INVALID";
        let get_local_position name = (
            let rec find counter = function
            | (local_, _) :: _ when local_ = name -> counter
            | _ ::  rest -> find (counter + 1) rest
            | [] -> assert false
            in
            find 0 f.locals )
        in
        let incr boundaries =
            List.map (fun (i, s) -> (i + 1, s)) boundaries
        in 
        let closest_exception boundaries = 
            if List.length boundaries = 0 then 
                assert false
            else (
                let a = List.hd boundaries in
                fst a - 1
            )
        in
        let br_fix id (boundaries:(int * int) list) (br: int32) =
             let increase = ref 0 in
             let result = List.fold_left (
                 fun result (curr, incr) -> 
                    (
                        print_endline ("boundary at: " ^ (string_of_int curr));
                        print_endline ("br:" ^ (Int32.to_string br));
                    if (curr - (incr + !increase)) <= (Int32.to_int br) then 
                        (           
                            increase := !increase + incr;
                            Int32.of_int ((Int32.to_int result) + incr)
                        ) 
                    else 
                        result
                    )) 
                br boundaries            
            in 
            if !increase = 0 then
                print_endline ("Br " ^ id ^ " (" ^ (Int32.to_string br) ^ ") got no increase")
            else 
                print_endline ("Br " ^ id ^ " (" ^ (Int32.to_string br) ^ ") got an increase of " ^ (string_of_int !increase));
            result 
            (* br *)
            (* let absolute_depth = depth - (Int32.to_int br) + 1 in            
            let result = (Int32.of_int (List.fold_left (fun increase boundary -> 
                if boundary > absolute_depth && boundary - absolute_depth > increase then boundary - absolute_depth else increase) (Int32.to_int br) boundaries)) in
            result *)
        in 
        let rec fix_body depth boundaries result = function
            | Loop (t, e) :: remaining -> 
                fix_body depth boundaries (result @ [Loop (t, fix_body (depth + 1) (incr boundaries) [] e)]) remaining                
            | Block (id, t, e) :: remaining -> 
                fix_body depth boundaries (result @ [Block (id, t, fix_body (depth + 1) (incr boundaries) [] e)]) remaining                
            | If (t, e1, e2) :: remaining -> 
                fix_body depth boundaries (result @ [If (t, fix_body (depth + 1) (incr boundaries) [] e1, fix_body (depth + 1) (incr boundaries) [] e2)]) remaining                
            | Br (id, x) :: remaining -> (
                fix_body depth boundaries (result @ [Br (id, br_fix id boundaries x)]) remaining)
            | TryCatch (s, then_, exception_name, catch_) :: remaining ->
                let i = get_local_position exception_name in 
                let stackframe_size = calc_stackframe_size f.locals in
                let offset = I32.of_int_u ((stackframe_size - (i + 1)) * Shadow_stack.pointer_size) in
                print_endline ("eh here:" ^ exception_name);
                recent_exception_name := exception_name;
                print_endline "add try catch blocks here";
                let result = fix_body depth boundaries (result @ 
                    [Block (exception_name, s, 
                        [Block (
                            "try_body_" ^ exception_name,
                            [],
                            (
                                let body = fix_body (depth + 2) ([(2, 2)] @ (incr (incr boundaries))) [] then_ in
                                let reversed = List.rev body in                        
                                match (List.hd reversed) with
                                | Br _ -> body
                                | _ -> body @ (print_endline ("eh: br to" ^ exception_name); [Br (exception_name, 1l)])
                            ))
                        ]
                        @
                        [                            
                            GetLocal "__local_sp";
                            DataSymbol "_exception_thrown";
                            Load ("exception", {ty = I32Type; align = 0; offset = 0l; sz = None});
                            Const (I32 1l);
                            Binary (I32 I32Op.ShrU);
                            Store ("exception_value", {ty = I32Type; align = 0; offset; sz = None});
                            DataSymbol "_exception_thrown";
                            Const (I32 0l);
                            Store ("exception", {ty = I32Type; align = 0; offset = 0l; sz = None})
                        ]
                        @
                        (fix_body (depth + 1) ([(1,1)] @ (incr boundaries)) [] catch_)
                    )]                                        
                ) remaining
                in
                print_endline "end try catch blocks here";
                result
            | CallIndirect (t, args) :: remaining -> 
                let x = find_function f.name in                         
                let rt = match x with 
                | Some (_, rt, _) -> rt 
                | None -> [I32Type]
                in
                fix_body depth boundaries (result @ [
                    CallIndirect (t, List.map (fun f -> fix_body depth boundaries [] f) args);
                    DataSymbol "_exception_thrown";
                    Load ("exception", {ty = I32Type; align = 0; offset = 0l; sz = None});
                    If ([], 
                        (
                            if List.length boundaries > 0 then (
                                [
                                    Const (I32 1l);
                                    Br ("try_body_" ^ !recent_exception_name, Int32.of_int (closest_exception boundaries))
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
                    (if List.length boundaries > 0 then (
                        [Br ("try_body_" ^ !recent_exception_name, Int32.of_int (closest_exception boundaries - 1))]
                    )
                    else 
                    [ 
                        Const (I32 1l);
                        Binary (I32 I32Op.Shl);
                        Const (I32 1l);
                        Binary (I32 I32Op.Add);
                        Store ("exception", {ty = I32Type; align = 0; offset = 0l; sz = None});
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
            | Call (function_name, args) :: remaining when function_name <> "caml_alloc" -> (
                let x = find_function f.name in                         
                let rt = match x with 
                | Some (_, rt, _) -> rt 
                | None -> [I32Type]
                in
                fix_body depth boundaries (result @ [
                    Call (function_name, List.map (fun f -> fix_body depth boundaries [] f) args);
                    DataSymbol "_exception_thrown";
                    Load ("exception", {ty = I32Type; align = 0; offset = 0l; sz = None});
                    If ([], 
                        (
                            if List.length boundaries > 0 then (                                
                                [
                                    Const (I32 1l);                                    
                                    Br ("try_body_" ^ !recent_exception_name, Int32.of_int (closest_exception boundaries))
                                ])
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
