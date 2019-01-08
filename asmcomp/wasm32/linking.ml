open Ast
open Values

let name s =
    try Utf8.decode s with Utf8.Utf8 ->
      failwith "invalid UTF-8 encoding"

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.sub s2 0 (String.length s1) = s1

let create_symbol_table m fti = (
  let get_func name = 
    List.find_opt (fun (n, _, _) -> n = name) fti
  in
  let global_symbols = (List.mapi (fun i (g:Ast.global) -> 
    {
      name = g.name;
      details = Global ({
        index = Int32.of_int i;      
      })
    }
  ) m.globals)
  in
  let check_duplicates symbols = Ast.(
    let used_names:string list ref = ref [] in
    List.iter (fun symbol -> 
      if (List.exists (fun f -> f = symbol.name) !used_names) then 
        failwith ("Duplicate name:" ^ symbol.name)
      else 
        used_names := !used_names @ [symbol.name]
    ) symbols
  )
  in 
  let m = {m with symbols = m.symbols @ global_symbols } in
  let m = {m with symbols = m.symbols @ (List.map (fun (f:Ast.func) ->(
    {
    name = f.name;
    details = Function
  })) m.funcs)} 
  in
  check_duplicates m.Ast.symbols;
  let m = {m with symbols = (List.fold_left (fun a {init = {detail; _}; _} -> (
    List.fold_left (fun a d -> 
      a @ (        
        match d with 
        | FunctionLoc symbol when not (List.exists (fun f -> f.name = symbol) a) -> 
            let s = get_func symbol in
            (match s with 
            | Some (_, rt, args) -> 
               if (isprefix "caml_curry" symbol) then
                Ast.Types.[{
                  name = symbol;
                  details = Import ([I32Type; I32Type], [I32Type])
                }]
              else 
                [{
                  name = symbol;
                  details = Import (List.fold_left (fun a i -> a @ i) [] args, rt)
                }]
            | None -> 
              if (isprefix "caml_curry" symbol) then
                Ast.Types.[{
                  name = symbol;
                  details = Import ([I32Type; I32Type], [I32Type])
                }]
              else 
                [{
                  name = symbol;
                  details = Import ([], [])
                }])
        | _ -> [])
    ) a detail     
  )) m.symbols m.data)} 
  in
  check_duplicates m.Ast.symbols;
  let add_missing_symbols () = Ast.(
    let code_symbols = ref [] in
    let rec handle_expr instr =
      (match instr with 
      | CallIndirect (_, args) :: _
      | Call (_, args) :: _ -> List.iter (fun i -> handle_expr i) args
      | SetLocal (_, arg) :: _ -> handle_expr arg
      | _ -> ()
      );

      (match instr with
      | Call (symbol, _) :: remaining
      | FuncSymbol symbol :: remaining ->  
        (if not (List.exists (fun s -> s.name = symbol) m.symbols) && 
            not (List.exists (fun s -> s.name = symbol) !code_symbols) then  (
            let s = get_func symbol in
            match s with 
            | Some (_, rt, args) -> 
              code_symbols := !code_symbols @
              (if (isprefix "caml_curry" symbol) then
                Ast.Types.[{
                  name = symbol;
                  details = Import ([I32Type; I32Type], [I32Type])
                }]
              else 
                 [{
                  name = symbol;
                  details = Import (List.fold_left (fun a i -> a @ i) [] args, Ast.Types.(match rt with [F32Type] -> [F32Type] | _ -> [I32Type]))
                }])
            | None -> 
              
              code_symbols := !code_symbols @ [{
                name = symbol;
                details = Import ([], [])
              }]
          )
        );
        handle_expr remaining              
      | DataSymbol symbol :: remaining ->                
        (if not (List.exists (fun s -> s.name = symbol) m.symbols) && 
            not (List.exists (fun s -> s.name = symbol) !code_symbols) then  ( 
          code_symbols := !code_symbols @ [{
            name = symbol;
            details = Data ({
                index = (-1l);
                relocation_offset = 0l;
                size = 0l;
                offset = 0l
            })
          }])
        );
        handle_expr remaining
      | Block (_, instr) :: remaining
      | Loop (_, instr) :: remaining ->
        handle_expr instr;
        handle_expr remaining;
      | If (_, e1 , e2) :: remaining ->
        handle_expr e1;
        handle_expr e2;
        handle_expr remaining;
      | _ :: remaining -> 
        handle_expr remaining;
      | [] -> ())
    in 
    List.iter (fun (f:Ast.func) -> handle_expr f.body) m.funcs;
    {m with symbols = m.symbols @ !code_symbols})
  in 
  let m = add_missing_symbols () in
  check_duplicates m.Ast.symbols;
  let turn_missing_functions_to_imports () = Ast.( 
    let missing_imports = List.filter (fun symbol ->
      match symbol.details with 
      | Function
      | Import _ -> (
        let key = symbol.name in        
        not (List.exists (fun (i:Ast.import) -> (Ast.string_of_name i.item_name) = key) m.imports)
        &&
        not (List.exists (fun (f:Ast.func) -> f.name = key) m.funcs)
      )
      | _ -> false    
    ) m.symbols in  
    let imports = ref [] in
    let types = ref [] in
    List.iter (fun symbol ->
      
      let key = symbol.name in
      let name_ = ("empty_type_" ^ key) in
      let (arg, result) = match symbol.details with 
      | Import i -> i      
      | _ -> assert false
      in 
      let empty_type = {tname = name_; tdetails = Types.FuncType (arg, result)} in
      types := !types @ [empty_type];
      imports := !imports @ [{
        module_name = name "env";
        item_name = name key;
        idesc = FuncImport name_
      }];
    ) missing_imports;
    {m with imports = m.imports @ !imports;
            types = m.types @ !types}
  )
  in
  let m = turn_missing_functions_to_imports () in
  check_duplicates m.Ast.symbols;
  let add_missing_memory_addresses () = Ast.(  
    let result = ref m in
    List.iter (fun (s:data_part segment) -> 
      List.iter (fun d -> 
        match d with 
        | Symbol symbol -> (
            let has_symbol = (List.exists (fun (x:data_part segment) ->
              x.init.name = symbol
            ) m.data) 
            in
            let has_symbol = has_symbol || List.exists (fun s -> s.name = symbol) !result.symbols in
            if not has_symbol then (
              let w = !result in       
              result := {w with symbols = w.symbols @ 
                [{
                  name = symbol;
                  details = Data ({
                      index = (-1l);
                      relocation_offset = 0l;
                      size = 0l;
                      offset = 0l
                  })
                }]
              };              
            )            
          )
        | _ -> ()
      ) s.init.detail    
    ) m.data;
    !result
  )
  in  
  let m = add_missing_memory_addresses () in
  check_duplicates m.Ast.symbols;
  let add_elems_for_funcs () = Ast.(
    let elems = ref [] in
    List.iteri (fun i _ -> 
      elems := !elems @ [{
        index = 0l;
        offset=[Const (I32 (I32.of_int_s i))];
        init=[I32.of_int_s i]
      }]
    ) m.imports;
    let no_of_imports = List.length m.imports in
    List.iteri (fun i _ -> 
      elems := !elems @ [{
        index = 0l;
        offset=[Const (I32 (I32.of_int_s (no_of_imports + i)))];
        init=[I32.of_int_s (no_of_imports + i)]
      }]
    ) m.funcs;
    let no_of_elems = Int32.of_int (List.length !elems) in
    let tables:Ast.table list = Types.[{ttype = TableType ({min = no_of_elems; max = Some no_of_elems}, AnyFuncType)}] in
    {
      m with elems = !elems;
             tables = tables;
    }
  )
  in
  let m = add_elems_for_funcs () in
  check_duplicates m.Ast.symbols;
  m
)