open Ast
open Values
open Wasm_types

let name s =
  try Utf8.decode s with Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let ast: Ast.module_ = {
    types = [{
        tname = "caml_init_gc";
        tdetails = FuncType ([I32Type; I32Type; I32Type; I32Type; I32Type; I32Type], [])
    };
    {
        tname = "caml_main";
        tdetails = FuncType ([], [])
    };
    {
        tname = "_start";
        tdetails = FuncType ([], [])
    };
    {
        tname = "caml_program";
        tdetails = FuncType ([], [])
    };
    ];
    globals = [];
    tables = [];
    memories = [];
    funcs = [
    {
        name = "caml_main";
        ftype = "caml_main";
        locals = [];
        body = [
            Call ("caml_init_gc", [[
                Const (I32 262144l);
                Const (I32 126976l);
                Const (I32 15l);
                Const (I32 80l);
                Const (I32 500l);
                Const (I32 1l);
            ]]);
            Call ("caml_program", []);
        ];
        no_of_args = 0;
    };
    {
        name = "_start";
        ftype = "_start";
        locals = [];
        body = [
            Call ("caml_main", []);
        ];
        no_of_args = 0;
    }];
    start = None;
    elems = [];
    data = [];
    imports = [{        
        module_name = name "env";
        item_name = name "caml_program";
        idesc = FuncImport "caml_program"
    }; {
        module_name = name "env";
        item_name = name "caml_init_gc";
        idesc = FuncImport "caml_init_gc"
    }
    ];
    exports = [];
    symbols = [
    {
        name = "caml_program";
        details = Import ([], [])
    };    
    {
        name = "caml_init_gc";
        details = Import ([I32Type; I32Type; I32Type; I32Type; I32Type; I32Type], [])
    };
    {
        name = "_start";
        details = Function;
    };
    {
        name = "caml_main";
        details = Function;
    }
    ];
}

let () = (
    Encode.encode ast;
    Encode.write_file "wasm-runtime/startup.wasm"
)