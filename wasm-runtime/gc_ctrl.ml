(* open Cmm *)
(* open Typed_cmm *)
open Ast
(* open Values *)
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
        tname = "caml_set_minor_heap_size";
        tdetails = FuncType ([I32Type], [])
    }
    ];
    globals = [];
    tables = [];
    memories = [];
    funcs = [{
        name = "caml_init_gc";
        ftype = "caml_init_gc";
        locals = [
            ("caml_init_minor_heap_wsz", I32Type);
            ("caml_init_heap_wsz", I32Type);
            ("caml_init_heap_chunk_sz", I32Type);
            ("caml_init_percent_free", I32Type);
            ("caml_init_max_percent_free", I32Type);
            ("caml_init_major_window", I32Type);
        ];
        body = [
            Call ("caml_set_minor_heap_size", [[GetLocal 0l]])
        ];
        no_of_args = 6;
    };
    ];
    start = None;
    elems = [];
    data = [];
    imports = [
    {
        module_name = name "env";
        item_name = name "caml_set_minor_heap_size";
        idesc = FuncImport "caml_set_minor_heap_size"
    }
    ];
    exports = [];
    symbols = [
        {
            name = "caml_init_gc";
            details = Function
        };
        {
            name = "caml_set_minor_heap_size";
            details = Import ([I32Type], [])
        }
    ];
}

let () = (
    Encode.encode ast;
    Encode.write_file "wasm-runtime/gc_ctrl.wasm"
)