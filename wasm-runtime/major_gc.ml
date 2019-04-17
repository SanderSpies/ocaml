(* open Cmm *)
(* open Typed_cmm *)
open Ast
open Values
open Wasm_types

let name s =
  try Utf8.decode s with Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let phase_mark = 0l
let phase_clean = 1l
let phase_sweep = 2l
let phase_idle = 3l

let ast: Ast.module_ = {
    types = [
    {
        tname = "caml_major_collection_slice";
        tdetails = FuncType ([I32Type], [])
    }
    ];
    globals = [{
        name = "caml_gc_phase";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (3l))]
    };
    {
        name = "caml_allocated_words";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (0l))]
    }];
    tables = [];
    memories = [];
    funcs = [{
        name = "caml_major_collection_slice";
        ftype = "caml_major_collection_slice";
        locals = [
            ("howmuch", I32Type)
        ];
        body = [
        ];
        no_of_args = 1;
    };
    ];
    start = None;
    elems = [];
    data = [];
    imports = [];
    exports = [];
    symbols = [
        {
            name = "caml_major_collection_slice";
            details = Function
        };
        {
            name = "caml_gc_phase";
            details = Global ({
                index = 0l
            })
        };
        {
            name = "caml_allocated_words";
            details = Global ({
                index = 1l
            })
        };
    ];
}

let () = (
    Encode.encode ast;
    Encode.write_file "wasm-runtime/major_gc.wasm"
)