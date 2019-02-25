(* open Cmm *)
(* open Typed_cmm *)
open Ast
open Values
open Wasm_types

let ast: Ast.module_ = {
    types = [{
        tname = "caml_set_minor_heap_size";
        tdetails = FuncType ([I32Type], [])
    };
    ];
    globals = [{
        name = "caml_young_base";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    }];
    tables = [];
    memories = [];
    funcs = [{
        name = "caml_set_minor_heap_size";
        ftype = "caml_set_minor_heap_size";
        locals = [
            ("bsz", I32Type);
        ];
        body = [
            DataSymbol "__heap_base";
            SetGlobal "caml_young_base"
        ];
        no_of_args = 1;
    }];
    start = None;
    elems = [];
    data = [];
    imports = [];
    exports = [];
    symbols = [
        {
            name = "caml_set_minor_heap_size";
            details = Function
        };
        {
            name = "__heap_base";
            details = Data ({
                index = -1l;
                relocation_offset = 0l;
                size =  4l;
                offset = 0l;
            });      
        };
        {
            name = "caml_young_base";
            details = Global ({
                index = 0l
            })
        }
    ];
}

let () = (
    Encode.encode ast;
    Encode.write_file "wasm-runtime/minor_gc.wasm"
)