(* open Cmm *)
(* open Typed_cmm *)
open Ast
(* open Values *)
(* open Wasm_types *)

let ast: Ast.module_ = {
    types = [];
    globals = [];
    tables = [];
    memories = [];
    funcs = [{
        name = "caml_alloc";
        ftype = "caml_alloc";
        locals = [
            ("wosize", I32Type);
            ("tag", I32Type)
        ];
        body = [
            GetLocal "";
        ];
        no_of_args = 2;
    }];
    start = None;
    elems = [];
    data = [];
    imports = [];
    exports = [];
    symbols = [];
}

let () = (
    Encode.encode ast;
    Encode.write_file "wasm-runtime/alloc.wasm"
)