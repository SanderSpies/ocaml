(* open Cmm *)
(* open Typed_cmm *)
open Ast
open Values
open Wasm_types


let name s =
  try Utf8.decode s with Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let ast: Ast.module_ = {
    types = [];
    globals = [{
        name = "caml_globals_scanned";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 0l)]
    } ];
    tables = [];
    memories = [];
    funcs = [{
        name = "caml_oldify_minor_roots";
        ftype = "caml_oldify_minor_roots";
        locals = [
            ("i", I32Type);
            ("glob", I32Type);
            ("j", I32Type);
            ("glob_value", I32Type);
        ];
        body = [
            (* The global roots *)
            GetGlobal "caml_globals_scanned";
            SetLocal 0l;

            (** 
            for (i = caml_globals_scanned;
                i <= caml_globals_inited && caml_globals[i] != 0;
                i++) {
            *)
            Loop([], [
                GetLocal 0l;
                GetGlobal "caml_globals_inited";
                Compare (I32 I32Op.LeS);
                
                DataSymbol "caml_globals";
                GetLocal 0l;
                Const (I32 4l);
                Binary (I32 I32Op.Mul);    
                Binary (I32 I32Op.Add);            
                Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                TeeLocal 1l;
                
                Const (I32 0l);
                Compare (I32 I32Op.Ne);
                Binary (I32 I32Op.And);
                BrIf 0l;                

                (* for(glob = caml_globals[i]; *glob != 0; glob++) { *)
                Loop([], [
                    GetLocal 1l;
                    Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                    TeeLocal 3l;
                    Const (I32 0l);
                    Compare (I32 I32Op.Ne);
                    BrIf 0l;

                    (* for (j = 0; j < Wosize_val(*glob); j++){ *) *)
                    Const (I32 0l);
                    SetLocal 2l;
                    Loop([], 
                        [
                            GetLocal 2l;
                            GetLocal 1l;
                        ]
                        @
                        Mlvalues.wosize_val
                        @
                        [
                            Compare (I32 I32Op.LtS);
                            BrIf;



                            (* Oldify (&Field (*glob, j)); *)*)
                            
                            GetLocal 1l;
                            GetLocal 2l;
                            Const (I32 4l);
                            Binary (I32 I32Op.Mul);
                            Binary (I32 I32Op.Add);
                            Load {ty = I32Type; align = 0; offset = 0l; sz = None};
                        ]
                        @
                        Minor_gc.oldify 
                        @
                        [
                            GetLocal 2l;
                            Const (I32 1l);
                            Binary (I32 I32Op.Add);
                            SetLocal 2l;
                        ]
                    );
                    GetLocal 1l;
                    Const (I32 1l);
                    Binary (I32 I32Op.Add);
                    SetLocal 1l;
                ]);
                GetLocal 0l;
                Const (I32 1l);
                Binary (I32 I32Op.Add);
                SetLocal 0l;
            ]);
            GetGlobal "caml_globals_inited";
            SetGlobal "caml_globals_scanned";

            (* ignore dynamic roots for now *)

            (* The stack and local roots *)
            
            Loop([], [

            ]);

            (* caml_bottom_of_stack === __stack_pointer?! *))
        ];
        no_of_args = 0;
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
    Encode.write_file "wasm-runtime/roots.wasm"
)