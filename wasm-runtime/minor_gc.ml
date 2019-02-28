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
    {
        tname = "caml_gc_dispatch";
        tdetails = FuncType ([], [])
    };
    ];
    globals = [{
        name = "caml_young_base";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_young_start";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_young_end";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_young_alloc_start";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_young_alloc_mid";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_young_alloc_end";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_young_trigger";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_young_limit";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_young_ptr";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_minor_heap_wsz";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]        
    }
    ];
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
            SetGlobal "caml_young_base";

            DataSymbol "__heap_base";
            SetGlobal "caml_young_start";

            DataSymbol "__heap_base";
            GetLocal 0l;
            Binary (I32 I32Op.Add);
            SetGlobal "caml_young_end";

            GetGlobal "caml_young_start";
            SetGlobal "caml_young_alloc_start";

            GetGlobal "caml_young_alloc_start";
            GetLocal 0l;
            Const (I32 2l);
            Binary (I32 I32Op.DivU);
            Binary (I32 I32Op.Add);
            SetGlobal "caml_young_alloc_mid";

            GetGlobal "caml_young_end";
            SetGlobal "caml_young_alloc_end";

            GetGlobal "caml_young_alloc_start";
            SetGlobal "caml_young_trigger";

            GetGlobal "caml_young_trigger";
            SetGlobal "caml_young_limit";

            GetGlobal "caml_young_alloc_end";
            SetGlobal "caml_young_ptr";

            GetLocal 0l;
            SetGlobal "caml_minor_heap_wsz";

        ];
        no_of_args = 1;
    };
    {
        name = "caml_gc_dispatch";
        ftype = "caml_gc_dispatch";
        locals = [];
        body = [

            
        ];
        no_of_args = 0;
    }
    (*{
        name = "caml_gc_dispatch";
    }*)
    ];
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
            name = "caml_gc_dispatch";
            details = Function
        };
        {
            name = "__heap_base";
            details = Data ({
                index = (-1l);
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
        };
        {
            name = "caml_young_start";
            details = Global ({
                index = 1l
            })
        };
        {
            name = "caml_young_end";
            details = Global ({
                index = 2l
            })
        };
        {
            name = "caml_young_alloc_start";
            details = Global ({
                index = 3l
            })
        };
        {
            name = "caml_young_alloc_mid";
            details = Global ({
                index = 4l
            })
        };
         {
            name = "caml_young_alloc_end";
            details = Global ({
                index = 5l
            })
        };
         {
            name = "caml_young_trigger";
            details = Global ({
                index = 6l
            })
        };
         {
            name = "caml_young_limit";
            details = Global ({
                index = 7l
            })
        };
        {
            name = "caml_young_ptr";
            details = Global ({
                index = 8l
            })
        };
        {
            name = "caml_minor_heap_wsz";
            details = Global ({
                index = 9l
            })
        };
    ];
}

let () = (
    Encode.encode ast;
    Encode.write_file "wasm-runtime/minor_gc.wasm"
)