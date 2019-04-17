(* open Cmm *)
(* open Typed_cmm *)
open Ast
open Values
open Wasm_types


let name s =
  try Utf8.decode s with Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let oldify local_pointer = 
  pointer
  @
  [
      Load {}
      SetLocal local;
  ]
  @ 
  Mlvalues.is_block local;
  @ 
  Mlvalues.is_young local;
  @ 
  [
      Binary (I32 I32Op.And);
      If ([], [
        Call ("caml_oldify_one", [oldify_v, p])
      ], [])
  ]

let ast: Ast.module_ = {
    types = [{
        tname = "caml_set_minor_heap_size";
        tdetails = FuncType ([I32Type], [])
    };
    {
        tname = "caml_gc_dispatch";
        tdetails = FuncType ([], [])
    };
     {
        tname = "caml_major_collection_slice";
        tdetails = FuncType ([I32Type], [])
    };
    {
        tname = "caml_empty_minor_heap";
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
    };
    {
        name = "caml_requested_minor_gc";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 0l)]        
    };
    {
        name = "caml_requested_major_slice";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 0l)]        
    };
    {
        name = "caml_gc_phase";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_allocated_words";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
    };
    {
        name = "caml_in_minor_collection";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 0l)]
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
        name = "caml_oldify_one";
        ftype = "caml_oldify_one";
        locals = [
            ("v", I32Type);
            ("p", I32Type);
        ];
        body = [];
        no_of_args = 2;
    };
    {
        name = "caml_oldify_mopup";
        ftype = "caml_oldify_mopup";
        locals = [];
        body = [

        ];
        no_of_args = 0;
    }
    {
        name = "caml_empty_minor_heap";
        ftype = "caml_empty_minor_heap";
        locals = [
            ("prev_alloc_words", I32Type)
        ];
        body = [
            GetGlobal "caml_young_ptr";
            GetGlobal "caml_young_alloc_end";
            Compare (I32 I32Op.Ne);
            If ([], [
                GetGlobal "caml_allocated_words";
                SetLocal 0l;
                Const (I32 1l);
                SetGlobal "caml_in_minor_collection";
                Call ("caml_oldify_minor_roots", []);
                (* Loop ... *)
                Call ("caml_oldify_mopup", []);
                (* Loop ephemerons *)
                Call ("caml_final_update_minor_roots", []);
                (* loop custom table *)
                
            ], [
            ]);
        ];
        no_of_args = 0;
    };
    {
        name = "caml_gc_dispatch";
        ftype = "caml_gc_dispatch";
        locals = [
            ("trigger", I32Type)
        ];
        body = [
            GetGlobal "caml_young_trigger";
            TeeLocal 0l;
            GetGlobal "caml_young_alloc_start";
            Compare (I32 I32Op.Eq);
            GetGlobal "caml_requested_minor_gc";
            Binary (I32 I32Op.Or);
            If ([], [
                Const (I32 0l);
                SetGlobal "caml_requested_minor_gc";
                GetGlobal "caml_young_alloc_mid";
                SetGlobal "caml_young_limit";
                GetGlobal "caml_young_trigger";
                SetGlobal "caml_young_limit";
                Call ("caml_empty_minor_heap", []);
                GetGlobal "caml_gc_phase";
                Const (I32 Major_gc.phase_idle);
                Compare (I32 I32Op.Eq);
                If ([], [
                    Call ("caml_major_collection_slice", [[Const (I32 (-1l))]])
                ], []);

                (* skipped the finalizers part here... *)
            ], []);

            GetLocal 0l;
            GetGlobal "caml_young_alloc_start";
            Compare (I32 I32Op.Ne);
            GetGlobal "caml_requested_major_slice";
            Binary (I32 I32Op.Or);
            If ([], [
                Const (I32 0l);
                SetGlobal "caml_requested_major_slice";
                GetGlobal "caml_young_alloc_start";
                SetGlobal "caml_young_trigger";
                GetGlobal "caml_young_trigger";
                SetGlobal "caml_young_limit";
                Call ("caml_major_collection_slice", [[
                    Const (I32 (-1l))
                ]])
            ], [])

        ];
        no_of_args = 0;
    };
    {

    }
    ];
    start = None;
    elems = [];
    data = [];
    imports = [{
        module_name = name "env";
        item_name = name "caml_major_collection_slice";
        idesc = FuncImport "caml_major_collection_slice"
    }];
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
         {
            name = "caml_requested_minor_gc";
            details = Global ({
                index = 10l
            })
        };
        {            
            name = "caml_requested_major_slice";
            details = Global ({
                index = 11l
            })
        };
        {
            name = "caml_major_collection_slice";
            details = Import ([I32Type], [])
        };
        {
            name = "caml_empty_minor_heap";
            details = Function
        };
        {
            name = "caml_gc_phase";
            details = Global ({
                index = 12l
            })
        };
        {
            name = "caml_allocated_words";
            details = Global ({
                index = 13l
            })
        };
        {
            name = "caml_in_minor_collection";
            details = Global ({
                index = 14l
            })
        };
        

    ];
}

let () = (
    Encode.encode ast;
    Encode.write_file "wasm-runtime/minor_gc.wasm"
)