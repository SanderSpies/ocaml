(* open Cmm *)
(* open Typed_cmm *)
open Ast
open Values
open Wasm_types

let atom_table = ref []

let () = (
  for i = 0 to 255 do 
    atom_table := !atom_table @ [
      Int32 (Int32.of_int i)
    ]
  done
)

let name s =
  try Utf8.decode s with Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let caml_white = Int32.of_int (0 lsl 8)
let caml_gray = Int32.of_int (1 lsl 8)
let caml_blue = Int32.of_int (2 lsl 8)
let caml_black = Int32.of_int (3 lsl 8)

let ast: Ast.module_ = {
    types = [{
        tname = "caml_alloc";
        tdetails = FuncType ([I32Type; I32Type], [I32Type])
    };
    {
        tname = "caml_gc_dispatch";
        tdetails = FuncType ([], [])
    }
    ];
    globals = [{
        name = "max_young_wosize";
        gtype = Types.GlobalType (Types.I32Type, Types.Immutable);
        value = [Const (I32 (-1l))]
      }; 
      {
        name = "caml_young_ptr";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
      };
      {
        name = "caml_young_trigger";
        gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
        value = [Const (I32 (-1l))]
      };
      {
        name = "no_scan_tag";
        gtype = Types.GlobalType (Types.I32Type, Types.Immutable);
        value = [Const (I32 (251l))]
      }];
    tables = [];
    memories = Types.[{
      (* TODO: this needs to be improved when doing GC *)
      mtype = MemoryType {min = 100l; max = Some 100l}
    }];
    funcs = [{
        name = "caml_alloc";
        ftype = "caml_alloc";
        locals = [
            ("wosize", I32Type);
            ("tag", I32Type);
            ("counter", I32Type)
        ];
        body = [
          GetLocal 0l;
          GetGlobal "max_young_wosize";
          Compare (I32 I32Op.GeS);
          If ([I32Type], [
            GetLocal 0l;
            Const (I32 0l);
            Compare (I32 I32Op.Eq);
            If ([I32Type], [
              DataSymbol "atom_table";
              GetLocal 0l;
              Const (I32 4l);
              Binary (I32 I32Op.Mul);
              Binary (I32 I32Op.Add);
              Load {ty = I32Type; align = 0; offset = 0l; sz = None}
            ], [
              GetGlobal "caml_young_ptr";
              GetLocal 0l;
              Binary (I32 I32Op.Sub);
              SetGlobal "caml_young_ptr";
              GetGlobal "caml_young_ptr";
              GetGlobal "caml_young_trigger";
              Compare (I32 I32Op.LtU);
              If ([], [
                GetGlobal "caml_young_ptr";                
                GetLocal 0l;
                Const (I32 1l);
                Binary (I32 I32Op.Add);
                Binary (I32 I32Op.Add);
                SetGlobal "caml_young_ptr";
                Call ("caml_gc_dispatch", []);
                GetGlobal "caml_young_ptr";
                GetLocal 0l;
                Const (I32 1l);
                Binary (I32 I32Op.Add);
                Binary (I32 I32Op.Sub);
                SetGlobal "caml_young_ptr";
              ], []);
              GetGlobal "caml_young_ptr";
              GetLocal 0l;
              Const (I32 10l);
              Binary (I32 I32Op.Shl);
              Const (I32 caml_black);
              Binary (I32 I32Op.Add);
              GetLocal 1l;
              Binary (I32 I32Op.Add);
              Store {ty = I32Type; align = 0; offset = 0l; sz = None};
              GetLocal 1l;
              GetGlobal "no_scan_tag";
              Compare (I32 I32Op.LtU);
              If ([], [
                Loop ([], [
                  GetLocal 2l;
                  GetLocal 0l;
                  Compare (I32 I32Op.LtU);
                  BrIf 0l;
                  GetGlobal "caml_young_ptr";
                  Const (I32 4l);
                  Binary (I32 I32Op.Add);
                  GetLocal 2l;
                  Const (I32 4l);
                  Binary (I32 I32Op.Mul);
                  Binary (I32 I32Op.Add);
                  Const (I32 1l);
                  Store {ty = I32Type; align = 0; offset = 0l; sz = None};                
                  
                  SetLocal (2l, [
                    GetLocal 2l;
                    Const (I32 1l);
                    Binary (I32 I32Op.Add);
                  ]);
                ]);  
              ], []);            
              GetGlobal "caml_young_ptr";
              Const (I32 4l);
              Binary (I32 I32Op.Add)
            ])
          ], [
            Const (I32 (-1l))
          ]);
        ];
            (* 

CAMLexport value caml_alloc (mlsize_t wosize, tag_t tag)
{
  value result;
  mlsize_t i;

  CAMLassert (tag < 256);
  CAMLassert (tag != Infix_tag);
  if (wosize <= Max_young_wosize){
    if (wosize == 0) {
      result = Atom (tag);
    }else{
      Alloc_small (result, wosize, tag);
      if (tag < No_scan_tag){
        for (i = 0; i < wosize; i++) Field (result, i) = Val_unit;
      }
    }
  }else{
    result = caml_alloc_shr (wosize, tag);
    if (tag < No_scan_tag){
      for (i = 0; i < wosize; i++) Field (result, i) = Val_unit;
    }
    result = caml_check_urgent_gc (result);
  }
  return result;
}
 *)

            
        no_of_args = 2;
    }];
    start = None;
    elems = [];
    data = 
    [ {index =  0l;
        offset = [Const (I32 0l)];
        init = {
          name = ("atom_table");
          detail = !atom_table;
        };
    }];
    exports = [];
    imports = [
    {
        module_name = name "env";
        item_name = name "caml_gc_dispatch";
        idesc = FuncImport "caml_gc_dispatch"
    };
    ];
    symbols = [{
        name = "caml_alloc";
        details = Function
    }; 
    {
        name = "caml_gc_dispatch";
        details =  Import ([], [])
    }; 
    {
      name = "max_young_wosize";      
      details = Global ({
          index = 0l
      })
    };
    {
      name = "caml_young_ptr";      
      details = Global ({
          index = 1l
      })
    };
    {
      name = "caml_young_trigger";      
      details = Global ({
          index = 2l
      })
    };
     {
      name = "no_scan_tag";      
      details = Global ({
          index = 3l
      })
    };
     {
      name = "atom_table";
      details = Data ({
          index = (-1l);
          relocation_offset = 0l;
          size =  255l;
          offset = 0l;
        })
    }
    ];
}

let () = (
    Encode.encode ast;
    Encode.write_file "wasm-runtime/alloc.wasm"
)