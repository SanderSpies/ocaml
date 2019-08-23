(* Version *)

[@@@ocaml.warning "-27"]

let version = 1l

open Values

(* Encoding stream *)

type stream =
{
  buf : Buffer.t;
  patches : (int * char) list ref
}

let stream () = {buf = Buffer.create 8192; patches = ref []}
let pos s = Buffer.length s.buf
let put s b = Buffer.add_char s.buf b
let put_string s bs = Buffer.add_string s.buf bs
let patch s pos b = s.patches := (pos, b) :: !(s.patches)

let to_string s =
  let bs = Buffer.to_bytes s.buf in
  List.iter (fun (pos, b) -> Bytes.set bs pos b) !(s.patches);
  Bytes.to_string bs

(* Encoding *)

let name2 s =
      try Utf8.decode s with Utf8.Utf8 ->
        failwith "invalid UTF-8 encoding"


type code_relocation =
  | R_WASM_FUNCTION_INDEX_LEB of int32 * string
  | R_WASM_MEMORY_ADDR_LEB of int32 * Ast.var  
  | R_WASM_TYPE_INDEX_LEB of int32 * Ast.var
  | R_WASM_GLOBAL_INDEX_LEB of int32 * string
  | R_WASM_MEMORY_ADDR_SLEB of int32 * string
  | R_WASM_TABLE_INDEX_SLEB  of int32 * string

type data_relocation =
  | R_WASM_TABLE_INDEX_I32 of int32 * string
  | R_WASM_MEMORY_ADDR_I32 of int32 * string
  

let encode_result = ref ""

let encode m =
  let s = stream () in

  let code_relocations:code_relocation list ref = ref [] in

  let data_relocations:data_relocation list ref = ref [] in
  
  let module E = struct
    (* Generic values *)

    let u8 i = put s (Char.chr (i land 0xff))
    let u16 i = u8 (i land 0xff); u8 (i lsr 8)
    let u32 i =
      Int32.(u16 (to_int (logand i 0xffffl) );
             u16 (to_int (shift_right i 16)))
    let u64 i =
      Int64.(u32 (to_int32 (logand i 0xffffffffL));
             u32 (to_int32 (shift_right i 32)))

    let rec vu64 i =
      let b = Int64.(to_int (logand i 0x7fL)) in
      if 0L <= i && i < 128L then u8 b
      else (u8 (b lor 0x80); vu64 (Int64.shift_right_logical i 7))

    let rec vs64 i =
      let b = Int64.(to_int (logand i 0x7fL)) in
      if -64L <= i && i < 64L then u8 b
      else (u8 (b lor 0x80); vs64 (Int64.shift_right i 7))

    let vu1 i = vu64 Int64.(logand (of_int i) 1L)
    let vu32 i = vu64 Int64.(logand (of_int32 i) 0xffffffffL)
    let vu32_fixed i = (
      let i = Int64.of_int32 i in
      Int64.(
        u8 ((to_int (logand i 0x7fL)) lor 0x80);
        u8 ((to_int  (logand (shift_right_logical i 7) 0x7fL) lor 0x80));
        u8 ((to_int  (logand (shift_right_logical i 14) 0x7fL) lor 0x80)); 
        u8 ((to_int  (logand (shift_right_logical i 21) 0x7fL) lor 0x80));
        u8 (to_int (logand (shift_right_logical i 28) 0x0fL))
      )
    )

    let vs7 i = vs64 (Int64.of_int i)
    let vs32 i = vs64 (Int64.of_int32 i)
    let f32 x = u32 (F32.to_bits x)
    let f64 x = u64 (F64.to_bits x)


    let vs32_fixed i = (
      let i = Int64.of_int32 i in
      let rec foo i p = 
        let b = Int64.(to_int (logand i 0x7fL)) in
        if -64L <= i && i < 64L && p = 0 then u8 b
        else (u8 (b lor 0x80); foo (Int64.shift_right i 7) (p - 1))
      in foo i 4
    )

    let len i =
      if Int32.to_int (Int32.of_int i) <> i then
        failwith
          "cannot encode length with more than 32 bit";
      vu32 (Int32.of_int i)

    let bool b = vu1 (if b then 1 else 0)
    let string bs = (
      len (String.length bs); put_string s bs
    )
    let name n = string (Utf8.encode n)
    let list f xs = List.iter f xs
    let opt f xo = Lib.Option.app f xo
    let vec f xs = len (List.length xs); list f xs

    let gap32 () = let p = pos s in u32 0l; u8 0; p
    let patch_gap32 p n =
      assert (n <= 0x0fff_ffff); (* Strings cannot excess 2G anyway *)
      let lsb i = Char.chr (i land 0xff) in
      patch s p (lsb (n lor 0x80));
      patch s (p + 1) (lsb ((n lsr 7) lor 0x80));
      patch s (p + 2) (lsb ((n lsr 14) lor 0x80));
      patch s (p + 3) (lsb ((n lsr 21) lor 0x80));
      patch s (p + 4) (lsb (n lsr 28))

    (* Types *)

    open Types

    let value_type = function
      | I32Type -> vs7 (-0x01)
      | I64Type -> vs7 (-0x02)
      | F32Type -> vs7 (-0x03)
      | F64Type -> vs7 (-0x04)

    let elem_type = function
      | AnyFuncType -> vs7 (-0x10)

    let stack_type = function
      | [] -> vs7 (-0x40)
      | [t] -> value_type t
      | _ ->
        failwith
          "cannot encode stack type with arity > 1 (yet)"

    let func_type = function
      | FuncType (ins, out) -> vs7 (-0x20); vec value_type ins; vec value_type out

    let limits vu {min; max} =
      bool (max <> None); vu min; opt vu max

    let table_type = function
      | TableType (lim, t) -> elem_type t; limits vu32 lim

    let memory_type = function
      | MemoryType lim -> limits vu32 lim

    let mutability = function
      | Immutable -> u8 0
      | Mutable -> u8 1

    let global_type = function
      | GlobalType (t, mut) -> value_type t; mutability mut

    (* Expressions *)

    open Ast
    open Values

    let op n = u8 n
    let end_ () = op 0x0b

    let code_pos = ref (-1l)
    let data_pos = ref (-1l)

    let memop {align; offset; _} =             
      vu32 (Int32.of_int align); 
      vu32_fixed offset

    let var x = vu32 x
    
    let reloc_index x = 
      vu32_fixed x
    
    let find_global_index index_ =
      let result = ref (-1) in
      List.iteri (fun i s -> match s.details with
      | Global {index} when s.name = index_ -> (
          result := i
        )
      | _ -> ()) m.symbols;
      if !result = -1 then 
        assert false 
      else 
        !result

    let find_global_index2 index_ =
      let result = ref (-1l) in
      List.iteri (fun i s -> match s.details with
      | Global {index} when s.name = index_ -> (
          result := index
        )
      | _ -> ()) m.symbols;
      if !result = (-1l) then (        
        failwith ("Could not find global: " ^ index_)
      ) else 
        !result

    let func_symbol_index symbol = 
      let rec f symbol symbols result = 
        match symbols with 
        | {name; details = Function} :: remaining
        | {name; details = Import _} :: remaining when name = symbol -> result
        | _ :: remaining -> f symbol remaining (Int32.add result 1l)
        | [] -> print_endline ("could not find:" ^ symbol); assert false
      in
      f symbol m.symbols 0l

    (* let data_symbol_index symbol = 
      let rec f symbol symbols result = 
        match symbols with 
        | {name; details = Data _} :: remaining when name = symbol -> result
        | _ :: remaining -> f symbol remaining (Int32.add result 1l)
        | [] -> assert false
      in
      f symbol m.symbols 0l *)


    let func_index symbol = 
      let rec find_import imports count = 
        match imports with
        | {item_name} :: remaining when (Ast.string_of_name item_name) = symbol -> count
        | _ :: remaining -> find_import remaining (Int32.add count 1l) 
        | [] -> (-1l)
      in
      let result = find_import m.imports 0l in
      if result = (-1l) then 
        let rec find_func funcs count = 
          match funcs with
          | {name; ftype} :: remaining when name = symbol -> count
          | _ :: remaining -> find_func remaining (Int32.add count 1l)
          | [] -> failwith ("Could not find: " ^ symbol)
        in
        find_func m.funcs (Int32.of_int (List.length m.imports))
      else 
        result
  
    let data_index symbol = 
      let rec iter_data (data: data_part segment list) count = 
        match data with
        | {init = {name}} :: remaining when name = symbol -> count
        | _ :: remaining -> iter_data remaining (Int32.add count 1l) 
        | [] -> (-1l)
      in iter_data m.data 0l

    let find_type x = 
      let rec iter result = function
        | ({tname}:Ast.type_) :: remaining when tname = x -> result
        | ({tname}:Ast.type_) :: remaining -> iter (Int32.add result 1l) remaining
        | [] -> result
      in
      iter 0l m.types 


    let rec instr locals e =
      let get_local_position name = (
        let rec find counter = function
          | (local_, _) :: _ when local_ = name -> Int32.of_int counter
          | _ ::  rest -> find (counter + 1) rest
          | [] -> failwith ("did not find:" ^ name)
        in
        find 0 locals )
      in
      match e with
      | Unreachable -> op 0x00
      | Nop -> op 0x01
      | Block (ts, es) -> op 0x02; stack_type ts; list (instr locals) es; end_ ()
      | Loop (ts, es) -> op 0x03; stack_type ts; list (instr locals) es; end_ ()
      | If (ts, es1, es2) ->
        op 0x04; stack_type ts; list (instr locals) es1;
        if es2 <> [] then op 0x05;
        list (instr locals) es2; end_ ()
      | Br x -> op 0x0c; var x
      | BrIf x -> op 0x0d; var x
      | BrTable (xs, x) -> op 0x0e; vec var xs; var x
      | Return -> op 0x0f
      | Call (symbol, args) ->
        List.iter (fun i -> List.iter (instr locals) i) args;
        op 0x10;
        let p = pos s in
        let index = func_index symbol in
        code_relocations := !code_relocations @ [R_WASM_FUNCTION_INDEX_LEB (Int32.of_int p, symbol)];          
        reloc_index index
      | CallIndirect (x, args) ->
        List.iter (fun i -> List.iter (instr locals) i) args;
        op 0x11;        
        let p2 = pos s in
        let x = find_type x in
        code_relocations := !code_relocations @ [R_WASM_TYPE_INDEX_LEB (Int32.of_int p2, x)];
        reloc_index x;
        u8 0x00
      | Drop -> op 0x1a
      | Select -> op 0x1b
      | GetLocal x -> op 0x20; var (get_local_position x)
      | SetLocal (x, args) -> 
        List.iter (instr locals) args;
        op 0x21;
        var (get_local_position x)
      | TeeLocal x -> op 0x22; var (get_local_position x)
      | GetGlobal x ->
        op 0x23;
        let p = pos s in
        code_relocations := !code_relocations @ [R_WASM_GLOBAL_INDEX_LEB (Int32.of_int p, x)];
        reloc_index (find_global_index2 x)
      | SetGlobal x ->
        op 0x24;
        let p = pos s in
        code_relocations := !code_relocations @ [R_WASM_GLOBAL_INDEX_LEB (Int32.of_int p, x)];
        reloc_index (find_global_index2 x)
      | Load ({ty = I32Type; sz = None; _} as mo) -> op 0x28; memop mo
      | Load ({ty = I64Type; sz = None; _} as mo) -> op 0x29; memop mo
      | Load ({ty = F32Type; sz = None; _} as mo) -> op 0x2a; memop mo
      | Load ({ty = F64Type; sz = None; _} as mo) -> op 0x2b; memop mo
      | Load ({ty = I32Type; sz = Some (Mem8, SX); _} as mo) ->
        op 0x2c; memop mo
      | Load ({ty = I32Type; sz = Some (Mem8, ZX); _} as mo) ->
        op 0x2d; memop mo
      | Load ({ty = I32Type; sz = Some (Mem16, SX); _} as mo) ->
        op 0x2e; memop mo
      | Load ({ty = I32Type; sz = Some (Mem16, ZX); _} as mo) ->
        op 0x2f; memop mo
      | Load {ty = I32Type; sz = Some (Mem32, _); _} ->
        assert false
      | Load ({ty = I64Type; sz = Some (Mem8, SX); _} as mo) ->
        op 0x30; memop mo
      | Load ({ty = I64Type; sz = Some (Mem8, ZX); _} as mo) ->
        op 0x31; memop mo
      | Load ({ty = I64Type; sz = Some (Mem16, SX); _} as mo) ->
        op 0x32; memop mo
      | Load ({ty = I64Type; sz = Some (Mem16, ZX); _} as mo) ->
        op 0x33; memop mo
      | Load ({ty = I64Type; sz = Some (Mem32, SX); _} as mo) ->
        op 0x34; memop mo
      | Load ({ty = I64Type; sz = Some (Mem32, ZX); _} as mo) ->
        op 0x35; memop mo
      | Load {ty = F32Type | F64Type; sz = Some _; _} ->
        assert false
      | Store ({ty = I32Type; sz = None; _} as mo) -> op 0x36; memop mo
      | Store ({ty = I64Type; sz = None; _} as mo) -> op 0x37; memop mo
      | Store ({ty = F32Type; sz = None; _} as mo) -> op 0x38; memop mo
      | Store ({ty = F64Type; sz = None; _} as mo) -> op 0x39; memop mo
      | Store ({ty = I32Type; sz = Some Mem8; _} as mo) -> op 0x3a; memop mo
      | Store ({ty = I32Type; sz = Some Mem16; _} as mo) -> op 0x3b; memop mo
      | Store {ty = I32Type; sz = Some Mem32; _} -> assert false
      | Store ({ty = I64Type; sz = Some Mem8; _} as mo) -> op 0x3c; memop mo
      | Store ({ty = I64Type; sz = Some Mem16; _} as mo) -> op 0x3d; memop mo
      | Store ({ty = I64Type; sz = Some Mem32; _} as mo) -> op 0x3e; memop mo
      | Store {ty = F32Type | F64Type; sz = Some _; _} -> assert false
      | CurrentMemory -> op 0x3f; u8 0x00
      | GrowMemory -> op 0x40; u8 0x00
      | FuncSymbol symbol ->
        op 0x41;
        let p = pos s in
        let found = ref false in
        List.iteri (fun symbol_index s -> match s.details with        
        | Import _
        | Function when s.name = symbol && not !found ->
          found := true;
          code_relocations := !code_relocations @ [R_WASM_TABLE_INDEX_SLEB (Int32.of_int p, symbol)];
          
          vs32_fixed (func_index symbol)
        | _ -> ()
        ) m.symbols;
        if not !found then
          failwith ("Symbol was not resolved:" ^ symbol)
      | DataSymbol symbol ->
        op 0x41;
        let p = pos s in
        let found = ref false in
        List.iteri (fun symbol_index s -> match s.details with
        | Data { index; offset } when s.name = symbol  && not !found ->
            found := true;
            code_relocations := !code_relocations @ [R_WASM_MEMORY_ADDR_SLEB (Int32.of_int p, symbol)];
            if symbol = "caml_globals_inited" || symbol = "caml_backtrace_pos" then
              vs32_fixed offset
            else 
              vs32_fixed (Int32.add offset 4l)
        | Import _
        | Function when s.name = symbol && not !found ->
          found := true;
          code_relocations := !code_relocations @ [R_WASM_TABLE_INDEX_SLEB (Int32.of_int p, symbol)];
          vs32_fixed (func_index symbol)
        | _ -> ()
        ) m.symbols;
        if not !found then
          failwith ("Symbol was not resolved:" ^ symbol)
      | Const (I32 c) -> op 0x41; vs32 c
      | Const (I64 c) -> op 0x42; vs64 c
      | Const (F32 c) -> op 0x43; f32 c
      | Const (F64 c) -> op 0x44; f64 c

      | Test (I32 I32Op.Eqz) -> op 0x45
      | Test (I64 I64Op.Eqz) -> op 0x50
      | Test (F32 _) -> assert false
      | Test (F64 _) -> assert false

      | Compare (I32 I32Op.Eq) -> op 0x46
      | Compare (I32 I32Op.Ne) -> op 0x47
      | Compare (I32 I32Op.LtS) -> op 0x48
      | Compare (I32 I32Op.LtU) -> op 0x49
      | Compare (I32 I32Op.GtS) -> op 0x4a
      | Compare (I32 I32Op.GtU) -> op 0x4b
      | Compare (I32 I32Op.LeS) -> op 0x4c
      | Compare (I32 I32Op.LeU) -> op 0x4d
      | Compare (I32 I32Op.GeS) -> op 0x4e
      | Compare (I32 I32Op.GeU) -> op 0x4f

      | Compare (I64 I64Op.Eq) -> op 0x51
      | Compare (I64 I64Op.Ne) -> op 0x52
      | Compare (I64 I64Op.LtS) -> op 0x53
      | Compare (I64 I64Op.LtU) -> op 0x54
      | Compare (I64 I64Op.GtS) -> op 0x55
      | Compare (I64 I64Op.GtU) -> op 0x56
      | Compare (I64 I64Op.LeS) -> op 0x57
      | Compare (I64 I64Op.LeU) -> op 0x58
      | Compare (I64 I64Op.GeS) -> op 0x59
      | Compare (I64 I64Op.GeU) -> op 0x5a

      | Compare (F32 F32Op.Eq) -> op 0x5b
      | Compare (F32 F32Op.Ne) -> op 0x5c
      | Compare (F32 F32Op.Lt) -> op 0x5d
      | Compare (F32 F32Op.Gt) -> op 0x5e
      | Compare (F32 F32Op.Le) -> op 0x5f
      | Compare (F32 F32Op.Ge) -> op 0x60

      | Compare (F64 F64Op.Eq) -> op 0x61
      | Compare (F64 F64Op.Ne) -> op 0x62
      | Compare (F64 F64Op.Lt) -> op 0x63
      | Compare (F64 F64Op.Gt) -> op 0x64
      | Compare (F64 F64Op.Le) -> op 0x65
      | Compare (F64 F64Op.Ge) -> op 0x66

      | Unary (I32 I32Op.Clz) -> op 0x67
      | Unary (I32 I32Op.Ctz) -> op 0x68
      | Unary (I32 I32Op.Popcnt) -> op 0x69

      | Unary (I64 I64Op.Clz) -> op 0x79
      | Unary (I64 I64Op.Ctz) -> op 0x7a
      | Unary (I64 I64Op.Popcnt) -> op 0x7b

      | Unary (F32 F32Op.Abs) -> op 0x8b
      | Unary (F32 F32Op.Neg) -> op 0x8c
      | Unary (F32 F32Op.Ceil) -> op 0x8d
      | Unary (F32 F32Op.Floor) -> op 0x8e
      | Unary (F32 F32Op.Trunc) -> op 0x8f
      | Unary (F32 F32Op.Nearest) -> op 0x90
      | Unary (F32 F32Op.Sqrt) -> op 0x91

      | Unary (F64 F64Op.Abs) -> op 0x99
      | Unary (F64 F64Op.Neg) -> op 0x9a
      | Unary (F64 F64Op.Ceil) -> op 0x9b
      | Unary (F64 F64Op.Floor) -> op 0x9c
      | Unary (F64 F64Op.Trunc) -> op 0x9d
      | Unary (F64 F64Op.Nearest) -> op 0x9e
      | Unary (F64 F64Op.Sqrt) -> op 0x9f

      | Binary (I32 I32Op.Add) -> op 0x6a
      | Binary (I32 I32Op.Sub) -> op 0x6b
      | Binary (I32 I32Op.Mul) -> op 0x6c
      | Binary (I32 I32Op.DivS) -> op 0x6d
      | Binary (I32 I32Op.DivU) -> op 0x6e
      | Binary (I32 I32Op.RemS) -> op 0x6f
      | Binary (I32 I32Op.RemU) -> op 0x70
      | Binary (I32 I32Op.And) -> op 0x71
      | Binary (I32 I32Op.Or) -> op 0x72
      | Binary (I32 I32Op.Xor) -> op 0x73
      | Binary (I32 I32Op.Shl) -> op 0x74
      | Binary (I32 I32Op.ShrS) -> op 0x75
      | Binary (I32 I32Op.ShrU) -> op 0x76
      | Binary (I32 I32Op.Rotl) -> op 0x77
      | Binary (I32 I32Op.Rotr) -> op 0x78

      | Binary (I64 I64Op.Add) -> op 0x7c
      | Binary (I64 I64Op.Sub) -> op 0x7d
      | Binary (I64 I64Op.Mul) -> op 0x7e
      | Binary (I64 I64Op.DivS) -> op 0x7f
      | Binary (I64 I64Op.DivU) -> op 0x80
      | Binary (I64 I64Op.RemS) -> op 0x81
      | Binary (I64 I64Op.RemU) -> op 0x82
      | Binary (I64 I64Op.And) -> op 0x83
      | Binary (I64 I64Op.Or) -> op 0x84
      | Binary (I64 I64Op.Xor) -> op 0x85
      | Binary (I64 I64Op.Shl) -> op 0x86
      | Binary (I64 I64Op.ShrS) -> op 0x87
      | Binary (I64 I64Op.ShrU) -> op 0x88
      | Binary (I64 I64Op.Rotl) -> op 0x89
      | Binary (I64 I64Op.Rotr) -> op 0x8a

      | Binary (F32 F32Op.Add) -> op 0x92
      | Binary (F32 F32Op.Sub) -> op 0x93
      | Binary (F32 F32Op.Mul) -> op 0x94
      | Binary (F32 F32Op.Div) -> op 0x95
      | Binary (F32 F32Op.Min) -> op 0x96
      | Binary (F32 F32Op.Max) -> op 0x97
      | Binary (F32 F32Op.CopySign) -> op 0x98

      | Binary (F64 F64Op.Add) -> op 0xa0
      | Binary (F64 F64Op.Sub) -> op 0xa1
      | Binary (F64 F64Op.Mul) -> op 0xa2
      | Binary (F64 F64Op.Div) -> op 0xa3
      | Binary (F64 F64Op.Min) -> op 0xa4
      | Binary (F64 F64Op.Max) -> op 0xa5
      | Binary (F64 F64Op.CopySign) -> op 0xa6

      | Convert (I32 I32Op.ExtendSI32) -> assert false
      | Convert (I32 I32Op.ExtendUI32) -> assert false
      | Convert (I32 I32Op.WrapI64) -> op 0xa7
      | Convert (I32 I32Op.TruncSF32) -> op 0xa8
      | Convert (I32 I32Op.TruncUF32) -> op 0xa9
      | Convert (I32 I32Op.TruncSF64) -> op 0xaa
      | Convert (I32 I32Op.TruncUF64) -> op 0xab
      | Convert (I32 I32Op.ReinterpretFloat) -> op 0xbc

      | Convert (I64 I64Op.ExtendSI32) -> op 0xac
      | Convert (I64 I64Op.ExtendUI32) -> op 0xad
      | Convert (I64 I64Op.WrapI64) -> assert false
      | Convert (I64 I64Op.TruncSF32) -> op 0xae
      | Convert (I64 I64Op.TruncUF32) -> op 0xaf
      | Convert (I64 I64Op.TruncSF64) -> op 0xb0
      | Convert (I64 I64Op.TruncUF64) -> op 0xb1
      | Convert (I64 I64Op.ReinterpretFloat) -> op 0xbd

      | Convert (F32 F32Op.ConvertSI32) -> op 0xb2
      | Convert (F32 F32Op.ConvertUI32) -> op 0xb3
      | Convert (F32 F32Op.ConvertSI64) -> op 0xb4
      | Convert (F32 F32Op.ConvertUI64) -> op 0xb5
      | Convert (F32 F32Op.PromoteF32) -> assert false
      | Convert (F32 F32Op.DemoteF64) -> op 0xb6
      | Convert (F32 F32Op.ReinterpretInt) -> op 0xbe

      | Convert (F64 F64Op.ConvertSI32) -> op 0xb7
      | Convert (F64 F64Op.ConvertUI32) -> op 0xb8
      | Convert (F64 F64Op.ConvertSI64) -> op 0xb9
      | Convert (F64 F64Op.ConvertUI64) -> op 0xba
      | Convert (F64 F64Op.PromoteF32) -> op 0xbb
      | Convert (F64 F64Op.DemoteF64) -> assert false
      | Convert (F64 F64Op.ReinterpretInt) -> op 0xbf

      | Throw t -> failwith "Exception handling spec not implemented yet"
      | TryCatch _ -> failwith "Exception handling spec not implemented yet"

    let const c =
      list (instr []) c; end_ ()

    (* Sections *)
    let code_section_index = ref 0
    let data_section_index = ref 0
    let section_counter = ref 0
    let section id f x needed =
      if needed then begin
        section_counter := !section_counter + 1;
        u8 id;
        let g = gap32 () in
        let p = pos s in
        f x;
        patch_gap32 g (pos s - p)
      end

    let custom_section id f x needed =
      if needed then begin
        u8 0;
        let g = gap32 () in        
        let p = pos s in
        string id;
        f x;
        patch_gap32 g (pos s - p);        
      end

    (* Type section *)
    let type_ (t:Ast.type_) = func_type t.tdetails

    let type_section ts =
      section 1 (vec type_) ts (ts <> [])

    
    (* Import section *)
    let import_desc d =
      match d with
      | FuncImport x -> u8 0x00; var (find_type x)
      | TableImport t -> u8 0x01; table_type t
      | MemoryImport t -> u8 0x02; memory_type t
      | GlobalImport t -> u8 0x03; global_type t

    let import im =
      let {module_name; item_name; idesc} = im in
      name module_name; name (name2 (Ast.string_of_name item_name)); import_desc idesc

    let import_section ims =
      section 2 (vec import) ims (ims <> [])

    (* Function section *)
    let func f = var (find_type f.ftype)

    let func_section fs =
      section 3 (vec func) fs (fs <> [])

    (* Table section *)
    let table tab =
      let {ttype} = tab in
      table_type ttype

    let table_section tabs =
      section 4 (vec table) tabs (tabs <> [])

    (* Memory section *)
    let memory mem =
      let {mtype} = mem in
      memory_type mtype

    let memory_section mems =
      section 5 (vec memory) mems (mems <> [])

    (* Global section *)
    let global g =
      let {gtype; value} = g in
      global_type gtype; const value

    let global_section gs =
      section 6 (vec global) gs (gs <> [])

    (* Export section *)
    let export_desc d =
      match d with
      | FuncExport symbol -> u8 0; var (func_index symbol)
      | TableExport x -> u8 1; var x
      | MemoryExport x -> u8 2; var x
      | GlobalExport x -> u8 3; var x

    


    let export ex =
      let {name = n; edesc} = ex in
      (match edesc with 
      | FuncExport _ -> name (name2 (Ast.string_of_name n))
      | _ -> name n);
      (* name n; *)
      export_desc edesc

    let export_section exs =
      section 7 (vec export) exs (exs <> [])

    (* Start section *)
    let start_section xo =
      section 8 (opt var) xo (xo <> None)

    (* Code section *)
    let compress ts =
      let combine t = function
        | (t', n) :: ts when t = t' -> (t, n + 1) :: ts
        | ts -> (t, 1) :: ts
      in List.fold_right combine ts []

    let local ((_, t), n) = len n; value_type t

    let code f =
      let {locals; body; no_of_args; _} = f in
      let g = gap32 () in
      let p = pos s in
      let remove_first locals no_of_removals = 
        let rec rem counter locals =
          if counter < no_of_removals then
            rem (counter + 1) (List.tl locals)
          else 
            locals
        in 
        rem 0 locals
      in
      vec local (compress (remove_first locals no_of_args));
      list (instr f.locals) body;
      end_ ();
      patch_gap32 g (pos s - p)

    let code_section fs =
      code_section_index := !section_counter;
      code_pos := Int32.of_int (pos s + 6);
      section 10 (vec code) fs (fs <> [])

    (* Element section *)
    let segment dat seg =
      let {index; offset; init} = seg in
      var index; const offset; dat init

    let table_segment seg =
      segment (vec reloc_index) seg

    let elem_section elems =
      section 9 (vec table_segment) elems (elems <> [])

    let count_it = ref 0
    let data_part_list (data_part_list: data_part) =      
      count_it := !count_it + 1;
      let g = gap32 () in      
      let start = pos s in
      List.iter (fun f ->
        match f with
        | String bs -> 
          put_string s bs
        | Float32 f -> 
          f32 f
        | Float64 f -> 
          f64 f
        | Symbol symbol ->
          let p = pos s in          
          let found = ref false in
          List.iteri (fun symbol_index s -> 
          match s.details with
           | Data { index; offset } when s.name = symbol -> 
            found := true;
            data_relocations := !data_relocations @ [R_WASM_MEMORY_ADDR_I32 (Int32.of_int p, symbol)];
            if offset = (-1l) then
              u32 0l
            else
              u32 offset
           | Function
           | Import _ when s.name = symbol ->
            found := true;
            let symbol_index = func_index symbol in
            data_relocations := !data_relocations @ [R_WASM_TABLE_INDEX_I32 (Int32.of_int p, symbol)]; 
            u32 symbol_index
           | Global _ when s.name = symbol ->
            failwith "Not handling a global here..."
           | _ -> ()
          ) m.symbols;
          if not !found then (
            failwith ("Not found symbol: " ^ symbol)
          )
        | FunctionLoc symbol -> 
          let p = pos s in
          let symbol_index = func_index symbol in
          data_relocations := !data_relocations @ [R_WASM_TABLE_INDEX_I32 (Int32.of_int p, symbol)];
          u32 symbol_index        
        | Int32 i32 -> 
          u32 i32
        | Nativeint ni -> 
          u32 (Nativeint.to_int32 ni)
        | Int16 i -> 
          u16 i
        | Int8 i -> 
          u8 i
      ) data_part_list.detail;
      let l = pos s - start in
      patch_gap32 g l

    let data_segment seg = (* https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md#data-section *)
      segment data_part_list seg

    let data_section data =
      data_section_index := !section_counter;      
      data_pos := Int32.of_int (pos s + 6);
      section 11 (vec data_segment) data (data <> [])

    let reloc_code data =
      vu32 (Int32.of_int !code_section_index);
      vu32 (Int32.of_int (List.length !code_relocations));      
      List.iteri (fun i r -> 
      (        
        match r with        
        | R_WASM_TABLE_INDEX_SLEB (offset, symbol_) 
        | R_WASM_MEMORY_ADDR_SLEB (offset, symbol_) -> ( 
          let exists = ref false in
          List.iteri (fun symbol_index s -> match s.details with          
          | Data {index}  when s.name = symbol_ ->
            exists := true;
            u8 4;
            vu32 (Int32.sub offset !code_pos);
            vs32_fixed (Int32.of_int symbol_index); 
            if symbol_ = "caml_globals_inited"  (* || symbol_ = "caml_backtrace_pos" || index = (-1l) *) then
              vs32 0l
            else
              vs32 4l            
          | Import _
          | Function when s.name = symbol_ -> 
            exists := true;            
            u8 1;            
            vu32 (Int32.sub offset !code_pos);
            vs32_fixed (Int32.of_int symbol_index); 
           | _ -> ()  
          ) m.symbols;
          if not !exists then (
            failwith ("Could not write relocation for:" ^ symbol_)
          )
        )
        | R_WASM_FUNCTION_INDEX_LEB (offset, symbol) ->
          let symbol_index = func_symbol_index symbol in          
          u8 0;
          vu32 (Int32.sub offset !code_pos);
          vu32_fixed symbol_index;
        | R_WASM_MEMORY_ADDR_LEB (offset, index_) -> 
          (
            let symbol_index = ref (-1) in
            List.iteri (fun i s -> match s.details with
            | Data { index; _ } when index = index_ -> (
                symbol_index := i
              )
            | _ -> ()) m.symbols;
            u8 3;
            vu32 (Int32.sub offset !code_pos);
            vs32_fixed (Int32.of_int !symbol_index);
            vs32 4l
          )
        | R_WASM_TYPE_INDEX_LEB (offset, index) ->
          u8 6;
          vu32 (Int32.sub offset !code_pos); 
          vu32_fixed index
        | R_WASM_GLOBAL_INDEX_LEB (offset, index_) ->
          u8 7;
          vu32 (Int32.sub offset !code_pos);
          let symbol_index = find_global_index index_ in 
          vu32_fixed (Int32.of_int symbol_index)
      )
      ) !code_relocations
      

    let relocate_code_section data =
      custom_section "reloc.CODE" reloc_code data (data <> [])

    let reloc_data data =
      vu32 (Int32.of_int !data_section_index);
      vu32 (Int32.of_int (List.length !data_relocations));    
      List.iter (fun r ->
        match r with      
        | R_WASM_TABLE_INDEX_I32 (offset, symbol) -> (            
          let symbol_index = func_symbol_index symbol in
          u8 2;
          vu32 (Int32.sub offset !data_pos);          
          vu32 symbol_index;
        )
        | R_WASM_MEMORY_ADDR_I32 (offset, symbol_) -> (
            let symbol_index = ref (-1) in
            List.iteri (fun i s -> match s.details with
            | Data _ when s.name = symbol_ -> (
                symbol_index := i;
              )
            | _ -> ()) m.symbols;
            (if !symbol_index >= List.length m.symbols then 
              failwith "Symbol index out of bounds"
            );
            u8 5;
            vu32 (Int32.sub offset !data_pos);
            vu32 (Int32.of_int !symbol_index);
            (* TODO: make more elegant *)
            let len = String.length symbol_ in
            let gc_roots_length = String.length "__gc_roots" in            
            let frametable_length = String.length "__frametable" in
            (* print_endline ("A1:" ^ symbol_); *)
            if symbol_ = "caml_frametable" || ( len > gc_roots_length && String.sub symbol_ (len - gc_roots_length) gc_roots_length = "__gc_roots") || ( len > frametable_length && String.sub symbol_ (len - frametable_length) frametable_length = "__frametable") then
              vs32 0l
            else 
              vs32 4l
        )
      ) !data_relocations

    let relocate_data_section data =
      custom_section "reloc.DATA" reloc_data data (data <> [])

    
    let counter = ref 0
    let symbol sym =   
      counter := !counter + 1;
      (match sym.details with
      | Import _
      | Function -> u8 0
      | Data _ ->  u8 1
      | Global _ -> u8 2
      );

      let flags = ref (
        match sym.details with 
        | Import _ -> 0l
        | _ -> 1l
      )
      in
      let exists = (match sym.details with      
      | Function -> 
        (if not (List.exists (fun (f:Ast.func) -> f.name = sym.name) m.funcs) then ( 
          failwith ("BUG: symbol " ^ sym.name ^ " appears to refer to a nonexisting function, perhaps it should refer to an import instead?"));            
        );
        (List.exists (fun (f:Ast.func) -> f.name = sym.name) m.funcs)
      | Import _ -> false
      | Data _ -> (data_index sym.name) <> -1l
      | Global _ -> true
      )
      in
      (if not exists then 
      (      
        flags := Int32.logor !flags 16l
      ));
      (match sym.details with 
      | Global _ -> flags := Int32.logor !flags 4l 
      | _ -> ());
      vu32 !flags;  
      (match sym.details with
      | Global f ->         
        vu32 f.index;
        if exists then (
          string sym.name
        )
      | Function ->
        vu32 (func_index sym.name);
        if exists then (
          string sym.name
          (* string sym.name *)
        ) 
      | Import _ ->
        vu32 (func_index sym.name);
      | Data d -> (       
        (if sym.name <> "" then        
        string sym.name
        else         
        string "_"); (* probably an initial block ?!malformed uleb128*)
        if exists then (
          vu32 (data_index sym.name);
          vu32 d.relocation_offset;
          vu32 d.size
        )
        )
      )        
    


    let symbol_table (data2:data_part segment list) =
        (* let size = ref 0l in
      List.iter (fun f -> 
        match f.details with
        | Data d -> size := Int32.add d.offset d.size
        | _ -> ()
      ) data;
      u8 2; (* WASM_DATA_SIZE *)
      let g = gap32 () in
      let p = pos s in
      vu32 !size;
      patch_gap32 g (pos s - p); *)
      (* u8 3; (* WASM_DATA_ALIGNMENT *)
      let g = gap32 () in
      let p = pos s in
      vu32 0l;
      patch_gap32 g (pos s - p); *)
      vu32 2l;
      u8 5;
      let g = gap32 () in
      let p = pos s in
      let no_of_data = List.length data2 in
      vu32 (Int32.of_int no_of_data);
      List.iter (fun (d: Ast.data_part Ast.segment) ->
        string d.init.name;
        vu32 4l;
        vu32 0l;      
      ) data2;
      patch_gap32 g (pos s - p);
      
      u8 8; (* WASM_SYMBOL_TABLE *)
      let g = gap32 () in
      let p = pos s in
      
      (* let written_symbols = ref [] in
      let data = List.filter (fun sym -> (
        let exists = List.exists (fun f -> f = sym.name) !written_symbols in
        if not exists then (
          written_symbols := !written_symbols @ [sym.name]
        );
        not exists
      )) m.symbols in *)
      vec symbol m.symbols;
      patch_gap32 g (pos s - p);
      
      List.iteri(fun i f ->
        match f.details with
        | Function when f.name = "caml_startup" ->  (
          u8 6; (* WASM_INIT_FUNCS *)
          let g = gap32 () in
          let p = pos s in
          vu32 1l;
          vu32 0l;
          vu32 (Int32.of_int i);
          patch_gap32 g (pos s - p)
        )
        | _ -> ()
      ) m.symbols

    let linking_section data =
      custom_section "linking" symbol_table data true

    let name_section_impl m =       
      u8 1; (* functions *)
      let g = gap32 () in
      let p = pos s in
      vu32 (Int32.of_int (List.length m.imports + List.length m.funcs));
      List.iteri (fun i import ->
        vu32 (Int32.of_int i);
        string (Ast.string_of_name import.item_name);        
      ) m.imports;
      List.iteri (fun i (f:Ast.func) ->
        vu32 (Int32.of_int (List.length m.imports + i));
        string f.name;        
      ) m.funcs;
      patch_gap32 g (pos s - p);

      u8 2; (* locals *)
      let g = gap32 () in
      let p = pos s in
      vu32 (Int32.of_int (List.length m.funcs));
      List.iteri(fun i f ->
        vu32 (Int32.of_int (List.length m.imports + i));
        vu32 (Int32.of_int (List.length f.locals));
        List.iteri(fun i (name, _) ->
          vu32 (Int32.of_int i);
          string name;
        ) f.locals
      ) m.funcs;
      patch_gap32 g (pos s - p)

    (* https://github.com/WebAssembly/wabt/blob/637e1fdb143aa180d57d87c09e604cb345d11020/src/binary-reader.cc#L1334 *)
    let name_section module_ = 
      custom_section "name" name_section_impl module_  true

      (* Module *)
    let module_ m =
      u32 0x6d736100l;
      u32 version;
      type_section m.types;
      import_section m.imports;
      func_section m.funcs;
      table_section m.tables;
      memory_section m.memories;
      global_section m.globals;
      export_section m.exports;
      start_section m.start;
      elem_section m.elems;
      code_section m.funcs;
      data_section m.data;
      linking_section m.data;
      if List.length !code_relocations > 0 then
        relocate_code_section m.funcs;
      if List.length !data_relocations > 0 then
        relocate_data_section m.funcs;
      name_section m
  end
  in E.module_ m; 
  encode_result := to_string s

let write_file output_name =
  let oc = open_out_bin output_name in
  output_string oc !encode_result;
  close_out oc;