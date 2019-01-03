(*
 * Throughout the implementation we use consistent naming conventions for
 * syntactic elements, associated with the types defined here and in a few
 * other places:
 *
 *   x : var
 *   v : value
 *   e : instrr
 *   f : func
 *   m : module_
 *
 *   t : value_type
 *   s : func_type
 *   c : context / config
 *
 * These conventions mostly follow standard practice in language semantics.
 *)

(* TODO: introduce symbol variable and use it instead! *)


module Types = Wasm_types

open Types

(* Operators *)

module IntOp =
struct
  type unop = Clz | Ctz | Popcnt
  type binop = Add | Sub | Mul | DivS | DivU | RemS | RemU
             | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
  type testop = Eqz
  type relop = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
  type cvtop = ExtendSI32 | ExtendUI32 | WrapI64
             | TruncSF32 | TruncUF32 | TruncSF64 | TruncUF64
             | ReinterpretFloat
end

module FloatOp =
struct
  type unop = Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt
  type binop = Add | Sub | Mul | Div | Min | Max | CopySign
  type testop
  type relop = Eq | Ne | Lt | Gt | Le | Ge
  type cvtop = ConvertSI32 | ConvertUI32 | ConvertSI64 | ConvertUI64
             | PromoteF32 | DemoteF64
             | ReinterpretInt
end

module I32Op = IntOp
module I64Op = IntOp
module F32Op = FloatOp
module F64Op = FloatOp

type unop = (I32Op.unop, I64Op.unop, F32Op.unop, F64Op.unop) Values.op
type binop = (I32Op.binop, I64Op.binop, F32Op.binop, F64Op.binop) Values.op
type testop = (I32Op.testop, I64Op.testop, F32Op.testop, F64Op.testop) Values.op
type relop = (I32Op.relop, I64Op.relop, F32Op.relop, F64Op.relop) Values.op
type cvtop = (I32Op.cvtop, I64Op.cvtop, F32Op.cvtop, F64Op.cvtop) Values.op

type mem_size = Mem8 | Mem16 | Mem32
type extension = SX | ZX

type 'a memop =
  {ty : value_type; align : int; offset : int32; sz : 'a option}
type loadop = (mem_size * extension) memop
type storeop = mem_size memop


(* Expressions *)

type var = int32
type literal = Values.value
type name = int list

type instr =
  | Unreachable                       (* trap unconditionally *)
  | Nop                               (* do nothing *)
  | Block of stack_type * instr list  (* execute in sequence *)
  | Loop of stack_type * instr list   (* loop header *)
  | If of stack_type * instr list * instr list  (* conditional *)
  | Br of var                         (* break to n-th surrounding label *)
  | BrIf of var                       (* conditional break *)
  | BrTable of var list * var         (* indexed break *)
  | Return                            (* break from function body *)
  | Call of (string * instr list list)     (* call function *)
  | CallIndirect of (string * instr list list)    (* call function through table *)
  | Drop                              (* forget a value *)
  | Select                            (* branchless conditional *)
  | GetLocal of var                   (* read local variable *)
  | SetLocal of (var * instr list)    (* write local variable *)
  | TeeLocal of var                   (* write local variable and keep value *)
  | GetGlobal of string               (* read global variable *)
  | SetGlobal of string               (* write global variable *)
  | Load of loadop                    (* read memory at address *)
  | Store of storeop                  (* write memory at address *)
  | CurrentMemory                     (* size of linear memory *)
  | GrowMemory                        (* grow linear memory *)
  | Const of literal                  (* constant *)
  | DataSymbol of string
  | FuncSymbol of string
  | Test of testop                    (* numeric test *)
  | Compare of relop                  (* numeric comparison *)
  | Unary of unop                     (* unary numeric operator *)
  | Binary of binop                   (* binary numeric operator *)
  | Convert of cvtop                  (* conversion *)


(* Globals & Functions *)

type const = instr list

type global =
{
  name : string;
  gtype : global_type;
  value : const;
}

type func =
{
  name: string;
  ftype : string;
  locals : (string * value_type) list;
  body : instr list;
  no_of_args: int;
}


(* Tables & Memories *)

type table =
{
  ttype : table_type;
}

type memory =
{
  mtype : memory_type;
}

type 'data segment =
{
  index : var;
  offset : const;
  init : 'data;
}

type table_segment = var list segment
type memory_segment = string segment


(* Modules *)

type type_ = {
  tname: string;
  tdetails: func_type
}

type export_desc =
  | FuncExport of string
  | TableExport of var
  | MemoryExport of var
  | GlobalExport of var

type export =
{
  name : name;
  edesc : export_desc;
}

type import_desc =
  | FuncImport of string
  | TableImport of table_type
  | MemoryImport of memory_type
  | GlobalImport of global_type

type import =
{
  module_name : name;
  item_name : name;
  idesc : import_desc;
}

type data_part_detail =
  | String of string
  | Int32 of int32
  | Nativeint of nativeint
  | Int16 of int
  | Int8 of int
  | Float32 of F32.t
  | Symbol of string
  | FunctionLoc of string

type data_part = {
  name: string;
  detail: data_part_detail list
}

type sym_info_function = {
  index: var;
}

type sym_info_data = {
  index: var;
  relocation_offset: var;
  size: var;
  offset: var;
}

type sym_info_details =
  | Function
  | Import of (stack_type * stack_type)
  | Global of sym_info_function
  | Data of sym_info_data

type sym_info = {
  name: string;
  details: sym_info_details;
}

type module_ =
{
  types : type_ list;
  globals : global list;
  tables : table list;
  memories : memory list;
  funcs : func list;
  start : var option;
  elems : var list segment list;
  data : data_part segment list;
  imports : import list;
  exports : export list;
  symbols : sym_info list;
}


(* Auxiliary functions *)

let empty_module =
{
  types = [];
  globals = [];
  tables = [];
  memories = [];
  funcs = [];
  start = None;
  elems  = [];
  data = [];
  imports = [];
  exports = [];
  symbols = [];
}

let string_of_name n =
  let b = Buffer.create 16 in
  let escape uc =
    if uc < 0x20 || uc >= 0x7f then
      Buffer.add_string b (Printf.sprintf "\\u{%02x}" uc)
    else begin
      let c = Char.chr uc in
      if c = '\"' || c = '\\' then Buffer.add_char b '\\';
      Buffer.add_char b c
    end
  in
  List.iter escape n;
  Buffer.contents b
