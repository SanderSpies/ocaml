(* open Cmm *)
(* open Typed_cmm *)
open Ast
open Values
open Wasm_types


(* let name s =
  try Utf8.decode s with Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let oldify = 
    () *)

let hd_val = 
    [
        Const (I32 4l);
        Binary (I32 I32Op.Sub);
    ]

let wosize_hd = 
    [
        Const (I32 10l);
        Binary (I32 I32Op.ShrU);
    ]

let wosize_val =
    hd_val 
    @
    wosize_hd

let is_block value = 
    value
    @
    [
        Const (I32 1l);
        Binary (I32 I32Op.And);
        Const (I32 0l);
        Compare (I32 I32Op.Eq);
    ]

let is_young value = 
    value 
    @
    [
        GetGlobal "caml_young_end";
        Lt
    ]
    @ 
    value 
    @ 
    [
        GetGlobal "caml_young_start";
        Gt;
        Binary (I32 I32Op.And);
    ]