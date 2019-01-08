(module $libasmrun
    (type $t0 (func  (param i32) (param i32) (result i32)))
    (import "env" "$caml_alloc" (func $caml_alloc (type 0)))
    (export "caml_alloc" (func $caml_alloc))
)