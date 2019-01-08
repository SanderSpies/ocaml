(module $libasmrun
    (type $t0 (func (param i32) (param i32)  (result i32)))
    (func $caml_alloc  (type $t0) (param $wosize i32) (param $tag i32)  (result i32)
        get_local $wosize
        get_local $tag
        i32.add
    )
    (func $wabt_bug (type $t0) (param $wosize i32) (param $tag i32) (result i32)
        get_local $wosize
        get_local $tag
        call $caml_alloc
    )
    (export "caml_alloc" (func $caml_alloc))
)