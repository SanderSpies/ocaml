(module $gc_ctrl

    (type $t0 (func  (param i32)))
    
    (import "env" "$caml_set_minor_heap_size" (func $caml_set_minor_heap_size (type 0)))

    (func $caml_init_gc (export "caml_init_gc")
        (param $caml_init_minor_heap_wsz i32)
        (param $caml_init_heap_wsz i32)
        (param $caml_init_heap_chunk_sz i32)
        (param $caml_init_percent_free i32)
        (param $caml_init_max_percent_free i32)
        (param $caml_init_major_window i32)

        get_local $caml_init_minor_heap_wsz
        call $caml_set_minor_heap_size
    )

    (func $foo
        i32.const 1
        i32.const 2
        i32.const 3
        i32.const 4
        i32.const 5
        i32.const 6
        call $caml_init_gc
    )
)