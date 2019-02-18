(module 
    (type $t0 (func  (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) ))
    (type $t1 (func (result i32)))
    (type $t2 (func ))

    (import "env" "$caml_init_gc" (func $caml_init_gc (type 0)))
    (import "env" "$caml_program" (func $caml_program (type 1)))

    (func $caml_main
        i32.const 262144 ;; caml_init_minor_heap_wsz
        i32.const 126976 ;; caml_init_heap_wsz
        i32.const 15 ;; caml_init_heap_chunk_sz
        i32.const 80 ;; caml_init_percent_free
        i32.const 500 ;; caml_init_max_percent_free
        i32.const 1 ;; caml_init_major_window
        call $caml_init_gc 

        call $caml_program
        drop
    )

    (func $_start (export "_start") (type 2) 
        call $caml_main
    )

    (func $pleh (export "pleh") (type 2) 
        call $_start
    )

)
