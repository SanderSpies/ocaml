(module 
    (type $t0 (func  (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) ))
    (type $t1 (func (result i32)))
    (type $t2 (func ))
(import "env" "$caml_program" (func $caml_program (type 1)))

 (global $__heap_base i32 (i32.const 2))
    
    (global $caml_young_base (mut i32) (i32.const 0))
    (global $caml_young_start (mut i32) (i32.const 0))
    (global $caml_young_end (mut i32) (i32.const 0))
    (global $caml_young_alloc_start (mut i32) (i32.const 0))
    (global $caml_young_alloc_mid (mut i32) (i32.const 0))
    (global $caml_young_alloc_end (mut i32) (i32.const 0))
    (global $caml_young_trigger (mut i32) (i32.const 0))
    (global $caml_young_limit (mut i32) (i32.const 0))
    (global $caml_young_ptr (mut i32) (i32.const 0))
    (global $caml_minor_heap_wsz (mut i32) (i32.const 0))

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

    (func $caml_set_minor_heap_size (export "caml_set_minor_heap_size")
        (param $bsz i32)
        get_global $__heap_base
        set_global $caml_young_base

        get_global $__heap_base
        set_global $caml_young_start

        get_global $__heap_base
        get_local $bsz
        i32.add
        set_global $caml_young_end

        get_global $caml_young_start
        set_global $caml_young_alloc_start

        ;; get_global $caml_young_alloc_start
        ;; get_local $bsz
        ;; i32.const 8
        ;; i32.div_u
        ;; i32.add
        ;; set_global $caml_young_alloc_mid

        ;; get_global $caml_young_end
        ;; set_global $caml_young_alloc_end

        ;; get_global $caml_young_alloc_start
        ;; set_global $caml_young_trigger

        ;; get_global $caml_young_trigger
        ;; set_global $caml_young_limit

        ;; get_global $caml_young_alloc_end
        ;; set_global $caml_young_ptr

        ;; get_local $bsz
        ;; i32.const 4
        ;; i32.div_u
        ;; set_global $caml_minor_heap_wsz
    )

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
