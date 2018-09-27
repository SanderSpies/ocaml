/* copied from runtime.js for now */

var env = {
  alloc: 0,
  caml_curry2: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry3: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry4: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry5: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry6: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry7: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry8: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry9: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry10: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry11: function() {
    throw new Error("Not implemented yet");
  },
  caml_apply4: function() {
    throw new Error("Not implemented yet");
  },
  caml_apply5: function() {
    throw new Error("Not implemented yet");
  },
  caml_alloc: function(amount) {
    const result = env.alloc;
    env.alloc += amount;
    return result;
  },
  caml_ml_set_binary_mode: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_close_channel: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_channel_size: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_pos_in: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_seek_in: function() {
    throw new Error("Not implemented yet");
  },
  caml_input_value: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_input_int: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_input_char: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_pos_out: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_seek_out: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_output_int: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_output_char: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_flush: function() {
    throw new Error("Not implemented yet");
  },
  jsRaise_i32_i32: function() {
    throw new Error("Not implemented yet");
  },
  caml_lessequal: function() {
    throw new Error("Not implemented yet");
  },
  caml_greaterequal: function() {
    throw new Error("Not implemented yet");
  },
  caml_create_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_blit_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_format_int: function() {
    throw new Error("Not implemented yet");
  },
  caml_int_of_string: function() {
    throw new Error("Not implemented yet");
  },
  jsRaise_i32_unit: function() {
    throw new Error("Not implemented yet");
  },
  caml_format_float: function() {
    throw new Error("Not implemented yet");
  },
  caml_float_of_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_open: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_open_descriptor_out: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_set_channel_name: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_out_channels_list: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_output_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_output: function() {
    throw new Error("Not implemented yet");
  },
  caml_output_value: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_open_descriptor_in: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_input: function() {
    throw new Error("Not implemented yet");
  },
  caml_blit_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_input_scan_line: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_seek_out_64: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_pos_out_64: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_channel_size_64: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_seek_in_64: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_pos_in_64: function() {
    throw new Error("Not implemented yet");
  },
  caml_modify: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_exit: function() {
    throw new Error("Not implemented yet");
  },
  caml_register_named_value: function() {
    throw new Error("Not implemented yet");
  },
  caml_fresh_oo_id: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_float_of_bits_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_compare: function() {
    throw new Error("Not implemented yet");
  },
  caml_int_compare: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_runtime_warnings_enabled: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_enable_runtime_warnings: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_getenv: function() {
    throw new Error("Not implemented yet");
  },
  caml_install_signal_handler: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_get_argv: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_get_config: function() {
    throw new Error("Not implemented yet");
  },
  caml_bytes_equal: function() {
    throw new Error("Not implemented yet");
  },
  caml_fill_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_bytes_compare: function() {
    throw new Error("Not implemented yet");
  },
  caml_string_equal: function() {
    throw new Error("Not implemented yet");
  },
  caml_string_compare: function() {
    throw new Error("Not implemented yet");
  },
  caml_output_value_to_buffer: function() {
    throw new Error("Not implemented yet");
  },
  caml_marshal_data_size: function() {
    throw new Error("Not implemented yet");
  },
  caml_input_value_from_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_floatarray_get: function() {
    throw new Error("Not implemented yet");
  },
  caml_floatarray_set: function() {
    throw new Error("Not implemented yet");
  },
  caml_output_value_to_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_obj_tag: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_create: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_get_key: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_get_key_copy: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_set_key: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_unset_key: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_check_key: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_blit_key: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_get_data: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_get_data_copy: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_set_data: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_unset_data: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_check_data: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_blit_data: function() {
    throw new Error("Not implemented yet");
  },
  caml_float_compare_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_hash: function() {
    throw new Error("Not implemented yet");
  },
  caml_array_concat: function() {
    throw new Error("Not implemented yet");
  },
  caml_make_float_vect: function() {
    throw new Error("Not implemented yet");
  },
  caml_make_vect: function() {
    throw new Error("Not implemented yet");
  },
  caml_array_sub: function() {
    throw new Error("Not implemented yet");
  },
  caml_array_append: function() {
    throw new Error("Not implemented yet");
  },
  caml_array_blit: function() {
    throw new Error("Not implemented yet");
  },
  caml_int32_format: function() {
    throw new Error("Not implemented yet");
  },
  caml_int32_of_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_int32_compare_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_add: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_sub: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_neg: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_xor: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_format: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_of_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_compare_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_nativeint_format: function() {
    throw new Error("Not implemented yet");
  },
  caml_nativeint_of_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_nativeint_compare_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_lex_engine: function() {
    throw new Error("Not implemented yet");
  },
  caml_new_lex_engine: function() {
    throw new Error("Not implemented yet");
  },
  caml_set_parser_trace: function() {
    throw new Error("Not implemented yet");
  },
  caml_parse_engine: function() {
    throw new Error("Not implemented yet");
  },
  caml_notequal: function() {
    throw new Error("Not implemented yet");
  },
  caml_obj_set_tag: function() {
    throw new Error("Not implemented yet");
  },
  caml_obj_block: function() {
    throw new Error("Not implemented yet");
  },
  caml_lazy_make_forward: function() {
    throw new Error("Not implemented yet");
  },
  caml_hexstring_of_float: function() {
    throw new Error("Not implemented yet");
  },
  caml_classify_float_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_equal: function() {
    throw new Error("Not implemented yet");
  },
  caml_raw_backtrace_next_slot: function() {
    throw new Error("Not implemented yet");
  },
  caml_convert_raw_backtrace_slot: function() {
    throw new Error("Not implemented yet");
  },
  caml_raw_backtrace_slot: function() {
    throw new Error("Not implemented yet");
  },
  caml_raw_backtrace_length: function() {
    throw new Error("Not implemented yet");
  },
  caml_get_current_callstack: function() {
    throw new Error("Not implemented yet");
  },
  caml_get_exception_raw_backtrace: function() {
    throw new Error("Not implemented yet");
  },
  caml_backtrace_status: function() {
    throw new Error("Not implemented yet");
  },
  caml_record_backtrace: function() {
    throw new Error("Not implemented yet");
  },
  caml_convert_raw_backtrace: function() {
    throw new Error("Not implemented yet");
  },
  caml_apply6: function() {
    throw new Error("Not implemented yet");
  },
  caml_final_release: function() {
    throw new Error("Not implemented yet");
  },
  caml_final_register_called_without_value: function() {
    throw new Error("Not implemented yet");
  },
  caml_final_register: function() {
    throw new Error("Not implemented yet");
  },
  caml_gc_stat: function() {
    throw new Error("Not implemented yet");
  },
  caml_gc_counters: function() {
    throw new Error("Not implemented yet");
  },
  caml_md5_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_md5_chan: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_random_seed: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_of_int: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_shift_left: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_or: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_mod: function() {
    throw new Error("Not implemented yet");
  },
  caml_greaterthan: function() {
    throw new Error("Not implemented yet");
  },
  caml_obj_dup: function() {
    throw new Error("Not implemented yet");
  },
  caml_hash_univ_param: function() {
    throw new Error("Not implemented yet");
  },
  caml_tuplify2: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_blit: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_check: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_get_copy: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_get: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_set: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_create: function() {
    throw new Error("Not implemented yet");
  },
  caml_obj_truncate: function() {
    throw new Error("Not implemented yet");
  },
  caml_lessthan: function() {
    throw new Error("Not implemented yet");
  },
  caml_set_oo_id: function() {
    throw new Error("Not implemented yet");
  },
  caml_make_array: function() {
    throw new Error("Not implemented yet");
  },
  caml_send0: function() {
    throw new Error("Not implemented yet");
  },
  caml_string_notequal: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_close: function() {
    throw new Error("Not implemented yet");
  },
  sqrt: function() {
    throw new Error("Not implemented yet");
  },
  atan2: function() {
    throw new Error("Not implemented yet");
  },
  cos: function() {
    throw new Error("Not implemented yet");
  },
  sin: function() {
    throw new Error("Not implemented yet");
  },
  exp: function() {
    throw new Error("Not implemented yet");
  },
  log: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_enabled: function() {
    throw new Error("Not implemented yet");
  },
  caml_register_channel_for_spacetime: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_write_magic_number: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_save_event: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_save_trie: function() {
    throw new Error("Not implemented yet");
  },
  caml_gc_minor: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_take_snapshot: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_save_event_for_automatic_snapshots: function() {
    throw new Error("Not implemented yet");
  }
};

let count = 0;
let globalInstance = null;
const importObject = {
  env,
  console: {
    log: function(e) {
      console.log("[ocaml-wasm] ", e);
    }
  },
  linking: {
    caml_curry2() {
      return 22;
    }
  },
  js: {
    tryWith: function(memPos, tryBody_, withHandler_) {
      const table = globalInstance.exports.table;
      const tryBody = table.get(tryBody_);
      const withHandler = table.get(withHandler_);
      try {
        tryBody(memPos);
      } catch (e) {
        withHandler(memPos, e);
      }
    },
    raise(e) {
      throw e;
    },
    caml_fresh_oo_id(v) {
      count++;
    }
  }
};

function fetchAndInstantiate(url) {
  return fetch(url)
    .then(response => response.arrayBuffer())
    .then(bytes => WebAssembly.instantiate(bytes, importObject))
    .then(results => results.instance);
}

function ocamlInt(i) {
  return (i << 1) + 1;
}

function jsInt(i) {
  if (i & (1 === 0)) throw Error("Expected an OCaml int, but got a pointer");
  return i >> 1;
}

describe("TEST", () => {
  describe("xxx", () => {
    it("should work", done => {
      fetchAndInstantiate("/base/test/curried_function.wasm")
        .then(instance => {
          console.error("THIS WORKS");
          done();
        })
        .catch(e => done(e));
    });
  });
});

describe("functions", () => {
  describe("noncurried function", () => {
    it("should return 20 when 5 is given", done => {
      fetchAndInstantiate("/base/test/noncurried_function.wasm")
        .then(instance => {
          const func =
            instance.exports.camlNoncurried_function__noncurried_function_1002;
          let calculatedValue = func(ocamlInt(5));
          expect(jsInt(calculatedValue)).to.equal(20);
          calculatedValue = func(ocamlInt(100));
          expect(jsInt(calculatedValue)).to.equal(115);
          done();
        })
        .catch(e => done(e));
    });
  });
  describe("curried functions", () => {
    it("should return 30 when 6 and 20 is given - direct pointer", done => {
      fetchAndInstantiate("/base/test/curried_function.wasm")
        .then(instance => {
          let func =
            instance.exports.camlCurried_function__curried_function_1002;
          env.alloc = instance.exports.__heap_base;
          instance.exports._start();
          let calculatedValue = func(ocamlInt(8), ocamlInt(20));
          expect(jsInt(calculatedValue)).to.equal(30);

          func = instance.exports.camlCurried_function__curried_function_3_1006;
          calculatedValue = func(ocamlInt(20));
          expect(jsInt(calculatedValue)).to.equal(29);
          done();
        })
        .catch(e => done(e));
    });
  });
  //
  //     // this function needs a change in wasmgen.ml - an extra argument needs to be added
  //     // to functions which have a fun_dbg list > 0
  //     it('should return 30 when 6 and 20 is given - caml_curry2', done => {
  //       fetchAndInstantiate("/base/test/curried_function.wasm").then(instance => {
  //         var i8 = new Uint8Array(instance.exports.memory.buffer);
  //         expect(i8[0]).to.equal(0);
  //         expect(instance.exports.caml_program()).to.equal(1);
  //         expect(i8[0]).to.equal(4);
  //         var i32_ = new Uint32Array(instance.exports.memory.buffer.slice(6, 14));
  //         expect(i32_[0]).to.equal(1792);
  //         expect(i32_[1]).to.equal(26);
  //
  //         // the pointer to caml_curry2
  //         var i32 = new Uint32Array(instance.exports.memory.buffer.slice(i32_[1] + 0, i32_[1] + 4));
  //         expect(i32[0]).to.equal(9);
  //
  //         // invoke caml_curry2
  //         let camlCurry2 = instance.exports.table.get(i32[0]);
  //         const allocatedMemoryAddress = camlCurry2(ocamlInt(6), i32_[1]);
  //         var i32 = new Uint32Array(instance.exports.memory.buffer.slice(allocatedMemoryAddress, allocatedMemoryAddress + 20));
  //
  //         // calculated value is a pointer to a allocated memory block
  //         // var i32 = new Uint32Array(instance.exports.memory.buffer.slice(allocatedMemoryAddress + 4, allocatedMemoryAddress + 20));
  //         let caml_curry2_1 = instance.exports.table.get(i32[0]);
  //
  //         const cv2 = caml_curry2_1(ocamlInt(20), allocatedMemoryAddress);
  //         expect(jsInt(cv2)).to.equal(30);
  //
  //         const cv3 = caml_curry2_1(ocamlInt(10), allocatedMemoryAddress + 4);
  //         expect(jsInt(cv3)).to.equal(20);
  //         done();
  //       })
  //       .catch(e => done(e))
  //     });
  //   });
  //
  //   // it('should support a simple crud', done => {
  //   //   fetchAndInstantiate("/base/test/crud.wasm").then(instance => {
  //   //     var i8 = new Uint8Array(instance.exports.memory.buffer);
  //   //     expect(i8[0]).to.equal(0);
  //   //     expect(instance.exports.caml_program()).to.equal(1);
  //   //     expect(i8[0]).to.equal(4);
  //   //     let dev = instance.exports.camlCrud__create_1211("Foo", 0);
  //   //     let dev2 = instance.exports.camlCrud__create_1211("Foo2", 0);
  //   //
  //   //
  //   //     done();
  //   //   })
  //   //   .catch(e => done(e))
  //   // });
  //
  //
  // });
  //
  //
  // describe('exception handling', () => {
  //   describe('raise + try with', () => {
  //     it ('should support basic exception handling', done => {
  //       fetchAndInstantiate("/base/test/exception_handling.wasm").then(instance => {
  //
  //         expect(instance.exports.caml_program()).to.equal(1);
  //
  //         globalInstance = instance;
  //         try {
  //           instance.exports.camlException_handling__other_1006();
  //         } catch (pointer) {
  //           var i32 = new Uint32Array(instance.exports.memory.buffer.slice(pointer + 0, pointer + 64));
  //           expect(i32[0]).to.equal(882);
  //           expect(i32[1]).to.equal(1);
  //         }
  //         try {
  //           instance.exports.camlException_handling__other2_1008();
  //         } catch (pointer) {
  //           var i32 = new Uint32Array(instance.exports.memory.buffer.slice(pointer + 0, pointer + 8));
  //           expect(i32[0]).to.equal(882);
  //           expect(i32[1]).to.equal(3);
  //         }
  //
  //         expect(instance.exports.camlException_handling__foo_1207(55)).to.equal(ocamlInt(500));
  //
  //         done();
  //       })
  //       .catch(e => { console.debug('what:', e); done(e) })
  //     })
  //   });
  // });
  //
  // describe('loop', () => {
  //   describe('loop', () => {
  //     it ('should loop', done => {
  //       fetchAndInstantiate("/base/test/loop.wasm").then(instance => {
  //         expect(instance.exports.caml_program()).to.equal(1);
  //         expect(instance.exports.camlLoop__a_1002(ocamlInt(3))).to.equal(ocamlInt(2950));
  //         done();
  //       })
  //       .catch(done)
  //     });
  //   });
  // });
  //
  // describe('switch', () => {
  //   describe('switch', () => {
  //     it ('should switch statements', done => {
  //       fetchAndInstantiate("/base/test/switch.wasm").then(instance => {
  //         expect(instance.exports.caml_program()).to.equal(1);
  //         expect(instance.exports.camlSwitch__small_switch_test1_1015()).to.equal(ocamlInt(222))
  //         expect(instance.exports.camlSwitch__small_switch_test2_1018()).to.equal(ocamlInt(70))
  //
  //         expect(instance.exports.camlSwitch__big_switch_test1_1021()).to.equal(ocamlInt(222))
  //         expect(instance.exports.camlSwitch__big_switch_test2_1024()).to.equal(ocamlInt(70));
  //         expect(instance.exports.camlSwitch__big_switch_test3_1027()).to.equal(ocamlInt(20));
  //         expect(instance.exports.camlSwitch__big_switch_test4_1030()).to.equal(ocamlInt(22));
  //
  //         expect(instance.exports.camlSwitch__big_switch_test5_1033()).to.equal(ocamlInt(30));
  //         expect(instance.exports.camlSwitch__big_switch_test6_1036()).to.equal(ocamlInt(70));
  //         expect(instance.exports.camlSwitch__big_switch_test7_1039()).to.equal(ocamlInt(40));
  //         expect(instance.exports.camlSwitch__big_switch_test8_1042()).to.equal(ocamlInt(70));
  //
  //         done();
  //       })
  //       .catch(e => { done(e) })
  //     });
  //   });
  // });
  //
  // describe('array', () => {
  //   describe('simple', () => {
  //     it ('should retrieve an array value', done => {
  //       fetchAndInstantiate("/base/test/array.wasm").then(instance => {
  //         expect(instance.exports.caml_program()).to.equal(1);
  //         expect(instance.exports.camlArray2__ala_1005()).to.equal(ocamlInt(10000000));
  //         expect(instance.exports.camlArray2__bla_1008()).to.equal(ocamlInt(2));
  //         try {
  //           instance.exports.camlArray2__cla_1011();
  //           done('should give an exception');
  //         }
  //         catch(pointer) {
  //           expect(pointer).to.equal(186);
  //         }
  //         try {
  //           instance.exports.camlArray2__dla_1014();
  //           done('should give an exception');
  //         }
  //         catch(pointer) {
  //           expect(pointer).to.equal(186);
  //         }
  //         done();
  //       })
  //       .catch(e => { done(e) })
  //     });
  //   });
  // });
  //
  //
  // describe('arithmetic', () => {
  //   describe('int', () => {
  //     it ('should support basic arithmetic', done => {
  //       fetchAndInstantiate("/base/test/arithmetic.wasm").then(instance => {
  //         expect(instance.exports.caml_program()).to.equal(1);
  //         expect(instance.exports.camlArithmetic__addi_1002(ocamlInt(5), ocamlInt(6))).to.equal(ocamlInt(11));
  //
  //         expect(instance.exports.camlArithmetic__mini_1005(ocamlInt(10), ocamlInt(5))).to.equal(ocamlInt(5));
  //         expect(instance.exports.camlArithmetic__divi_1008(ocamlInt(10), ocamlInt(5))).to.equal(ocamlInt(2));
  //         expect(instance.exports.camlArithmetic__muli_1011(ocamlInt(10), ocamlInt(5))).to.equal(ocamlInt(50));
  //
  //         expect(instance.exports.camlArithmetic__modi_1014(ocamlInt(10), ocamlInt(3))).to.equal(ocamlInt(1));
  //         expect(instance.exports.camlArithmetic__modi_1014(ocamlInt(99), ocamlInt(3))).to.equal(ocamlInt(0));
  //         expect(instance.exports.camlArithmetic__modi_1014(ocamlInt(101), ocamlInt(3))).to.equal(ocamlInt(2));
  //
  //         expect(instance.exports.camlArithmetic__land__1017(ocamlInt(10), ocamlInt(3))).to.equal(ocamlInt(2));
  //         expect(instance.exports.camlArithmetic__lor__1020(ocamlInt(4), ocamlInt(2))).to.equal(ocamlInt(6));
  //         expect(instance.exports.camlArithmetic__lxor__1023(ocamlInt(10), ocamlInt(3))).to.equal(ocamlInt(9));
  //         expect(instance.exports.camlArithmetic__lsl__1026(ocamlInt(10), ocamlInt(1))).to.equal(ocamlInt(20));
  //         expect(instance.exports.camlArithmetic__lsr__1029(ocamlInt(10), ocamlInt(1))).to.equal(ocamlInt(5));
  //         expect(instance.exports.camlArithmetic__asr__1032(ocamlInt(10), ocamlInt(1))).to.equal(ocamlInt(5));
  //         done();
  //       })
  //       .catch(e => { done(e) })
  //     })
  //   });
  //   describe('float', () => {
  //     xit ('should support basic arithmetic', done => {
  //       fetchAndInstantiate("/base/test/arithmetic.wasm").then(instance => {
  //         /* floats work, but tests need to be fixed - need to figure how one can access the memory from js */
  //         //
  //         //
  //         //
  //         // expect(instance.exports.caml_program()).to.equal(1);
  //         //
  //         // let alloc = instance.exports.table.get(3);
  //         // let addr = alloc(8);
  //         //
  //         // const x = new Float32Array(instance.exports.memory.buffer.slice(addr, addr + 12));
  //         // x[0] = 5;
  //         // x[1] = 12;
  //         //
  //         // instance.exports.memory
  //         // const x21 = new Float32Array(instance.exports.memory.buffer.slice(addr, addr + 12));
  //         // console.debug('riiight1:', x21[0], x21[1], x21[2], x[0], x[1]);
  //         //
  //         // var pointer = instance.exports.camlArithmetic__divf_1043(addr, addr + 4);
  //         // const x2 = new Float32Array(instance.exports.memory.buffer.slice(pointer, pointer + 4));
  //         // console.debug('riiight:', x2[0]);
  //
  //         done();
  //       })
  //       .catch(e => { done(e) })
  //     })
  //   });
});
