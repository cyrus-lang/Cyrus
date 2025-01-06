use gccjit_sys::{gcc_jit_context_new_rvalue_from_int, gcc_jit_rvalue_get_type};

use super::macros::BuiltinFuncsTable;
use crate::{build_builtin_funcs, compile_shared_library_func, compile_shared_library_variadic_func, Compiler};
use std::{
    ffi::{c_int, c_void},
    sync::LazyLock,
};

compile_shared_library_func!(
    builtin_free_object_func,
    fn cyrus_builtin__free_object(obj: *mut c_void) -> *mut c_void,
    Compiler::void_type,
    Compiler::void_type
);

compile_shared_library_variadic_func!(
    builtin_cprintf_func,
    cyrus_builtin__cprintf,
    *mut c_void,
    Compiler::void_type
);

pub static BUILT_INS: LazyLock<BuiltinFuncsTable> = LazyLock::new(|| {
    build_builtin_funcs! {
        "cprintf" => builtin_cprintf_func,
        "free" => builtin_free_object_func
    }
});
