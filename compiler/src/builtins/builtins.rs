use gccjit_sys::{gcc_jit_context_new_rvalue_from_int, gcc_jit_rvalue_get_type};

use super::macros::BuiltinFuncsTable;
use crate::{build_builtin_funcs, compile_shared_library_variadic_func, Compiler};
use std::{
    ffi::{c_int, c_void},
    sync::LazyLock,
};

compile_shared_library_variadic_func!(
    builtin_print_func,
    cyrus_builtin__printf,
    *mut c_void,
    Compiler::void_type
);

pub static BUILT_INS: LazyLock<BuiltinFuncsTable> = LazyLock::new(|| {
    build_builtin_funcs! {
        "printf" => builtin_print_func
    }
});
