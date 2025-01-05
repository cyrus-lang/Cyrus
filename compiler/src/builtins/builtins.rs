use super::macros::BuiltinFuncsTable;
use crate::{build_builtin_funcs, compile_shared_library_func, Compiler};
use std::{ffi::c_void, os::raw::c_char, sync::LazyLock};

compile_shared_library_func!(
    builtin_print_func,
    fn cyrus_builtin__printf(fmt: *const c_char) -> *mut c_void,
    Compiler::void_type,
    1,
    Compiler::string_type
);

pub static BUILT_INS: LazyLock<BuiltinFuncsTable> = LazyLock::new(|| {
    build_builtin_funcs! {
        "printf" => builtin_print_func
    }
});
