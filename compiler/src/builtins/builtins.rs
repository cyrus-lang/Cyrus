use std::sync::LazyLock;
use crate::{build_builtin_funcs, compile_shared_library_func, Compiler};
use super::macros::BuiltinFuncsTable;

extern "C" {
    fn cyrus_builtin__print(fmt: *const i8);
}

compile_shared_library_func!(builtin_print_func, cyrus_builtin__print, Compiler::void_type, 
                        Compiler::string_type
                        );

pub static BUILT_INS: LazyLock<BuiltinFuncsTable> = LazyLock::new(|| {
    build_builtin_funcs! {
        "print" => builtin_print_func
    }
});
