use macros::BuiltinHashMap;
use std::{collections::HashMap, sync::LazyLock};

pub mod macros;
pub mod object_converter;
pub mod stdio;

pub static BUILT_INS: LazyLock<BuiltinHashMap> = LazyLock::new(|| {
    builtin_builder! {
        "print" => stdio::builtin_func_print,
        "println" => stdio::builtin_func_println,
        "input" => stdio::builtin_func_input,
        "clear" => stdio::builtin_func_clear_screen
    }
});
