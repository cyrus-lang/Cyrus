use gccjit_sys::{gcc_jit_context, gcc_jit_rvalue};
use std::{collections::HashMap, sync::LazyLock};

use super::funcs::builtin_func__len;
use crate::builtins::funcs::builtin_func__sizeof;

pub type BuiltinFuncDef =
    fn(file_path: String, context: *mut gcc_jit_context, args: Vec<*mut gcc_jit_rvalue>) -> *mut gcc_jit_rvalue;

pub type BuiltinFuncsTable = HashMap<String, BuiltinFuncDef>;

#[macro_export]
macro_rules! build_builtin_funcs {
    ($( $key:expr => $val:expr ),*) => {
        {
            let mut map: BuiltinFuncsTable = HashMap::new();
            $(map.insert($key.to_string(), $val);)*
            map
        }
    };
}

pub static BUILTIN_FUNCS: LazyLock<BuiltinFuncsTable> = LazyLock::new(|| {
    build_builtin_funcs! {
        "len" => builtin_func__len,
        "sizeof" => builtin_func__sizeof
    }
});
