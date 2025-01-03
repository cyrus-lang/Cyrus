use super::builtin_funcs::io::compile_builtin_printf_func;
use gccjit_sys::*;
use std::{collections::HashMap, sync::LazyLock};

pub type BuiltinFuncDef =
    fn(context: *mut gcc_jit_context, block: *mut gcc_jit_block, args: Vec<*mut gcc_jit_rvalue>) -> *mut gcc_jit_rvalue;

type BuiltinFuncsTable = HashMap<String, BuiltinFuncDef>;

macro_rules! builtin_builder {
    ($( $key:expr => $val:expr ),*) => {
        {
            let mut map: BuiltinFuncsTable = HashMap::new();
            $(map.insert($key.to_string(), $val);)*
            map
        }
    };
}

static BUILT_INS: LazyLock<BuiltinFuncsTable> = LazyLock::new(|| {
    builtin_builder! {
        "printf" => compile_builtin_printf_func
    }
});

pub fn retrieve_builtin_func(func_name: String) -> Option<BuiltinFuncDef> {
    let builtins_ref: &BuiltinFuncsTable = &*BUILT_INS;
    builtins_ref.get(&func_name).copied()
}
