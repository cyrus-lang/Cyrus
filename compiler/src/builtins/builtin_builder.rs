use std::{collections::HashMap, sync::LazyLock};

use crate::builtins::shared_library;

pub type BuiltinFuncDef = fn(
    context: *mut gccjit_sys::gcc_jit_context,
    block: *mut gccjit_sys::gcc_jit_block,
    args: &mut Vec<*mut gccjit_sys::gcc_jit_rvalue>,
) -> *mut gccjit_sys::gcc_jit_rvalue;

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
        "print" => shared_library::builtin_print_func
    }
});

pub fn retrieve_builtin_func(func_name: String) -> Option<BuiltinFuncDef> {
    let builtins_ref: &BuiltinFuncsTable = &*BUILT_INS;
    builtins_ref.get(&func_name).copied()
}
