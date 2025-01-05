use std::collections::HashMap;

use super::builtins::BUILT_INS;

pub type BuiltinFuncDef = fn(
    context: *mut gccjit_sys::gcc_jit_context,
    block: *mut gccjit_sys::gcc_jit_block,
    args: &mut Vec<*mut gccjit_sys::gcc_jit_rvalue>,
) -> *mut gccjit_sys::gcc_jit_rvalue;

pub type BuiltinFuncsTable = HashMap<String, BuiltinFuncDef>;

#[macro_export]
macro_rules! build_builtin_funcs {
    ($( $key:expr => $val:expr ),*) => {
        {
            let mut map: super::macros::BuiltinFuncsTable = std::collections::HashMap::new();
            $(map.insert($key.to_string(), $val);)*
            map
        }
    };
}

#[macro_export]
macro_rules! compile_shared_library_func {
    (
        $def_name:ident,
        fn $func_name:ident($($arg_name:ident: $arg_type:ty),*) -> $ret_type:ty,
        $func_ret_type:expr,
        $variadic:expr,
        $($type:expr),*
    ) => {
        pub fn $def_name(
            context: *mut gccjit_sys::gcc_jit_context,
            block: *mut gccjit_sys::gcc_jit_block,
            args: &mut Vec<*mut gccjit_sys::gcc_jit_rvalue>,
        ) -> *mut gccjit_sys::gcc_jit_rvalue {
            extern "C" {
                fn $func_name($($arg_name: $arg_type),*) -> $ret_type;
            }
        
            let func_ptr: unsafe extern "C" fn($($arg_name: $arg_type),*) -> $ret_type = unsafe {
                std::mem::transmute($func_name as unsafe extern "C" fn($($arg_name: $arg_type),*) -> $ret_type)
            };
            let func_ptr_c_void: *mut c_void = func_ptr as *mut c_void;

            let return_type = $func_ret_type(context);

            let mut param_types: Vec<*mut gccjit_sys::gcc_jit_type> = Vec::new();

            $(
                param_types.push($type(context));
            )*

            let func_type = unsafe {
                gccjit_sys::gcc_jit_context_new_function_ptr_type(
                    context,
                    std::ptr::null_mut(),
                    return_type,
                    args.len().try_into().unwrap(),
                    param_types.as_mut_ptr(),
                    $variadic,
                )
            };

            let func = unsafe { gccjit_sys::gcc_jit_context_new_rvalue_from_ptr(context, func_type, func_ptr_c_void) };

            let rvalue = unsafe {
                gccjit_sys::gcc_jit_context_new_call_through_ptr(
                    context,
                    std::ptr::null_mut(),
                    func,
                    args.len().try_into().unwrap(),
                    args.as_mut_ptr(),
                )
            };
            unsafe { gccjit_sys::gcc_jit_block_add_eval(block, std::ptr::null_mut(), rvalue) };

            return rvalue;
        }
    };
}

pub fn retrieve_builtin_func(func_name: String) -> Option<BuiltinFuncDef> {
    let builtins_ref: &BuiltinFuncsTable = &*BUILT_INS;
    builtins_ref.get(&func_name).copied()
}
