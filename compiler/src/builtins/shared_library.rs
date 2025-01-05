use crate::Compiler;

macro_rules! compile_shared_library_func {
    ($def_name:ident, $func:expr, $return_type:expr, $($type:expr),*) => {
        pub fn $def_name(
            context: *mut gccjit_sys::gcc_jit_context,
            block: *mut gccjit_sys::gcc_jit_block,
            args: &mut Vec<*mut gccjit_sys::gcc_jit_rvalue>,
        ) -> *mut gccjit_sys::gcc_jit_rvalue {
            let return_type = $return_type(context);

            let func_ptr = unsafe { std::mem::transmute::<*mut (), *mut core::ffi::c_void>($func as *mut ()) };

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
                    0,
                )
            };

            let func = unsafe { gccjit_sys::gcc_jit_context_new_rvalue_from_ptr(context, func_type, func_ptr) };

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


extern "C" {
    fn cyrus_builtin__print(fmt: *const i8);
}

compile_shared_library_func!(builtin_print_func, cyrus_builtin__print, Compiler::void_type, 
                        Compiler::string_type
                        );
