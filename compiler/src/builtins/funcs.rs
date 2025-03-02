use crate::Compiler;
use gccjit_sys::*;
use std::{ffi::CString, ptr::null_mut};

pub fn builtin_func__sizeof(context: *mut gcc_jit_context) -> *mut gcc_jit_function {
    let func_name = CString::new("__cyrus_builtin__sizeof").unwrap();
    let rvalue_param_name = CString::new("rvalue").unwrap();
    let size_t_type = Compiler::size_t_type(context);
    let void_ptr_type = Compiler::void_ptr_type(context);

    let rvalue_param =
        unsafe { gcc_jit_context_new_param(context, null_mut(), void_ptr_type, rvalue_param_name.as_ptr()) };

    let func_ptr = unsafe {
        gcc_jit_context_new_function(
            context,
            null_mut(),
            gcc_jit_function_kind::GCC_JIT_FUNCTION_INTERNAL,
            size_t_type,
            func_name.as_ptr(),
            1,
            [rvalue_param].as_mut_ptr(),
            0,
        )
    };
    let block_name = CString::new("entry").unwrap();
    let block = unsafe { gcc_jit_function_new_block(func_ptr, block_name.as_ptr()) };

    let rvalue = unsafe { gcc_jit_param_as_rvalue(rvalue_param) };

    // ANCHOR 
    
    // gcc_jit_size

    // let builtin_func_name = CString::new("__builtin_object_size").unwrap();
    // let builtin_func_ptr = unsafe { gcc_jit_context_get_builtin_function(context, builtin_func_name.as_ptr()) };

    // let rvalue_size = unsafe { gcc_jit_context_new_call(context, null_mut(), builtin_func_ptr, 2, [
    //     rvalue,
    //     gcc_jit_context_new_rvalue_from_int(context, Compiler::i32_type(context), 3),
    // ].as_mut_ptr()) };

    // unsafe { gcc_jit_block_end_with_return(block, null_mut(), rvalue_size) };

    return func_ptr;
}
