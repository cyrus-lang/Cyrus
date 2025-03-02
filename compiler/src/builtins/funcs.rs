use crate::Compiler;
use gccjit_sys::*;
use std::{ffi::CString, ptr::null_mut};

pub fn builtin_func__sizeof(context: *mut gcc_jit_context) -> *mut gcc_jit_function {
    let func_name = CString::new("__cyrus_builtin__sizeof").unwrap();
    let rvalue_param_name = CString::new("rvalue").unwrap();
    let size_t_type = Compiler::size_t_type(context);

    // Change the parameter type to the specific type you want to get the size of
    let int_type = unsafe { gcc_jit_context_get_int_type(context, 0, 1) }; // Example: int type
    let int_ptr_type = unsafe { gcc_jit_type_get_pointer(int_type) };

    let rvalue_param =
        unsafe { gcc_jit_context_new_param(context, null_mut(), int_ptr_type, rvalue_param_name.as_ptr()) };

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

    // Get the type of the parameter directly
    let rvalue_type = unsafe { gcc_jit_rvalue_get_type(gcc_jit_param_as_rvalue(rvalue_param)) };
    let rvalue_size = unsafe { gcc_jit_context_new_sizeof(context, rvalue_type) };

    unsafe { gcc_jit_block_end_with_return(block, null_mut(), rvalue_size) };

    return func_ptr;
}

// pub fn builtin_func__sizeof(context: *mut gcc_jit_context) -> *mut gcc_jit_function {
//     let func_name = CString::new("__cyrus_builtin__sizeof").unwrap();
//     let rvalue_param_name = CString::new("rvalue").unwrap();
//     let size_t_type = Compiler::size_t_type(context);
//     let void_ptr_type = Compiler::void_ptr_type(context);

//     let rvalue_param =
//         unsafe { gcc_jit_context_new_param(context, null_mut(), void_ptr_type, rvalue_param_name.as_ptr()) };

//     let func_ptr = unsafe {
//         gcc_jit_context_new_function(
//             context,
//             null_mut(),
//             gcc_jit_function_kind::GCC_JIT_FUNCTION_INTERNAL,
//             size_t_type,
//             func_name.as_ptr(),
//             1,
//             [rvalue_param].as_mut_ptr(),
//             0,
//         )
//     };
//     let block_name = CString::new("entry").unwrap();
//     let block = unsafe { gcc_jit_function_new_block(func_ptr, block_name.as_ptr()) };

//     // rvalue requires to be dereferenced before getting size
//     let mut rvalue = unsafe { gcc_jit_param_as_rvalue(rvalue_param) };
//     rvalue = unsafe { gcc_jit_lvalue_as_rvalue(gcc_jit_rvalue_dereference(rvalue, null_mut())) };

//     let rvalue_type = unsafe { gcc_jit_rvalue_get_type(rvalue) };
//     let rvalue_size = unsafe { gcc_jit_context_new_sizeof(context, rvalue_type) };

//     unsafe { gcc_jit_block_end_with_return(block, null_mut(), rvalue_size) };

//     return func_ptr;
// }