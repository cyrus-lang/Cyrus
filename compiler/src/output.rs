use crate::{Compiler, scope::Scope};
use ast::token::{Location, TokenKind};
use gccjit_sys::*;
use std::{
    cell::RefCell,
    ffi::CString,
    fs::{self, File},
    process::exit,
    ptr::null_mut,
    rc::Rc,
};
use utils::compile_time_errors::errors::*;
use utils::compiler_error;
use utils::generate_random_hex::generate_random_hex;

impl Compiler {
    pub fn compile_main_func(&mut self) {
        // The main function is compiled after all other function definitions
        // to ensure that all functions are available for use within main.
        if let Some(mut func_def) = self.main_func_ref.clone() {
            let loc = self.gccjit_location(func_def.loc.clone());
            let scope = Rc::new(RefCell::new(Scope::new()));

            // Generate a unique random hexadecimal name for the function definition
            // to avoid naming conflicts and ensure uniqueness in the generated code.
            func_def.name = format!("entry_point__{}", generate_random_hex());

            let mut void_return_type = false;
            if let Some(return_type_token) = func_def.return_type.clone() {
                if !(return_type_token.kind == TokenKind::Void || return_type_token.kind == TokenKind::I32) {
                    let mut err_msg = String::from("Invalid return type for the 'main' function.\n");
                    err_msg.push_str("Only `void` or `i32` are valid return types for the 'main' function.\n");
                    err_msg.push_str(&format!("Found return type: '{}'.", return_type_token.kind));
                    compiler_error!(err_msg, self.file_path.clone())
                }

                if return_type_token.kind == TokenKind::Void {
                    void_return_type = true;
                }
            } else {
                void_return_type = true;
            }

            let func_ptr = self.compile_func_def(scope, func_def.clone());

            // Create definite main func and call func_ptr
            let main_func_return_type = Compiler::i32_type(self.context);
            let main_func_name = CString::new("main").unwrap();
            let main_func_ptr = unsafe {
                gcc_jit_context_new_function(
                    self.context,
                    loc,
                    gcc_jit_function_kind::GCC_JIT_FUNCTION_EXPORTED,
                    main_func_return_type,
                    main_func_name.as_ptr(),
                    0,
                    null_mut(),
                    0,
                )
            };
            let main_func_block_name = CString::new("entry").unwrap();
            let main_func_block = unsafe { gcc_jit_function_new_block(main_func_ptr, main_func_block_name.as_ptr()) };

            let exec_result = unsafe { gcc_jit_context_new_call(self.context, loc, func_ptr, 0, null_mut()) };
            if void_return_type {
                if !self.block_is_terminated(main_func_block) {
                    unsafe { gcc_jit_block_add_eval(main_func_block, null_mut(), exec_result) };
                    let return_value =
                        unsafe { gcc_jit_context_new_rvalue_from_int(self.context, main_func_return_type, 0) };
                    unsafe { gcc_jit_block_end_with_return(main_func_block, loc, return_value) };
                }
            } else {
                unsafe { gcc_jit_block_end_with_return(main_func_block, loc, exec_result) };
            }
            self.mark_block_terminated(main_func_block);
        } else {
            let mut err_msg = String::from("No entry point detected.\n");
            err_msg.push_str("Consider to add an execution entry point: \n\n");
            err_msg.push_str("fn main() {\n");
            err_msg.push_str("   ...\n");
            err_msg.push_str("}");
            compiler_error!(err_msg, self.file_path.clone());
        }
    }

    pub fn execute(&mut self) {
        self.compile_main_func();
        let result = unsafe { gcc_jit_context_compile(self.context) };

        if result.is_null() {
            exit(1);
        }

        let name = CString::new("main").unwrap();
        let main = unsafe { gcc_jit_result_get_code(result, name.as_ptr()) };
        if main.is_null() {
            compiler_error!(
                "Undefined behaviour when retrieving main func of the content.",
                self.file_path.clone()
            );
        }

        unsafe {
            let main_fn: extern "C" fn() = std::mem::transmute(main);
            main_fn();
        }

        compiled_successfully();

        unsafe { gcc_jit_result_release(result) };
    }

    pub fn make_executable_file(&mut self, file_path: String) {
        self.compile_main_func();
        unsafe {
            let file_path = CString::new(file_path).unwrap();
            gcc_jit_context_compile_to_file(
                self.context,
                gccjit_sys::gcc_jit_output_kind::GCC_JIT_OUTPUT_KIND_EXECUTABLE,
                file_path.as_ptr(),
            )
        };
    }

    pub fn make_object_file(&mut self, file_path: String) {
        unsafe {
            let file_path = CString::new(file_path).unwrap();
            gcc_jit_context_compile_to_file(
                self.context,
                gccjit_sys::gcc_jit_output_kind::GCC_JIT_OUTPUT_KIND_OBJECT_FILE,
                file_path.as_ptr(),
            )
        };
    }

    pub fn make_dynamic_library(&mut self, file_path: String) {
        unsafe {
            let file_path = CString::new(file_path).unwrap();
            gcc_jit_context_compile_to_file(
                self.context,
                gccjit_sys::gcc_jit_output_kind::GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY,
                file_path.as_ptr(),
            )
        };
    }

    pub fn create_dump_file(&self, file_path: String) {
        if !fs::exists(file_path.clone()).unwrap() {
            if let Err(err) = File::create(file_path.clone()) {
                compiler_error!(
                    format!("Failed to dump source code at '{}': {}", file_path, err),
                    self.file_path.clone()
                )
            }
        }
    }

    pub fn make_dump_ir(&mut self, file_path: String) {
        self.compile_main_func();
        self.create_dump_file(file_path.clone());
        unsafe {
            let file_path = CString::new(file_path).unwrap();
            gcc_jit_context_dump_to_file(self.context, file_path.as_ptr(), self.cbool(true))
        };
    }

    pub fn make_dump_asm(&mut self, file_path: String) {
        self.compile_main_func();
        self.create_dump_file(file_path.clone());
        unsafe {
            let file_path = CString::new(file_path).unwrap();
            gcc_jit_context_compile_to_file(
                self.context,
                gcc_jit_output_kind::GCC_JIT_OUTPUT_KIND_ASSEMBLER,
                file_path.as_ptr(),
            )
        };
    }

    pub fn set_debug_info(&self, is_debug_mode: bool) {
        unsafe {
            gcc_jit_context_set_bool_option(
                self.context,
                gccjit_sys::gcc_jit_bool_option::GCC_JIT_BOOL_OPTION_DEBUGINFO,
                self.cbool(is_debug_mode),
            )
        };
    }
}
