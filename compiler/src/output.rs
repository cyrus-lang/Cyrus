use std::ffi::CString;
use gccjit_sys::*;
use utils::compiler_error;
use crate::Compiler;

impl Compiler {
    pub fn execute(&self) {
        let result = unsafe { gcc_jit_context_compile(self.context) };

        // let name = CString::new("main").unwrap();
        // let main = unsafe { gcc_jit_result_get_code(result, name.as_ptr()) };
        // if main.is_null() {
        //     compiler_error!("A 'main' function required as the entry point.");
        // }

        // unsafe {
        //     let main_fn: extern "C" fn() = std::mem::transmute(main);
        //     main_fn();
        // }
    }

    pub fn make_executable_file(&self, file_path: String) {
        unsafe {
            let file_path = CString::new(file_path).unwrap();
            gcc_jit_context_compile_to_file(
                self.context,
                gccjit_sys::gcc_jit_output_kind::GCC_JIT_OUTPUT_KIND_EXECUTABLE,
                file_path.as_ptr(),
            )
        };
    }

    pub fn make_object_file(&self, file_path: String) {
        unsafe {
            let file_path = CString::new(file_path).unwrap();
            gcc_jit_context_compile_to_file(
                self.context,
                gccjit_sys::gcc_jit_output_kind::GCC_JIT_OUTPUT_KIND_OBJECT_FILE,
                file_path.as_ptr(),
            )
        };
    }

    pub fn make_dump_file(&self, file_path: String) {
        unsafe {
            let file_path = CString::new(file_path).unwrap();
            gcc_jit_context_dump_to_file(self.context, file_path.as_ptr(), self.cbool(true))
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
