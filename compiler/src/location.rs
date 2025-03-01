use std::ffi::CString;

use ast::token::Location;
use gccjit_sys::{gcc_jit_context_new_location, gcc_jit_location};

use crate::Compiler;

impl Compiler {
    pub fn gccjit_location(&self, loc: Location) -> *mut gcc_jit_location {
        let file_name = CString::new(self.file_path.clone()).unwrap();
        unsafe { gcc_jit_context_new_location(self.context, file_name.as_ptr(), loc.line as i32, loc.column as i32) }
    }
}
