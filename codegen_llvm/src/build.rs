use utils::compiler_error;
use utils::compile_time_errors::errors::*;

use crate::CodeGenLLVM;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn execute(&mut self) {
        if let Some(main_func) = self.module.get_function("main") {
            unsafe { self.execution_engine.run_function_as_main(main_func, &[]) };
        } else {
            // FIXME
            compiler_error!(
                format!("No entry point detected. Consider to add such a function into your module:\nfn main() {{\n   ...\n}}"),
                self.file_path.clone()
            );
        }
    }
}
