use std::process::exit;

use crate::diag::*;
use crate::CodeGenLLVM;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn execute(&mut self) {
        if let Some(main_func) = self.module.get_function("main") {
            unsafe { self.execution_engine.run_function_as_main(main_func, &[]) };
        } else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::NoEntryPointDetected,
                location: None,
            });
            self.reporter.display_diags();
            exit(1);
        }
    }
}
