use crate::CodeGenLLVM;
use crate::diag::*;
use std::path::Path;
use std::process::exit;

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

    pub fn emit_llvm_ir(&mut self, output_path: String) {
        if let Err(err) = self.module.print_to_file(output_path) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Failed to print llvm-ir into file:\n{}", err.to_string())),
                location: None,
            });
            self.reporter.display_diags();
            exit(1);
        }
    }

    pub fn emit_asm(&mut self, output_path: String) {
        if let Err(err) = self.target_machine.write_to_file(
            &self.module,
            inkwell::targets::FileType::Assembly,
            Path::new(&output_path),
        ) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Failed to print assembly into file:\n{}", err.to_string())),
                location: None,
            });
            self.reporter.display_diags();
            exit(1);
        }
    }
}
