use crate::CodeGenLLVM;
use crate::diag::*;
use ast::ast::FuncDef;
use inkwell::OptimizationLevel;
use inkwell::execution_engine::FunctionLookupError;
use inkwell::execution_engine::JitFunction;
use inkwell::llvm_sys::LLVMType;
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::module::Linkage;
use inkwell::passes::PassManager;
use inkwell::types::AsTypeRef;
use inkwell::types::FunctionType;
use std::ffi::c_void;
use std::path::Path;
use std::process::exit;

type MainFunc = unsafe extern "C" fn() -> c_void;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn optimize(&mut self) {
        let fpm = PassManager::create(&self.module);
        self.target_machine.add_analysis_passes(&fpm);
        fpm.initialize();
        fpm.finalize();
    }

    pub(crate) fn build_optimization_level(&mut self, opt_level: i32) -> OptimizationLevel {
        match opt_level {
            0 => OptimizationLevel::None,
            1 => OptimizationLevel::Less,
            2 => OptimizationLevel::Default,
            3 => OptimizationLevel::Aggressive,
            _ => panic!(),
        }
    }

    pub(crate) fn build_entry_point(&mut self) {
        if let Some(main_func) = self.entry_point {
            // wrap actual main_func as entry point
            let return_type = self.context.i32_type();
            let mut param_types: Vec<*mut LLVMType> = Vec::new();
            let fn_type = unsafe {
                FunctionType::new(LLVMFunctionType(
                    return_type.as_type_ref(),
                    param_types.as_mut_ptr(),
                    param_types.len() as u32,
                    0,
                ))
            };

            let entry_point = self.module.add_function("main", fn_type, Some(Linkage::External));
            let entry_block = self.context.append_basic_block(entry_point, "entry");
            self.builder.position_at_end(entry_block);
            self.builder.build_call(main_func, &[], "call_main").unwrap();
            self.builder.build_return(Some(&return_type.const_int(0, false))).unwrap();
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::NoEntryPointDetected,
                location: None,
            });
            exit(1);
        }
    }

    pub fn execute(&mut self) {
        let opt_level = self.build_optimization_level(self.opts.optimization_level);
        let execution_engine = self.module.create_jit_execution_engine(opt_level).unwrap();

        let main_func_result: Result<JitFunction<'ctx, MainFunc>, FunctionLookupError> =
            unsafe { execution_engine.get_function("main") };

        match main_func_result {
            Ok(main_func) => {
                unsafe { main_func.as_raw()() };
            }
            Err(err) => match err {
                FunctionLookupError::JITNotEnabled => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("JIT not enabled.")),
                        location: None,
                    });
                    exit(1);
                }
                FunctionLookupError::FunctionNotFound => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::NoEntryPointDetected,
                        location: None,
                    });
                    exit(1);
                }
            },
        }
    }

    pub fn emit_llvm_ir(&mut self, output_path: String) {
        if let Err(err) = self.module.print_to_file(output_path) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Failed to print llvm-ir into file:\n{}", err.to_string())),
                location: None,
            });
            exit(1);
        }
    }

    pub fn emit_asm(&mut self, output_path: String) {
        if let Err(err) = self.target_machine.write_to_file(
            &self.module,
            inkwell::targets::FileType::Assembly,
            Path::new(&output_path),
        ) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Failed to print assembly into file:\n{}", err.to_string())),
                location: None,
            });
            exit(1);
        }
    }
}
