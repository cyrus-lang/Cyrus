use crate::CodeGenLLVM;
use crate::diag::*;
use ast::ast::VisType;
use inkwell::OptimizationLevel;
use inkwell::execution_engine::FunctionLookupError;
use inkwell::execution_engine::JitFunction;
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::llvm_sys::prelude::LLVMTypeRef;
use inkwell::module::Linkage;
use inkwell::passes::PassManager;
use inkwell::targets::FileType;
use inkwell::types::AsTypeRef;
use inkwell::types::FunctionType;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::ffi::c_void;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::exit;
use utils::fs::ensure_output_dir;
use utils::fs::get_output_file_path;
use utils::generate_random_hex::generate_random_hex;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildManifest {
    pub sources: HashMap<String, String>,
    pub objects: HashMap<String, String>,
}

impl Default for BuildManifest {
    fn default() -> Self {
        Self {
            sources: HashMap::new(),
            objects: HashMap::new(),
        }
    }
}

impl BuildManifest {
    fn save_file(&self) {
        File::open("build/manifest.json")
            .unwrap()
            .write(serde_json::to_string(&self).unwrap().as_bytes())
            .unwrap();
    }

    fn read_file(&self) -> Self {
        match serde_json::from_str::<BuildManifest>(&utils::fs::read_file("build/manifest.json".to_string()).0) {
            Ok(manifest) => manifest,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed to parse 'build/manifest.json': {}", err.to_string())),
                    location: None,
                });
                exit(1);
            }
        }
    }
}

type MainFunc = unsafe extern "C" fn() -> c_void;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn optimize(&self) {
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
        if let Some(mut main_func) = self.entry_point.clone() {
            main_func.name = format!("main_{}", generate_random_hex());
            if main_func.vis_type != VisType::Internal {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::NonInternalEntryPoint,
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: main_func.loc.line,
                        column: main_func.loc.column,
                        length: main_func.span.end,
                    }),
                });
                exit(1);
            }

            let main_func_ptr = self.build_func_def(main_func.clone());

            // wrap actual main_func as entry point
            let return_type = self.context.i32_type();
            let mut param_types: Vec<LLVMTypeRef> = Vec::new();
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
            self.builder.build_call(main_func_ptr, &[], "call_main").unwrap();
            self.builder
                .build_return(Some(&return_type.const_int(0, false)))
                .unwrap();
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
        let opt_level = self.build_optimization_level(self.opts.opt_level);
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
        let output_dir = Path::new(&output_path);
        ensure_output_dir(output_dir);
        let output_file = get_output_file_path(output_dir, Path::new(&self.file_path));

        if let Err(err) = self.module.print_to_file(output_file) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Failed to print llvm-ir into file:\n{}", err.to_string())),
                location: None,
            });
            exit(1);
        }
    }

    pub fn emit_asm(&mut self, output_path: String) {
        todo!();
        // if let Err(err) = self.target_machine.write_to_file(
        //     &self.module,
        //     inkwell::targets::FileType::Assembly,
        //     Path::new(&output_path),
        // ) {
        //     display_single_diag(Diag {
        //         level: DiagLevel::Error,
        //         kind: DiagKind::Custom(format!("Failed to print assembly into file:\n{}", err.to_string())),
        //         location: None,
        //     });
        //     exit(1);
        // }
    }

    pub fn generate_executable_file(&self, output_path: String) {
        // // Link object file into executable (using system linker)
        // let output_executable_path = Path::new("my_executable");
        // let linker_output = std::process::Command::new("cc")
        //     .arg("my_module.o")
        //     .arg("-o")
        //     .arg("my_executable")
        //     .output()
        //     .expect("failed to execute linker");

        // if !linker_output.status.success() {
        //     eprintln!("Linker error: {}", String::from_utf8_lossy(&linker_output.stderr));
        //     std::fs::remove_file("my_module.o").unwrap();
        //     return;
        // }
        todo!();
    }

    pub fn generate_object_file(&self, output_path: String) {
        let output_dir = Path::new(&output_path);
        ensure_output_dir(output_dir);
        let output_file = get_output_file_path(output_dir, Path::new(&self.file_path));

        if let Err(err) = self
            .target_machine
            .write_to_file(&self.module, FileType::Object, Path::new(&output_file))
        {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Failed to generate object file: {}", err.to_string())),
                location: None,
            });
            exit(1);
        }
    }

    pub fn generate_dynamic_library(&self, output_path: String) {
        let output_dir = Path::new(&output_path);
        ensure_output_dir(output_dir);
        let output_file = get_output_file_path(output_dir, Path::new(&self.file_path));

        todo!();
    }

    pub(crate) fn ensure_build_manifest(&mut self) {
        let file_path = "build/manifest.json";
        if !fs::exists(file_path).unwrap() {
            match File::create(file_path) {
                Ok(mut file) => {
                    file.write(serde_json::to_string(&BuildManifest::default()).unwrap().as_bytes())
                        .unwrap();
                }
                Err(err) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!("Failed to create 'build/manifest.json': {}", err.to_string())),
                        location: None,
                    });
                    exit(1);
                }
            }
        } else {
            self.build_manifest = self.build_manifest.read_file();
        }
    }

    pub(crate) fn ensure_build_directory(&self) {
        ensure_output_dir(Path::new("build/"));
        ensure_output_dir(Path::new("build/obj"));
        ensure_output_dir(Path::new("build/sources"));
    }

    pub(crate) fn hash_source_code(&mut self) -> String {
        let output_dir = "build/sources".to_string();
        let source_code = utils::fs::read_file(self.file_path.clone()).0;
        let checksum = crc32fast::hash(source_code.as_bytes()).to_string();
        let output_file = format!("{}/{}", output_dir, generate_random_hex());
        File::create_new(output_file.clone())
            .unwrap()
            .write(checksum.as_bytes())
            .unwrap();

        self.build_manifest.sources.insert(self.file_path.clone(), output_file.clone());
        output_file
    }

    pub(crate) fn source_code_changed(&self) -> bool {
        let current_hash = self.hash_source_code();
        // source: String, hash: String
        // !(source == hash)
        todo!();
    }
}
