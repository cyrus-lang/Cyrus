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
use rand::Rng;
use rand::distr::Alphanumeric;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::env;
use std::ffi::c_void;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
use std::process::exit;
use utils::fs::absolute_to_relative;
use utils::fs::ensure_output_dir;
use utils::fs::get_output_file_path;
use utils::generate_random_hex::generate_random_hex;

const BUILD_DIR_PATH: &str = "build/";
const SOURCES_DIR_PATH: &str = "build/sources";
const OBJ_DIR_PATH: &str = "build/obj";
const MANIFEST_FILE_PATH: &str = "build/manifest.json";
const OUTPUT_FILE_PATH: &str = "build/output";

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
        fs::remove_file(MANIFEST_FILE_PATH).unwrap();
        let mut file = File::create(MANIFEST_FILE_PATH).unwrap();
        file.write(serde_json::to_string(&self).unwrap().as_bytes()).unwrap();
    }

    fn read_file(&self) -> Self {
        match serde_json::from_str::<BuildManifest>(&utils::fs::read_file(MANIFEST_FILE_PATH.to_string()).0) {
            Ok(manifest) => manifest,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed to parse '{}': {}", MANIFEST_FILE_PATH, err.to_string())),
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
        let fpm = PassManager::create(self.module.borrow_mut().deref_mut());
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

            let main_func_ptr = self.build_func_def(main_func.clone());

            let entry_point =
                self.module
                    .borrow_mut()
                    .deref_mut()
                    .add_function("main", fn_type, Some(Linkage::External));
            let entry_block = self.context.append_basic_block(entry_point, "entry");
            self.builder.position_at_end(entry_block);
            self.builder.build_call(main_func_ptr, &[], "call_main").unwrap();
            self.builder
                .build_return(Some(&return_type.const_int(0, false)))
                .unwrap();
        } else if !self.is_entry_point {
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
        let execution_engine = self
            .module
            .borrow_mut()
            .deref_mut()
            .create_jit_execution_engine(opt_level)
            .unwrap();

        let module = self.module.borrow_mut();
        for module_metadata in self.loaded_modules.clone() {
            let loaded_module = module_metadata.module.borrow_mut().deref().clone();
            if let Err(err) = module.link_in_module(loaded_module) {
                eprintln!("In-Memory linkage error: {}", err.to_string_lossy());
                exit(1);
            }
        }

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

    pub fn emit_llvm_ir(&mut self, output_path: Option<String>) {
        if let Some(output_path) = output_path {
            if let Err(err) = self.module.borrow_mut().deref_mut().print_to_file(output_path) {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed to print llvm-ir into file:\n{}", err.to_string())),
                    location: None,
                });
                exit(1);
            }
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Output file path must be specified to generate llvm-ir..".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    pub fn emit_asm(&mut self, output_path: Option<String>) {
        if let Some(output_path) = output_path {
            if let Err(err) = self.target_machine.write_to_file(
                &self.module.borrow_mut().deref_mut(),
                FileType::Assembly,
                Path::new(&output_path),
            ) {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed to print assembly into file:\n{}", err.to_string())),
                    location: None,
                });
                exit(1);
            }
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Output file path must be specified to generate llvm-ir..".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    pub fn generate_executable_file(&self, output_path: Option<String>) {
        let object_files: Vec<String>;
        let output_path = {
            if let Some(path) = output_path {
                // generate object file path and save it in system temp
                let mut temp_path = env::temp_dir();
                temp_path.push(format!("{}.o", generate_random_hex()));
                let temp_path_str = temp_path.to_str().unwrap().to_string();
                self.generate_object_file_internal(temp_path_str.clone());
                object_files = vec![temp_path_str];
                path
            } else {
                if self.compiler_invoked_single {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(
                            "Output file path must be specified to generate executable.".to_string(),
                        ),
                        location: None,
                    });
                    exit(1);
                }

                object_files = self.build_manifest.objects.values().cloned().collect();

                ensure_output_dir(Path::new(OUTPUT_FILE_PATH));
                format!("{}/{}", OUTPUT_FILE_PATH, {
                    if let Some(file_name) = &self.opts.project_name {
                        file_name.clone()
                    } else {
                        let file_name = Path::new(&self.file_path)
                            .file_stem()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .replace(".", "_")
                            .replace("/", "")
                            .to_string();
                        file_name.clone()
                    }
                })
            }
        };

        // TODO Consider to make linker dynamic through Project.toml and CLI Program.
        let linker = "cc";

        let mut linker_command = std::process::Command::new(linker);

        for path in object_files {
            linker_command.arg(path);
        }

        let linker_output = linker_command.arg("-o").arg(output_path).output();

        match linker_output {
            Ok(output) => {
                if !output.status.success() {
                    eprintln!("Linker error: {}", String::from_utf8_lossy(&output.stderr));
                    exit(1);
                    return;
                }
            }
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed execute linker ({}):\n{}", linker, err.to_string())),
                    location: None,
                });
                exit(1);
            }
        }
    }

    pub fn generate_dynamic_library(&self, output_path: Option<String>) {
        let object_files: String;
        let output_path = {
            if let Some(path) = output_path {
                // generate object file path and save it in system temp
                let mut temp_path = env::temp_dir();
                temp_path.push(format!("{}.o", generate_random_hex()));
                let temp_path_str = temp_path.to_str().unwrap().to_string();
                self.generate_object_file_internal(temp_path_str.clone());
                object_files = temp_path_str;
                path
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(
                        "Output file path must be specified to generate dynamic library.".to_string(),
                    ),
                    location: None,
                });
                exit(1);
            }
        };

        // TODO Consider to make linker dynamic through Project.toml and CLI Program.
        let linker = "cc";

        let linker_output = std::process::Command::new(linker)
            .arg(object_files)
            .arg("-fPIC")
            .arg("-o")
            .arg(output_path)
            .output();

        match linker_output {
            Ok(output) => {
                if !output.status.success() {
                    eprintln!("Linker error: {}", String::from_utf8_lossy(&output.stderr));
                    return;
                }
            }
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed execute linker ({}):\n{}", linker, err.to_string())),
                    location: None,
                });
                exit(1);
            }
        }
    }

    pub fn generate_object_file(&self, output_path: Option<String>) {
        if let Some(output_path) = output_path {
            let output_dir = Path::new(&output_path);
            ensure_output_dir(output_dir);
            let output_file = get_output_file_path(output_dir, Path::new(&self.file_path));
            self.generate_object_file_internal(output_file.to_str().unwrap().to_string());
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Output directory path must be specified to generate object files.".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn generate_object_file_internal(&self, output_path: String) {
        if let Err(err) = self.target_machine.write_to_file(
            &self.module.borrow_mut().deref_mut(),
            FileType::Object,
            Path::new(&output_path),
        ) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Failed to generate object file: {}", err.to_string())),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn ensure_build_manifest(&mut self) {
        if !fs::exists(MANIFEST_FILE_PATH).unwrap() {
            match File::create(MANIFEST_FILE_PATH) {
                Ok(mut file) => {
                    file.write(serde_json::to_string(&BuildManifest::default()).unwrap().as_bytes())
                        .unwrap();
                }
                Err(err) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Failed to create '{}': {}",
                            MANIFEST_FILE_PATH,
                            err.to_string()
                        )),
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
        ensure_output_dir(Path::new(BUILD_DIR_PATH));
        ensure_output_dir(Path::new(OBJ_DIR_PATH));
        ensure_output_dir(Path::new(SOURCES_DIR_PATH));
    }

    pub(crate) fn hash_source_code(&mut self) -> String {
        let source_code = utils::fs::read_file(self.file_path.clone()).0;
        crc32fast::hash(source_code.as_bytes()).to_string()
    }

    pub(crate) fn object_file_exists(&mut self) -> bool {
        let wd = std::env::current_dir().unwrap().to_str().unwrap().to_string();
        let file_path = absolute_to_relative(self.file_path.clone(), wd).unwrap();
        self.build_manifest.objects.get(&file_path.clone()).is_some()
    }

    pub(crate) fn source_code_changed(&mut self) -> bool {
        let output_dir = SOURCES_DIR_PATH.to_string();
        let current_hash = self.hash_source_code();
        let wd = std::env::current_dir().unwrap().to_str().unwrap().to_string();
        let file_path = absolute_to_relative(self.file_path.clone(), wd).unwrap();
        if let Some(hash_file_path) = self.build_manifest.read_file().sources.get(&file_path.clone()) {
            match File::open(hash_file_path.clone()) {
                Ok(mut file) => {
                    let mut saved_hash = String::new();
                    file.read_to_string(&mut saved_hash).unwrap();
                    !(current_hash == *saved_hash)
                }
                Err(_) => {
                    // saved hash does not exist in sources directory
                    // let's remove it from BuildManifest and create a new one
                    self.build_manifest.sources.remove(&file_path.clone());
                    let output_file = self.save_source_hash(output_dir, current_hash);
                    self.build_manifest.sources.insert(file_path.clone(), output_file);
                    self.build_manifest.save_file();
                    true
                }
            }
        } else {
            let output_file = self.save_source_hash(output_dir, current_hash);
            self.build_manifest.sources.insert(file_path.clone(), output_file);
            self.build_manifest.save_file();
            true
        }
    }

    pub(crate) fn save_source_hash(&self, output_dir: String, hash_str: String) -> String {
        let rng = rand::rng();
        let random_hex: String = rng.sample_iter(&Alphanumeric).take(30).map(char::from).collect();
        let output_file = format!("{}/{}", output_dir, random_hex);
        File::create_new(output_file.clone())
            .unwrap()
            .write(hash_str.as_bytes())
            .unwrap();
        output_file
    }

    pub(crate) fn save_object_file(&mut self) {
        let rng = rand::rng();
        let random_hex: String = rng.sample_iter(&Alphanumeric).take(30).map(char::from).collect();
        let output_file = format!("{}/{}.o", OBJ_DIR_PATH, random_hex);

        let wd = std::env::current_dir().unwrap().to_str().unwrap().to_string();
        let file_path = absolute_to_relative(self.file_path.clone(), wd).unwrap();

        // remove previous object_file
        if let Some(obj_file) = self.build_manifest.objects.get(&file_path.clone()) {
            if fs::exists(obj_file).unwrap() {
                let _ = fs::remove_file(obj_file).unwrap();
            }
        }

        // generate new object_file
        self.generate_object_file_internal(output_file.clone());
        self.build_manifest.objects.insert(file_path.clone(), output_file);
        self.build_manifest.save_file();
    }
}
