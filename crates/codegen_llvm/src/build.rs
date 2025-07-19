use crate::CodeGenLLVM;
use crate::diag::*;
use crate::opts::BuildDir;
use crate::opts::CodeModelOptions;
use crate::opts::RelocModeOptions;
use crate::scope::Scope;
use crate::scope::ScopeRef;
use inkwell::module::FlagBehavior;
use inkwell::passes::PassBuilderOptions;
use inkwell::passes::PassManager;
use inkwell::targets::FileType;
use inkwell::targets::RelocMode;
use rand::Rng;
use rand::distr::Alphanumeric;
use serde::Deserialize;
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::ops::DerefMut;
use std::path::Path;
use std::process::Command;
use std::process::Stdio;
use std::process::exit;
use std::rc::Rc;
use utils::fs::dylib_extension;
use utils::fs::ensure_output_dir;
use utils::fs::executable_extension;
use utils::generate_random_hex::generate_random_hex;

const SOURCES_DIR_PATH: &str = "sources";
const OBJECTS_FILENAME: &str = "obj";
const MANIFEST_FILENAME: &str = "manifest.json";
const OUTPUT_FILENAME: &str = "output";

#[derive(Debug, Clone)]
pub enum OutputKind {
    None,
    LlvmIr(Option<String>),
    Asm(Option<String>),
    ObjectFile(Option<String>),
    Dylib(Option<String>),
}

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
    fn save_file(&self, build_dir: String) {
        let manifest_filepath = format!("{}/{}", build_dir, MANIFEST_FILENAME);

        fs::remove_file(manifest_filepath.clone()).unwrap();
        let mut file = File::create(manifest_filepath).unwrap();
        file.write(serde_json::to_string(&self).unwrap().as_bytes()).unwrap();
    }

    fn read_file(&self, build_dir: String) -> Self {
        let manifest_filepath = format!("{}/{}", build_dir, MANIFEST_FILENAME);

        match serde_json::from_str::<BuildManifest>(&utils::fs::read_file(manifest_filepath.to_string()).0) {
            Ok(manifest) => manifest,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed to parse '{}': {}", manifest_filepath, err.to_string())),
                    location: None,
                });
                exit(1);
            }
        }
    }
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn enable_module_flags(&mut self) {
        let module_ref = self.module.borrow_mut();

        if matches!(
            self.opts.reloc_mode,
            RelocModeOptions::PIC | RelocModeOptions::DynamicNoPic
        ) {
            let pic_level = self.context.i32_type().const_int(2, false);
            module_ref.add_basic_value_flag("PIC Level", FlagBehavior::Error, pic_level);

            let pie_level = self.context.i32_type().const_int(2, false);
            module_ref.add_basic_value_flag("PIE Level", FlagBehavior::Error, pie_level);
        }

        if self.opts.code_model != CodeModelOptions::Kernel {
            // Unwind tables (uwtable) are not supported in 'kernel' code model.
            // Kernel code typically disables exception handling and stack unwinding.
            // Enabling uwtable may cause linker errors or generate invalid metadata.
            let uwtable_value = self.context.i32_type().const_int(2, false);
            module_ref.add_basic_value_flag("uwtable", FlagBehavior::Error, uwtable_value);
        }
    }

    pub(crate) fn execute_linker(&self, output_path: String, object_files: Vec<String>, extra_args: Vec<String>) {
        let linker = "clang";

        let mut linker_command = std::process::Command::new(linker);
        // FIXME Remove
        // linker_command.arg("-fPIE");
        linker_command.arg("-o").arg(output_path);

        object_files.iter().for_each(|path| {
            linker_command.arg(path);
        });

        extra_args.iter().for_each(|path| {
            linker_command.arg(path);
        });

        match linker_command.output() {
            Ok(output) => {
                if !output.status.success() {
                    eprintln!("Linker error: {}", String::from_utf8_lossy(&output.stderr));
                    exit(1);
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

        if let BuildDir::Default = self.opts.build_dir {
            fs::remove_dir_all(format!("{}/{}", self.final_build_dir.clone(), OBJECTS_FILENAME)).unwrap();
            fs::remove_dir_all(format!("{}/{}", self.final_build_dir.clone(), SOURCES_DIR_PATH)).unwrap();
            fs::remove_file(format!("{}/{}", self.final_build_dir.clone(), MANIFEST_FILENAME)).unwrap();
        }
    }

    pub(crate) fn generate_output_file_name(&self) -> String {
        let mut file_name = Path::new(&self.file_path.clone())
            .with_extension("")
            .to_str()
            .unwrap()
            .to_string();
        let wd = env::current_dir().unwrap();
        file_name = file_name.replace(wd.to_str().unwrap(), "");
        file_name = file_name
            .trim_start_matches('/')
            .replace('/', "_")
            .replace('-', "_")
            .replace('.', "")
            .to_string();
        file_name
    }

    pub(crate) fn generate_output(&mut self) {
        match &self.output_kind {
            OutputKind::LlvmIr(output_path) => self.emit_llvm_ir(output_path.clone()),
            OutputKind::Asm(output_path) => self.emit_asm(output_path.clone()),
            OutputKind::ObjectFile(output_path) => self.generate_object_file(output_path.clone()),
            OutputKind::Dylib(output_path) => self.generate_dynamic_library(output_path.clone()),
            OutputKind::None => {}
        }
    }

    fn emit_llvm_ir(&mut self, output_path: Option<String>) {
        if let Some(output_path) = output_path {
            ensure_output_dir(Path::new(&output_path.clone()));
            let file_path = format!("{}/{}.ll", output_path, self.generate_output_file_name());

            if let Err(err) = self.module.borrow_mut().deref_mut().print_to_file(file_path) {
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
                kind: DiagKind::Custom("Output directory must be specified to generate llvm-ir.".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    fn emit_asm(&mut self, output_path: Option<String>) {
        if let Some(output_path) = output_path {
            ensure_output_dir(Path::new(&output_path.clone()));
            let file_path = format!("{}/{}.asm", output_path, self.generate_output_file_name());

            if let Err(err) = self.target_machine.write_to_file(
                &self.module.borrow_mut().deref_mut(),
                FileType::Assembly,
                Path::new(&file_path),
            ) {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed to write assembly into file:\n{}", err.to_string())),
                    location: None,
                });
                exit(1);
            }
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Output directory must be specified to generate assembly.".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    fn get_sanitized_project_name(&self, file_path: &str) -> String {
        let project_name = if let Some(file_name) = &self.opts.project_name {
            file_name.clone()
        } else {
            file_path.to_string()
        };

        Path::new(&project_name)
            .file_stem()
            .unwrap_or_else(|| {
                eprintln!("Warning: Could not determine file stem for project name. Using a default.");
                std::ffi::OsStr::new("default")
            })
            .to_str()
            .unwrap_or_else(|| {
                eprintln!("Warning: Project name contains invalid Unicode. Using a default.");
                "default"
            })
            .replace('.', "_")
            .replace('-', "_")
            .replace('/', "")
            .to_string()
    }

    pub fn generate_executable_file(&self, output_path: Option<String>) {
        let object_files: Vec<String> = self.build_manifest.objects.values().cloned().collect();

        let output_path = {
            if let Some(path) = output_path {
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

                let executable_output_dir = format!("{}/{}", self.final_build_dir.clone(), OUTPUT_FILENAME);
                ensure_output_dir(Path::new(&executable_output_dir));
                let sanitized_project_name = self.get_sanitized_project_name(&self.file_path);
                format!("{}/{}", executable_output_dir, sanitized_project_name)
            }
        };

        self.execute_linker(output_path, object_files, Vec::new());
    }

    fn generate_dynamic_library(&self, output_path: Option<String>) {
        let object_files: Vec<String> = self.build_manifest.objects.values().cloned().collect();

        let output_path = {
            if let Some(path) = output_path {
                path
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(
                        "Output directory must be specified to generate dynamic library.".to_string(),
                    ),
                    location: None,
                });
                exit(1);
            }
        };

        ensure_output_dir(Path::new(&output_path.clone()));
        let output_path = format!(
            "{}/{}.{}",
            output_path,
            self.generate_output_file_name(),
            dylib_extension()
        );

        self.execute_linker(output_path, object_files, vec!["-fPIC".to_string()]);
    }

    fn generate_object_file(&self, output_path: Option<String>) {
        if let Some(output_path) = output_path {
            ensure_output_dir(Path::new(&output_path.clone()));
            let file_path = format!("{}/{}.o", output_path, self.generate_output_file_name());
            self.generate_object_file_internal(file_path);
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Output directory path must be specified to generate object files.".to_string()),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn run_passes(&self) {
        let mut module = self.module.borrow_mut();
        let fpm = PassManager::create(module.deref_mut());
        self.target_machine.add_analysis_passes(&fpm);
        fpm.initialize();

        let passes: &[&str] = &["instcombine", "reassociate", "gvn", "simplifycfg", "mem2reg"];

        module
            .run_passes(
                passes.join(",").as_str(),
                &self.target_machine,
                PassBuilderOptions::create(),
            )
            .unwrap();

        fpm.finalize();
    }

    pub(crate) fn build_entry_point(&mut self) {
        if let Some(main_func) = self.entry_point.clone() {
            let params_metadata = self.build_func_params(
                main_func.name.clone(),
                main_func.loc.clone(),
                main_func.span.end,
                main_func.params.list.clone(),
                None,
                false,
            );

            let scope: ScopeRef<'ctx> = Rc::new(RefCell::new(Scope::new()));
            self.build_func_def(scope, main_func, params_metadata, true);
        } else if self.is_current_module_entry_point() && self.entry_point.is_none() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::NoEntryPointDetected,
                location: None,
            });
            exit(1);
        }
    }

    pub fn execute(&mut self) {
        let temp_file_path = format!(
            "{}/{}{}",
            env::temp_dir().to_str().unwrap(),
            generate_random_hex(),
            executable_extension()
        );
        self.generate_executable_file(Some(temp_file_path.clone()));
        std::process::Command::new(temp_file_path.clone())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .unwrap_or_else(|e| {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed to execute process: {}", e.to_string())),
                    location: None,
                });
                exit(1);
            });
    }

    pub(crate) fn apply_llc_flags(&self) -> Vec<String> {
        let mut flags: Vec<String> = Vec::new();
        if let Some(reloc_mode_str) = self.opts.reloc_mode.to_linker_reloc_mode() {
            flags.push(reloc_mode_str);
        }
        if let Some(target_triple_str) = &self.opts.target_triple {
            flags.push(format!("--mtriple={}", target_triple_str));
        }
        if let Some(cpu_str) = &self.opts.cpu {
            flags.push(format!("--mcpu={}", cpu_str));
        }
        if let Some(code_model_str) = self.opts.code_model.to_linker_code_model() {
            flags.push(code_model_str);
        }
        flags
    }

    pub(crate) fn generate_object_file_internal(&self, output_path: String) {
        let temp_dir = env::temp_dir();
        let temp_ll_file_path = temp_dir.join("module.ll");
        let temp_bc_file_path = temp_dir.join("module.bc");

        if let Some(parent) = temp_ll_file_path.parent() {
            if let Err(e) = fs::create_dir_all(parent) {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed to create temporary directory: {}", e)),
                    location: None,
                });
                exit(1);
            }
        }

        let result: Result<(), String> = (|| {
            self.module
                .borrow()
                .print_to_file(&temp_ll_file_path)
                .map_err(|err| format!("Failed to print LLVM IR to temporary file: {}", err))?;

            let mut llvm_as_command = Command::new("llvm-as");
            llvm_as_command.arg(&temp_ll_file_path);
            llvm_as_command.arg("-o");
            llvm_as_command.arg(&temp_bc_file_path);

            let llvm_as_command_output = llvm_as_command
                .output()
                .map_err(|e| format!("Failed to execute llvm-as command: {}", e))?;

            if !llvm_as_command_output.status.success() {
                return Err(format!(
                    "llvm-as command failed with exit code {}:\nSTDOUT:\n{}\nSTDERR:\n{}",
                    llvm_as_command_output.status.code().unwrap_or(-1),
                    String::from_utf8_lossy(&llvm_as_command_output.stdout),
                    String::from_utf8_lossy(&llvm_as_command_output.stderr)
                ));
            }

            let mut llc_command = Command::new("llc");
            self.apply_llc_flags().iter().for_each(|flag| {
                llc_command.arg(flag.clone());
            });
            llc_command.arg("-filetype=obj");
            llc_command.arg(&temp_bc_file_path);
            llc_command.arg("-o");
            llc_command.arg(&output_path);

            let llc_command_output = llc_command
                .output()
                .map_err(|e| format!("Failed to execute llc command: {}", e))?;

            if !llc_command_output.status.success() {
                return Err(format!(
                    "llc command failed with exit code {}:\nSTDOUT:\n{}\nSTDERR:\n{}",
                    llc_command_output.status.code().unwrap_or(-1),
                    String::from_utf8_lossy(&llc_command_output.stdout),
                    String::from_utf8_lossy(&llc_command_output.stderr)
                ));
            }

            Ok(())
        })();

        if let Err(err) = result {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(err),
                location: None,
            });
            exit(1);
        }

        // clean up temporary files: the .ll and the .bc file
        for path in [&temp_ll_file_path, &temp_bc_file_path] {
            if let Err(e) = fs::remove_file(path) {
                eprintln!("Warning: Failed to remove temporary file {:?}: {}", path, e);
            }
        }
    }

    pub(crate) fn ensure_build_manifest(&mut self, build_dir: String) {
        if !fs::exists(format!("{}/{}", build_dir, MANIFEST_FILENAME)).unwrap() {
            match File::create(format!("{}/{}", build_dir, MANIFEST_FILENAME)) {
                Ok(mut file) => {
                    file.write(serde_json::to_string(&BuildManifest::default()).unwrap().as_bytes())
                        .unwrap();
                }
                Err(err) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Failed to create '{}': {}",
                            format!("{}/{}", build_dir, MANIFEST_FILENAME),
                            err.to_string()
                        )),
                        location: None,
                    });
                    exit(1);
                }
            }
        } else {
            self.build_manifest = self.build_manifest.read_file(build_dir);
        }
    }

    pub(crate) fn ensure_build_directory(&self, build_dir: String) {
        ensure_output_dir(Path::new(&build_dir.clone()));
        ensure_output_dir(Path::new(&format!("{}/{}", build_dir, OBJECTS_FILENAME)));
        ensure_output_dir(Path::new(&format!("{}/{}", build_dir, SOURCES_DIR_PATH)));
    }

    pub(crate) fn hash_source_code(&mut self) -> String {
        let source_code = utils::fs::read_file(self.file_path.clone()).0;
        crc32fast::hash(source_code.as_bytes()).to_string()
    }

    pub(crate) fn object_file_exists(&mut self) -> bool {
        self.build_manifest.objects.get(&self.file_path.clone()).is_some()
    }

    pub(crate) fn source_code_changed(&mut self, build_dir: String) -> bool {
        let output_dir = format!("{}/{}", self.final_build_dir.clone(), SOURCES_DIR_PATH);
        let current_hash = self.hash_source_code();

        if let Some(hash_file_path) = self
            .build_manifest
            .read_file(build_dir.clone())
            .sources
            .get(&self.file_path.clone())
        {
            match File::open(hash_file_path.clone()) {
                Ok(mut file) => {
                    let mut saved_hash = String::new();
                    file.read_to_string(&mut saved_hash).unwrap();
                    !(current_hash == *saved_hash)
                }
                Err(_) => {
                    // saved hash does not exist in sources directory
                    // let's remove it from BuildManifest and create a new one
                    self.build_manifest.sources.remove(&self.file_path.clone());
                    let output_file = self.save_source_hash(output_dir, current_hash);
                    self.build_manifest.sources.insert(self.file_path.clone(), output_file);
                    self.build_manifest.save_file(build_dir);
                    true
                }
            }
        } else {
            let output_file = self.save_source_hash(output_dir, current_hash);
            self.build_manifest.sources.insert(self.file_path.clone(), output_file);
            self.build_manifest.save_file(build_dir);
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

    pub(crate) fn save_object_file(&mut self, build_dir: String) {
        let rng = rand::rng();
        let random_hex: String = rng.sample_iter(&Alphanumeric).take(30).map(char::from).collect();
        let output_file = format!("{}/{}/{}.o", build_dir, OBJECTS_FILENAME, random_hex);

        // remove previous object file if exists
        if let Some(obj_file) = self.build_manifest.objects.get(&self.file_path.clone()) {
            if fs::exists(obj_file).unwrap() {
                let _ = fs::remove_file(obj_file).unwrap();
            }
        }

        // generate object file
        self.generate_object_file_internal(output_file.clone());
        self.build_manifest.objects.insert(self.file_path.clone(), output_file);
        self.build_manifest.save_file(build_dir);
    }
}
