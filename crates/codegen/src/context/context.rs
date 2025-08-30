use super::{build_manifest::BuildManifest, object_file_info::ObjectFileInfo};
use crate::{
    builder::module::{CodeGenModule, CodeGenModuleOutput},
    options::{CodeGenOptions, OutputKind, RelocModeOptions},
};
use diagcentral::display_single_custom_diag;
use inkwell::{
    OptimizationLevel,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
};
use project_layout::OBJECTS_FILENAME;
use resolver::{Resolver, moduleloader::ModuleFilePath};
use std::{
    cell::RefCell,
    path::{Path, PathBuf},
    process::{Command, exit},
    rc::Rc,
    sync::{Arc, Mutex},
};
use typed_ast::{ModuleID, TypedProgramTree};
use utils::generate_random_hex::generate_random_hex;

pub struct CodeGenContext {
    pub opts: CodeGenOptions,
    pub build_manifest: Arc<Mutex<BuildManifest>>,
    pub compiled_objects: Arc<Mutex<Vec<ObjectFileInfo>>>,
    pub master_module_file_path: String,
    resolver_rc: Rc<Resolver>,
    output_kind: OutputKind,
    final_build_dir: String,
}

impl CodeGenContext {
    pub fn new(
        final_build_dir: String,
        opts: CodeGenOptions,
        output_kind: OutputKind,
        resolver_rc: Rc<Resolver>,
        master_module_file_path: String,
    ) -> Self {
        let build_manifest = Arc::new(Mutex::new(BuildManifest::new(
            opts.base_path.clone(),
            final_build_dir.clone(),
        )));
        let compiled_objects = Arc::new(Mutex::new(Vec::<ObjectFileInfo>::new()));

        Self {
            build_manifest,
            compiled_objects,
            opts,
            output_kind,
            final_build_dir,
            resolver_rc,
            master_module_file_path,
        }
    }

    pub fn display_target_machine_information(&self) {
        let target_machine = get_target_machine(
            self.opts.reloc_mode.to_llvm_reloc_mode(),
            self.opts.code_model.to_llvm_code_model(),
            self.opts.cpu.clone(),
            self.opts.target_triple.clone(),
        );

        if !self.opts.quiet {
            println!("Target Triple: {}", target_machine.get_triple().to_string());
            println!("CPU Name: {}", target_machine.get_cpu().to_string());
            println!(
                "Data Layout: {}",
                target_machine
                    .get_target_data()
                    .get_data_layout()
                    .as_str()
                    .to_str()
                    .unwrap()
            );
            println!("Pointer Size: {}-bit", {
                target_machine.get_target_data().get_pointer_byte_size(None) * 8
            });
            println!("Optimization Level: {}", {
                if let Some(opt_level) = self.opts.opt_level {
                    format!("O{}", opt_level)
                } else {
                    "Default".to_string()
                }
            });
            println!("Relocation Mode: {}", self.opts.reloc_mode);
            println!("Code Model: {}", self.opts.code_model);
        }
    }

    fn emit_byte_code(&self, codegen_output: &CodeGenModuleOutput, output_path: String, module_name: String) {
        let bytecode_path = Path::new(&output_path).join(format!("{}.bc", module_name));
        codegen_output.emit_bitcode(&bytecode_path);
    }

    fn emit_dylib(&self, output_path: String) {
        let compiled_objects = self.compiled_objects.lock().unwrap();
        let object_files = compiled_objects.clone();

        let object_files_str_list: Vec<String> = object_files
            .iter()
            .map(|object_file_info| object_file_info.file_path.clone())
            .collect();

        self.trigger_linker_emit_dylib(object_files_str_list, output_path);
    }

    fn emit_asm(&self, codegen_output: &CodeGenModuleOutput, output_path: String, module_name: String) {
        let asm_path = Path::new(&output_path).join(format!("{}.s", module_name));
        codegen_output.emit_asm(&asm_path);
    }

    fn emit_exec(&self, output_path: String) {
        let compiled_objects = self.compiled_objects.lock().unwrap();
        let object_files = compiled_objects.clone();

        let object_files_str_list: Vec<String> = object_files
            .iter()
            .map(|object_file_info| object_file_info.file_path.clone())
            .collect();

        self.trigger_linker(object_files_str_list, output_path);

        drop(compiled_objects);
    }

    fn trigger_linker(&self, object_files_str_list: Vec<String>, output_path: String) {
        let linker = self.opts.linker.clone().unwrap();

        let mut linker_command = Command::new(linker.clone());

        if self.opts.reloc_mode == RelocModeOptions::Static {
            linker_command.arg("-static");
            linker_command.arg("-lc");
        } else if matches!(
            self.opts.reloc_mode,
            RelocModeOptions::PIC | RelocModeOptions::DynamicNoPic
        ) {
            linker_command.arg("-ldl");
            linker_command.arg("-rdynamic");
        }

        linker_command.arg("-funroll-loops");
        linker_command.arg("-flto");

        if let Some(opt_level) = self.opts.opt_level {
            linker_command.arg(format!("-O{}", opt_level));
        }

        linker_command.arg("-o").arg(output_path);
        linker_command.args(object_files_str_list);

        match linker_command.output() {
            Ok(output) => {
                if !output.status.success() {
                    eprintln!("Linker error: {}", String::from_utf8_lossy(&output.stderr));
                    exit(1);
                }
            }
            Err(err) => {
                display_single_custom_diag!(format!("Failed execute linker ({}):\n{}", linker, err.to_string()));
            }
        }
    }

    fn trigger_linker_emit_dylib(&self, object_files_str_list: Vec<String>, output_path: String) {
        let linker = self.opts.linker.clone().unwrap();
        let mut linker_command = Command::new(&linker);

        #[cfg(target_os = "linux")]
        {
            linker_command.arg("-shared");
        }

        #[cfg(target_os = "macos")]
        {
            linker_command.arg("-dynamiclib");
        }

        #[cfg(target_os = "windows")]
        {
            linker_command.arg("-shared");
        }

        if self.opts.reloc_mode == RelocModeOptions::Static {
            linker_command.arg("-static");
            linker_command.arg("-lc");
        } else if matches!(
            self.opts.reloc_mode,
            RelocModeOptions::PIC | RelocModeOptions::DynamicNoPic
        ) {
            linker_command.arg("-ldl");
            linker_command.arg("-rdynamic");
        }

        let library_name = self.opts.project_name.clone().unwrap_or("library".to_string());
        let library_filename = shared_library_filename(&library_name);
        let library_file_path = PathBuf::from(&output_path).join(library_filename);

        linker_command.arg("-o").arg(&library_file_path);
        linker_command.args(object_files_str_list);

        match linker_command.output() {
            Ok(output) => {
                if !output.status.success() {
                    eprintln!("Linker error: {}", String::from_utf8_lossy(&output.stderr));
                    exit(1);
                }
            }
            Err(err) => {
                display_single_custom_diag!(format!("Failed to execute linker ({}):\n{}", linker, err.to_string()));
            }
        }
    }

    pub fn compile_modules(
        &self,
        typed_modules: Vec<(String, ModuleFilePath, ModuleID, Rc<RefCell<TypedProgramTree>>)>,
    ) {
        let build_manifest_guard = self.build_manifest.lock().unwrap();
        let mut build_manifest = build_manifest_guard.read_manifest().unwrap_or_else(|| {
            build_manifest_guard.save_manifest();
            BuildManifest::new(self.opts.base_path.clone(), self.final_build_dir.clone())
        });
        drop(build_manifest_guard);

        fn need_to_be_recompiled(
            output_kind: OutputKind,
            module_file_path: String,
            disable_modulefs_cache: bool,
            build_manifest: &BuildManifest,
        ) -> bool {
            matches!(
                output_kind,
                OutputKind::ByteCode(_) | OutputKind::Asm(_) | OutputKind::LlvmIr(_)
            ) && build_manifest.check_source_code_changed(module_file_path.clone())
                || disable_modulefs_cache
        }

        typed_modules
            .iter()
            .for_each(|(module_name, module_file_path, module_id, program_tree)| {
                if need_to_be_recompiled(
                    self.output_kind.clone(),
                    module_file_path.clone(),
                    self.opts.disable_modulefs_cache,
                    &build_manifest,
                ) {
                    let new_source_code_hash = build_manifest.hash_source_code(module_file_path.clone());
                    build_manifest.update_source_hash(module_file_path.clone(), new_source_code_hash);
                    utils::tui::tui_compiled(module_file_path.clone());

                    let codegen_module = CodeGenModule::new(&self.opts, program_tree.clone());
                    let codegen_output = codegen_module.codegen(
                        self.resolver_rc.clone(),
                        *module_id,
                        module_name.to_string(),
                        module_file_path.clone(),
                    );

                    match self.output_kind.clone() {
                        OutputKind::LlvmIr(output_path) => codegen_output.emit_llvm_ir(output_path),
                        OutputKind::ByteCode(output_path) => {
                            self.emit_byte_code(&codegen_output, output_path, module_name.clone())
                        }
                        OutputKind::Asm(output_path) => {
                            self.emit_asm(&codegen_output, output_path, module_name.clone());
                        }
                        OutputKind::ObjectFile(output_path) => {
                            let obj_file_name = format!(
                                "{}.o",
                                make_module_name(self.master_module_file_path.clone(), module_file_path.clone())
                            );
                            let obj_file_path = Path::new(&output_path).join(obj_file_name);
                            codegen_output.emit_obj(&obj_file_path);
                        }
                        OutputKind::Dylib(output_path) => {
                            self.emit_dylib(output_path);
                        }
                        OutputKind::Executable(_) => {
                            let obj_file_name = format!("{}.o", generate_random_hex());
                            let obj_file_path = Path::new(&self.final_build_dir)
                                .join(OBJECTS_FILENAME)
                                .join(obj_file_name);
                            codegen_output.emit_obj(&obj_file_path);
                            self.insert_compiled_object(obj_file_path.to_str().unwrap().to_string());
                        }
                        OutputKind::None => {}
                    }
                } else {
                    utils::tui::tui_skipped(module_file_path.clone());
                }

                if !build_manifest.check_source_code_hash_exists(module_file_path.clone()) {
                    let source_code_hash = build_manifest.hash_source_code(module_file_path.clone());
                    build_manifest.add_source_code(module_file_path.clone(), source_code_hash.to_string());
                }
            });

        build_manifest.save_manifest();
        utils::tui::tui_compile_finished();

        if let OutputKind::Executable(output_path) = &self.output_kind {
            self.emit_exec(output_path.clone())
        }
    }

    fn insert_compiled_object(&self, obj_file_path: String) {
        let mut compiled_objects = self.compiled_objects.lock().unwrap();
        compiled_objects.push(ObjectFileInfo {
            file_path: obj_file_path,
        });
        drop(compiled_objects);
    }

    #[allow(unused)]
    fn compile_modules_in_parallel(&self, typed_modules: Vec<TypedProgramTree>) {
        todo!();
    }
}

pub(crate) fn get_target_machine(
    reloc_mode: RelocMode,
    code_model: CodeModel,
    cpu: Option<String>,
    target_triple_opt: Option<String>,
) -> TargetMachine {
    Target::initialize_all(&InitializationConfig::default());

    let cpu = if let Some(cpu) = cpu {
        cpu
    } else {
        TargetMachine::get_host_cpu_name().to_string()
    };
    let features = TargetMachine::get_host_cpu_features().to_string();

    let target_triple = if let Some(target_triple_str) = target_triple_opt {
        TargetTriple::create(&target_triple_str)
    } else {
        TargetMachine::get_default_triple()
    };

    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = match target.create_target_machine(
        &target_triple,
        &cpu,
        &features,
        OptimizationLevel::Default,
        reloc_mode,
        code_model,
    ) {
        Some(target_machine) => target_machine,
        None => {
            display_single_custom_diag!(
                "Failed to create LLVM Target Machine with given target_triple, cpu, features.".to_string()
            );
        }
    };

    target_machine
}

pub(crate) fn make_module_name(master_module_file_path: String, current_module_file_path: String) -> String {
    let module_path = Path::new(&current_module_file_path);
    let master_root = Path::new(&master_module_file_path).parent().unwrap();

    let canonicalized = module_path.canonicalize().unwrap();
    let rel = canonicalized.strip_prefix(master_root).unwrap_or(&module_path);

    let stemmed: Vec<String> = rel
        .with_extension("")
        .components()
        .filter_map(|c| {
            let s = c.as_os_str().to_string_lossy().into_owned();
            if s.is_empty() { None } else { Some(s) } // drop empties
        })
        .collect();

    let mut name = stemmed.join(".");

    while name.contains("..") {
        name = name.replace("..", ".");
    }
    name.trim_start_matches('.').to_string()
}

fn shared_library_filename(base: &str) -> String {
    #[cfg(target_os = "linux")]
    {
        format!("lib{}.so", base)
    }

    #[cfg(target_os = "macos")]
    {
        format!("lib{}.dylib", base)
    }

    #[cfg(target_os = "windows")]
    {
        format!("{}.dll", base)
    }
}
