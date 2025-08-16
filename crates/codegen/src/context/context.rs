use super::{build_manifest::BuildManifest, object_file_info::ObjectFileInfo};
use crate::{
    builder::module::{CodeGenModule, CodeGenModuleOutput},
    options::{CodeGenOptions, OutputKind},
};
use resolver::{Resolver, moduleloader::ModuleFilePath};
use std::{
    cell::RefCell,
    path::Path,
    rc::Rc,
    sync::{Arc, Mutex},
};
use typed_ast::{ModuleID, TypedProgramTree};

pub struct CodeGenContext {
    pub opts: CodeGenOptions,
    pub build_manifest: Arc<Mutex<BuildManifest>>,
    pub compiled_objects: Arc<Mutex<Vec<ObjectFileInfo>>>,
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
        }
    }

    fn emit_byte_code(&self, codegen_output: &CodeGenModuleOutput, output_path: String, module_name: String) {
        let bytecode_path = Path::new(&output_path).join(format!("{}.bc", module_name));
        codegen_output.emit_bitcode(&bytecode_path);
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

        typed_modules
            .iter()
            .for_each(|(module_name, module_file_path, module_id, program_tree)| {
                if !build_manifest.check_source_code_hash_exists(module_file_path.clone()) {
                    let source_code_hash = build_manifest.hash_source_code(module_file_path.clone());
                    build_manifest.add_source_code(module_file_path.clone(), source_code_hash.to_string());
                }

                if build_manifest.check_source_code_changed(module_file_path.clone()) {
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
                        OutputKind::Asm(_) => todo!(),
                        OutputKind::ObjectFile(_) => todo!(),
                        OutputKind::Dylib(_) => todo!(),
                        OutputKind::Executable(_) => todo!(),
                        OutputKind::None => {}
                    }
                } else {
                    utils::tui::tui_skipped(module_file_path.clone());
                }
            });

        build_manifest.save_manifest();
        utils::tui::tui_compile_finished();
    }

    #[allow(unused)]
    fn compile_modules_in_parallel(&self, typed_modules: Vec<TypedProgramTree>) {
        todo!();
    }
}
