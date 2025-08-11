use super::{build_manifest::BuildManifest, object_file_info::ObjectFileInfo};
use crate::{
    builder::module::CodeGenModule,
    options::{CodeGenOptions, OutputKind, get_final_build_dir},
};
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};
use typed_ast::TypedProgramTree;

pub struct CodeGenContext {
    pub options: CodeGenOptions,
    pub build_manifest: Arc<Mutex<BuildManifest>>,
    pub compiled_objects: Arc<Mutex<Vec<ObjectFileInfo>>>,
    output_kind: OutputKind,
    final_build_dir: String,
}

impl CodeGenContext {
    pub fn new(options: CodeGenOptions, output_kind: OutputKind) -> Self {
        let build_manifest = Arc::new(Mutex::new(BuildManifest::default()));
        let final_build_dir = get_final_build_dir(options.build_dir.clone());
        let compiled_objects = Arc::new(Mutex::new(Vec::<ObjectFileInfo>::new()));

        Self {
            build_manifest,
            compiled_objects,
            options,
            output_kind,
            final_build_dir,
        }
    }

    pub fn compile_executable(compiled_objects: HashMap<String, ObjectFileInfo>) {
        todo!();
    }

    fn compile_modules(&self, typed_modules: Vec<TypedProgramTree>) {
        typed_modules.iter().for_each(|program_tree| {
            let codegen_module = CodeGenModule::new(program_tree);
            let emit_object_result = codegen_module.emit_object_file();

            let mut compiled_objects = self.compiled_objects.lock().unwrap();
            compiled_objects.push(ObjectFileInfo::new(emit_object_result.file_path));
            drop(compiled_objects);
        });
    }

    fn compile_modules_in_paralell(&self, typed_modules: Vec<TypedProgramTree>) {
        todo!();
    }
}
