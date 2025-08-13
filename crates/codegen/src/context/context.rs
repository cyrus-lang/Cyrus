use super::{build_manifest::BuildManifest, object_file_info::ObjectFileInfo};
use crate::{
    builder::module::CodeGenModule,
    options::{CodeGenOptions, OutputKind, get_final_build_dir},
};
use resolver::Resolver;
use std::{
    cell::RefCell,
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
    pub fn new(opts: CodeGenOptions, output_kind: OutputKind, resolver_rc: Rc<Resolver>) -> Self {
        let build_manifest = Arc::new(Mutex::new(BuildManifest::default()));
        let final_build_dir = get_final_build_dir(opts.build_dir.clone());
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

    pub fn emit_exec(&self, output_path: String) {
        todo!()
    }

    pub fn compile_modules(&self, typed_modules: Vec<(String, ModuleID, Rc<RefCell<TypedProgramTree>>)>) {
        typed_modules.iter().for_each(|(module_name, module_id, program_tree)| {
            let codegen_module = CodeGenModule::new(&self.opts, program_tree.clone());
            let codegen_output = codegen_module.codegen(self.resolver_rc.clone(), *module_id, module_name.to_string());

            match self.output_kind.clone() {
                OutputKind::LlvmIr(output_path) => codegen_output.emit_llvm_ir(output_path),
                OutputKind::Asm(_) => todo!(),
                OutputKind::ObjectFile(_) => todo!(),
                OutputKind::Dylib(_) => todo!(),
                OutputKind::Run => {}
                OutputKind::None => {}
            }

            // let bytecode_path = Path::new("");
            // codegen_output.emit_bitcode(bytecode_path);
            // let mut compiled_objects = self.compiled_objects.lock().unwrap();
            // compiled_objects.push(ObjectFileInfo::new(emit_object_result.file_path));
            // drop(compiled_objects);
        });
    }

    fn compile_modules_in_paralell(&self, typed_modules: Vec<TypedProgramTree>) {
        todo!();
    }
}
