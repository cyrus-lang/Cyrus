use inkwell::{builder::Builder, context::Context, module::Module};
use typed_ast::TypedProgramTree;

pub struct CodeGenModule<'a> {
    module: Module<'a>,
    ctx: &'a Context,
    builder: Builder<'a>,
    program_tree: &'a TypedProgramTree,
}

pub struct CodeGenModuleEmitObjectResult {
    pub file_path: String,
}

impl<'a> CodeGenModule<'a> {
    pub fn new(program_tree: &TypedProgramTree) -> Self {
        todo!();
    }

    pub fn emit_object_file(&self) -> CodeGenModuleEmitObjectResult {
        todo!();
    }
}
