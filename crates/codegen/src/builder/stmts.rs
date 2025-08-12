use super::module::CodeGenBuilder;
use typed_ast::{TypedFuncDecl, TypedStatement};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_toplevel_statements(&self, stmts: &Vec<TypedStatement>) {
        for stmt in stmts {
            match stmt {
                TypedStatement::GlobalVariable(typed_global_variable) => todo!(),
                TypedStatement::FuncDef(typed_func_def) => todo!(),
                TypedStatement::FuncDecl(typed_func_decl) => {
                    self.build_func_decl_stmt(typed_func_decl);
                }
                TypedStatement::Variable(typed_variable) => todo!(),
                TypedStatement::Struct(typed_struct) => todo!(),
                TypedStatement::Enum(typed_enum) => todo!(),
                TypedStatement::Interface(typed_interface) => todo!(),
                // Ignored as toplevel statements
                TypedStatement::BlockStatement(_) => continue,
                TypedStatement::If(_) => continue,
                TypedStatement::Return(_) => continue,
                TypedStatement::Break(_) => continue,
                TypedStatement::Continue(_) => continue,
                TypedStatement::For(_) => continue,
                TypedStatement::Foreach(_) => continue,
                TypedStatement::Switch(_) => continue,
                TypedStatement::Import(_) => continue,
                TypedStatement::Typedef(_) => continue,
                TypedStatement::Expression(_) => continue,
            }
        }
    }

    fn build_func_decl_stmt(&self, func_decl: &TypedFuncDecl) {
        self.build_func_decl(
            func_decl.name.clone(),
            func_decl.params.clone(),
            func_decl.return_type.clone(),
            func_decl.vis.clone(),
            None,
        );
    }
}
