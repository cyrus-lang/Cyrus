use ast::ast::Enum;

use crate::CodeGenLLVM;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_enum(&self, enum_statement: Enum) {
        dbg!(enum_statement.clone());
        todo!();
    }
}
