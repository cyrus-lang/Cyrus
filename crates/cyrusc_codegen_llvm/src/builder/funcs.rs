use crate::builder::builder::IRBuilderCtx;
use cyrusc_cir::{CIRBlockStmt, CIRFuncDeclStmt, cir_func_decl_as_func_ty};
use inkwell::values::FunctionValue;

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_func_decl(&self, func_decl: &CIRFuncDeclStmt) -> FunctionValue<'ll> {
        let fn_type = self.emit_func_ty(cir_func_decl_as_func_ty(func_decl));

        let func_name = &func_decl.name;
        let llvmmodule = self.llvmmodule.borrow();

        let fn_value = llvmmodule
            .get_function(func_name)
            .unwrap_or_else(|| llvmmodule.add_function(func_name, fn_type, None));

        fn_value
    }

    pub(crate) fn emit_func_body(&mut self, fn_value: FunctionValue<'ll>, cir_block: &CIRBlockStmt) {
        

        self.emit_body(cir_block);
    }
}
