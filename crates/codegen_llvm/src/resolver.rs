use crate::{
    CodeGenLLVM, funcs::FuncMetadata, modules::ModuleID, types::InternalType, variables::GlobalVariableMetadata,
};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn resolve_func_metadata(&self, module_id: ModuleID, name: String) -> Option<FuncMetadata<'ctx>> {
        let module_metadata = match self.get_module_metadata_by_module_id(module_id) {
            Some(module_metadata) => module_metadata,
            None => return None,
        };

        module_metadata.func_table.get(&name).cloned()
    }

    pub(crate) fn resolve_global_variable_metadata(
        &self,
        module_id: ModuleID,
        name: String,
    ) -> Option<GlobalVariableMetadata<'ctx>> {
        todo!();
    }

    pub(crate) fn resolve_type(&self, module_id: ModuleID, name: String) -> Option<InternalType<'ctx>> {
        todo!()
    }
}
