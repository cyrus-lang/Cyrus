use crate::{
    context::CodeGenLLVM, funcs::FuncMetadata, modules::ModuleID, structs::StructMetadata, types::TypedefMetadata,
    variables::GlobalVariableMetadata,
};

#[derive(Debug, Clone)]
pub enum MetadataResolverResult<'a> {
    Func(FuncMetadata<'a>),
    Struct(StructMetadata<'a>),
    Typedef(TypedefMetadata<'a>),
    GlobalVariable(GlobalVariableMetadata<'a>),
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn resolve_metadata(&self, module_id: ModuleID, name: String) -> Option<MetadataResolverResult<'ctx>> {
        match self.resolve_func_metadata(module_id, name.clone()) {
            Some(func_metadata) => Some(MetadataResolverResult::Func(func_metadata)),
            None => match self.resolve_struct_metadata(module_id, name.clone()) {
                Some(struct_metadata) => Some(MetadataResolverResult::Struct(struct_metadata)),
                None => match self.resolve_global_variable_metadata(module_id, name.clone()) {
                    Some(global_variable_metadata) => {
                        Some(MetadataResolverResult::GlobalVariable(global_variable_metadata))
                    }
                    None => match self.resolve_typedef(module_id, name) {
                        Some(typedef_metadata) => Some(MetadataResolverResult::Typedef(typedef_metadata)),
                        None => None,
                    },
                },
            },
        }
    }

    pub(crate) fn resolve_func_metadata(&self, module_id: ModuleID, name: String) -> Option<FuncMetadata<'ctx>> {
        let module_metadata = match self.get_module_metadata_by_module_id(module_id) {
            Some(module_metadata) => module_metadata,
            None => return None,
        };

        let opt = module_metadata.func_table.get(&name).cloned();
        drop(module_metadata);
        opt
    }

    pub(crate) fn resolve_struct_metadata(&self, module_id: ModuleID, name: String) -> Option<StructMetadata<'ctx>> {
        let module_metadata = match self.get_module_metadata_by_module_id(module_id) {
            Some(module_metadata) => module_metadata,
            None => return None,
        };

        let opt = module_metadata.struct_table.get(&name).cloned();
        drop(module_metadata);
        opt
    }

    pub(crate) fn resolve_global_variable_metadata(
        &self,
        module_id: ModuleID,
        name: String,
    ) -> Option<GlobalVariableMetadata<'ctx>> {
        let module_metadata = match self.get_module_metadata_by_module_id(module_id) {
            Some(module_metadata) => module_metadata,
            None => return None,
        };

        let opt = module_metadata.global_variables_table.get(&name).cloned();
        drop(module_metadata);
        opt
    }

    pub(crate) fn resolve_typedef(&self, module_id: ModuleID, name: String) -> Option<TypedefMetadata<'ctx>> {
        let module_metadata = match self.get_module_metadata_by_module_id(module_id) {
            Some(module_metadata) => module_metadata,
            None => return None,
        };

        let opt = module_metadata.typedef_table.get(&name).cloned();
        drop(module_metadata);
        opt
    }
}
