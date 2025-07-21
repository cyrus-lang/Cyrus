use crate::{
    funcs::FuncMetadata, modules::ModuleID, structs::StructMetadata, types::{InternalType, TypedefMetadata}, variables::GlobalVariableMetadata, CodeGenLLVM
};

#[derive(Debug, Clone)]
pub enum MetadataResolverResult<'a> {
    Func(FuncMetadata<'a>),
    Struct(StructMetadata<'a>),
    Typedef(TypedefMetadata<'a>),
    GlobalVariable(GlobalVariableMetadata<'a>),
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn resolve_metadata(
        &self,
        module_id: ModuleID,
        name: String,
    ) -> Option<MetadataResolverResult<'ctx>> {
        todo!();
    }

    pub(crate) fn resolve_func_metadata(
        &self,
        module_id: ModuleID,
        name: String,
    ) -> Option<FuncMetadata<'ctx>> {
        
        todo!();
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
