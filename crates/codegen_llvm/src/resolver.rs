use ast::{
    ast::{Identifier, ModuleImport, ModuleSegment},
    token::{Location, Span},
};

use crate::{
    context::CodeGenLLVM,
    enums::{EnumID, EnumMetadata},
    funcs::FuncMetadata,
    modules::ModuleID,
    structs::{StructID, StructMetadata},
    types::TypedefMetadata,
    variables::GlobalVariableMetadata,
};

#[derive(Debug, Clone)]
pub enum MetadataResolverResult<'a> {
    Func(FuncMetadata<'a>),
    Struct(StructMetadata<'a>),
    Typedef(TypedefMetadata<'a>),
    GlobalVariable(GlobalVariableMetadata<'a>),
    Enum(EnumMetadata<'a>),
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
                    None => match self.resolve_typedef_metadata(module_id, name.clone()) {
                        Some(typedef_metadata) => Some(MetadataResolverResult::Typedef(typedef_metadata)),
                        None => match self.resolve_enum_metadata(
                            module_id,
                            ModuleImport {
                                segments: vec![ModuleSegment::SubModule(Identifier {
                                    name,
                                    loc: Location::default(),
                                    span: Span::default(),
                                })],
                                loc: Location::default(),
                                span: Span::default(),
                            },
                        ) {
                            Some(enum_metadata) => Some(MetadataResolverResult::Enum(enum_metadata)),
                            None => None,
                        },
                    },
                },
            },
        }
    }

    pub(crate) fn resolve_enum_metadata(
        &self,
        module_id: ModuleID,
        module_import: ModuleImport,
    ) -> Option<EnumMetadata<'ctx>> {
        if let Some(identifier) = module_import.as_identifier() {
            let module_metadata = match self.get_module_metadata_by_module_id(module_id) {
                Some(module_metadata) => module_metadata,
                None => return None,
            };

            let opt = module_metadata.enum_table.get(&identifier.name).cloned();
            drop(module_metadata);
            opt
        } else {
            // FIXME Implement resolve enum metadata from module import.
            todo!();
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

    pub(crate) fn resolve_enum_metadata_with_enum_id(&self, enum_id: EnumID) -> Option<EnumMetadata<'ctx>> {
        let module_metadata_registry = self.module_metadata_registry.borrow();

        let enum_metadata_opt = module_metadata_registry
            .iter()
            .find(|m| m.enum_table.iter().find(|r| r.1.enum_id == enum_id).is_some())
            .cloned();

        drop(module_metadata_registry);

        match enum_metadata_opt {
            Some(module_metadata) => {
                let enum_metadata = module_metadata
                    .enum_table
                    .iter()
                    .find(|r| r.1.enum_id == enum_id)
                    .unwrap()
                    .1
                    .clone();

                Some(enum_metadata)
            }
            None => None,
        }
    }

    pub(crate) fn resolve_struct_metadata_with_struct_id(&self, struct_id: StructID) -> Option<StructMetadata<'ctx>> {
        let module_metadata_registry = self.module_metadata_registry.borrow();

        let struct_metadata_opt = module_metadata_registry
            .iter()
            .find(|m| m.struct_table.iter().find(|r| r.1.struct_id == struct_id).is_some())
            .cloned();

        drop(module_metadata_registry);

        match struct_metadata_opt {
            Some(module_metadata) => {
                let struct_metadata = module_metadata
                    .struct_table
                    .iter()
                    .find(|r| r.1.struct_id == struct_id)
                    .unwrap()
                    .1
                    .clone();

                Some(struct_metadata)
            }
            None => None,
        }
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

    pub(crate) fn resolve_typedef_metadata(&self, module_id: ModuleID, name: String) -> Option<TypedefMetadata<'ctx>> {
        let module_metadata = match self.get_module_metadata_by_module_id(module_id) {
            Some(module_metadata) => module_metadata,
            None => return None,
        };

        let opt = module_metadata.typedef_table.get(&name).cloned();
        drop(module_metadata);
        opt
    }
}
