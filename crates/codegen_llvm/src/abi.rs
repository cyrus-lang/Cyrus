use crate::context::CodeGenLLVM;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn generate_enum_abi_name(&self, module_name: String, enum_name: String) -> String {
        format!("{}_{}", module_name, enum_name)
    }

    pub(crate) fn generate_enum_variant_abi_name(
        &self,
        module_name: String,
        enum_name: String,
        variant_name: String,
    ) -> String {
        format!("{}_{}_{}", module_name, enum_name, variant_name)
    }

    pub(crate) fn generate_struct_abi_name(&self, module_name: String, struct_name: String) -> String {
        format!("{}_{}", module_name, struct_name)
    }

    pub(crate) fn generate_func_abi_name(&self, module_name: String, func_name: String) -> String {
        format!("{}_{}", module_name, func_name)
    }

    pub(crate) fn generate_method_abi_name(
        &self,
        module_id: String,
        struct_name: String,
        method_name: String,
    ) -> String {
        format!("{}_{}_{}", module_id, struct_name, method_name)
    }
}
