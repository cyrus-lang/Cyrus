use crate::visibility::Visibility;

/// Makes an ABI-safe function name based on visibility and module context.
pub fn make_func_abi_name(module_name: &str, func_name: &str, vis: Visibility) -> String {
    match vis {
        _ if func_name == "main" => func_name.to_string(),
        _ => format!("{module_name}.{func_name}"),
    }
}

/// Makes an ABI-safe global variable name, respecting external linkage rules.
pub fn make_global_var_abi_name(_: &str, var_name: &str, _: &Visibility) -> String {
    var_name.to_string()
}

/// Internal helper for constructing basic ABI-safe type names.
fn make_type_abi_name(module_name: &str, type_name: &str) -> String {
    format!("{module_name}_{type_name}")
}

/// Internal helper for constructing ABI-safe names for monomorphized types.
fn make_monomorphic_type_abi_name(module_name: &str, type_name: &str, monomorph_id: &str) -> String {
    format!("{module_name}_{type_name}@{monomorph_id}")
}

/// Makes an ABI-safe struct name.
pub fn make_struct_abi_name(module_name: &str, struct_name: &str) -> String {
    make_type_abi_name(module_name, struct_name)
}

/// Makes an ABI-safe union name.
pub fn make_union_abi_name(module_name: &str, union_name: &str) -> String {
    make_type_abi_name(module_name, union_name)
}

/// Makes an ABI-safe enum name.
pub fn make_enum_abi_name(module_name: &str, enum_name: &str) -> String {
    make_type_abi_name(module_name, enum_name)
}

/// Makes an ABI-safe enum variant name.
pub fn make_enum_variant_abi_name(module_name: &str, enum_name: &str, variant_name: &str) -> String {
    format!("{module_name}_{enum_name}_{variant_name}")
}

/// Makes an ABI-safe name for a monomorphized struct.
pub fn make_monomorphic_struct_abi_name(module_name: &str, struct_name: &str, monomorph_id: &str) -> String {
    make_monomorphic_type_abi_name(module_name, struct_name, monomorph_id)
}

/// Makes an ABI-safe name for a monomorphized union.
pub fn make_monomorphic_union_abi_name(module_name: &str, union_name: &str, monomorph_id: &str) -> String {
    make_monomorphic_type_abi_name(module_name, union_name, monomorph_id)
}

/// Makes an ABI-safe name for a monomorphized enum.
pub fn make_monomorphic_enum_abi_name(module_name: &str, enum_name: &str, monomorph_id: &str) -> String {
    make_monomorphic_type_abi_name(module_name, enum_name, monomorph_id)
}
