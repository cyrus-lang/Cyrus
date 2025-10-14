use ast::AccessSpecifier;
use static_analyzer::monomorph::MonomorphID;

pub(crate) fn make_func_abi_name(module_name: &String, func_name: &String, vis: AccessSpecifier) -> String {
    if func_name == "main" || matches!(vis, AccessSpecifier::Extern | AccessSpecifier::PublicExtern) {
        func_name.clone()
    } else {
        format!("{}.{}", module_name, func_name)
    }
}

pub(crate) fn make_global_var_abi_name(
    module_name: &String,
    global_var_name: &String,
    vis: &AccessSpecifier,
) -> String {
    if matches!(vis, AccessSpecifier::Extern | AccessSpecifier::PublicExtern) {
        global_var_name.clone()
    } else {
        format!("{}.{}", module_name, global_var_name)
    }
}

pub(crate) fn generate_struct_abi_name(module_name: &String, struct_name: &String) -> String {
    format!("{}_{}", module_name, struct_name)
}

pub(crate) fn generate_union_abi_name(module_name: &String, union_name: &String) -> String {
    format!("{}_{}", module_name, union_name)
}

pub(crate) fn generate_enum_abi_name(module_name: &String, enum_name: &String) -> String {
    format!("{}_{}", module_name, enum_name)
}

pub(crate) fn generate_enum_variant_abi_name(
    module_name: &String,
    enum_name: &String,
    variant_name: &String,
) -> String {
    format!("{}_{}_{}", module_name, enum_name, variant_name)
}

pub(crate) fn generate_monomorphic_struct_abi_name(
    module_name: &String,
    struct_name: &String,
    monomorph_id: MonomorphID,
) -> String {
    format!("{}_{}@{}", module_name, struct_name, monomorph_id)
}

pub(crate) fn generate_monomorphic_union_abi_name(
    module_name: &String,
    union_name: &String,
    monomorph_id: MonomorphID,
) -> String {
    format!("{}_{}@{}", module_name, union_name, monomorph_id)
}
