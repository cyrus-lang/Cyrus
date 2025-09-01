use ast::AccessSpecifier;

pub(crate) fn make_func_abi_name(module_name: String, func_name: String, vis: AccessSpecifier) -> String {
    if func_name == "main" || matches!(vis, AccessSpecifier::Extern | AccessSpecifier::PublicExtern) {
        func_name
    } else {
        format!("{}.{}", module_name, func_name)
    }
}

pub(crate) fn make_global_var_abi_name(module_name: String, global_var_name: String, vis: AccessSpecifier) -> String {
    if matches!(vis, AccessSpecifier::Extern | AccessSpecifier::PublicExtern) {
        global_var_name
    } else {
        format!("{}.{}", module_name, global_var_name)
    }
}
