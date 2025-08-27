pub(crate) fn make_func_abi_name(module_name: String, func_name: String) -> String {
    format!("{}.{}", module_name, func_name)
}

pub(crate) fn make_global_var_abi_name(module_name: String, global_var_name: String) -> String {
    format!("{}.{}", module_name, global_var_name)
}
