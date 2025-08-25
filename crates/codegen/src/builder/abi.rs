pub(crate) fn make_method_abi_name(module_name: String, struct_name: String, method_name: String) -> String {
    format!("{}.{}.{}", module_name, struct_name, method_name)
}
