/// Trait that defines how to generate ABI-safe names for the language.
/// This allows multiple ABIs to coexist with different name mangling rules.
pub trait ABINameMangling: Send + Sync {
    /// Function names
    fn func_name(&self, module_name: &str, func_name: &str, exported: bool) -> String;

    /// Method names
    fn method_name(&self, module_name: &str, object_name: &str, method_name: &str) -> String;

    /// Global variable names
    fn global_var_name(&self, module_name: &str, var_name: &str) -> String;

    /// Struct names
    fn struct_name(&self, module_name: &str, struct_name: &str) -> String;

    /// Union names
    fn union_name(&self, module_name: &str, union_name: &str) -> String;

    /// Enum names
    fn enum_name(&self, module_name: &str, enum_name: &str) -> String;

    /// Enum variant names
    fn enum_variant_name(&self, module_name: &str, enum_name: &str, variant_name: &str) -> String;
}

/// Default ABI mangler
#[allow(non_camel_case_types)]
pub struct Cyrus_ABI;

impl Cyrus_ABI {
    pub fn new() -> Self {
        Self {}
    }

    fn type_name(module_name: &str, type_name: &str) -> String {
        format!("{module_name}_{type_name}")
    }

    fn sanitize(name: &str) -> String {
        let name = name.trim_start_matches('_');
        name.chars()
            .map(|c| if c.is_ascii_alphanumeric() || c == '_' { c } else { '_' })
            .collect()
    }
}

impl ABINameMangling for Cyrus_ABI {
    fn func_name(&self, module_name: &str, func_name: &str, exported: bool) -> String {
        if func_name == "main" {
            func_name.to_string()
        } else {
            if exported {
                func_name.to_string()
            } else {
                format!("{}.{}", Self::sanitize(module_name), Self::sanitize(func_name))
            }
        }
    }

    fn method_name(&self, module_name: &str, object_name: &str, method_name: &str) -> String {
        format!(
            "{}_{}_{}",
            Self::sanitize(module_name),
            Self::sanitize(object_name),
            Self::sanitize(method_name)
        )
    }

    fn global_var_name(&self, module_name: &str, var_name: &str) -> String {
        format!("{}_{}", Self::sanitize(module_name), Self::sanitize(var_name))
    }

    fn struct_name(&self, module_name: &str, struct_name: &str) -> String {
        Self::type_name(module_name, struct_name)
    }

    fn union_name(&self, module_name: &str, union_name: &str) -> String {
        Self::type_name(module_name, union_name)
    }

    fn enum_name(&self, module_name: &str, enum_name: &str) -> String {
        Self::type_name(module_name, enum_name)
    }

    fn enum_variant_name(&self, module_name: &str, enum_name: &str, variant_name: &str) -> String {
        format!(
            "{}_{}_{}",
            Self::sanitize(module_name),
            Self::sanitize(enum_name),
            Self::sanitize(variant_name)
        )
    }
}

#[allow(non_camel_case_types)]
pub struct C_ABI;

impl C_ABI {
    pub fn new() -> Self {
        Self {}
    }

    fn sanitize(name: &str) -> String {
        let name = name.trim_start_matches('_');
        name.chars()
            .map(|c| if c.is_ascii_alphanumeric() || c == '_' { c } else { '_' })
            .collect()
    }
}

impl ABINameMangling for C_ABI {
    fn func_name(&self, _module_name: &str, func_name: &str, _exported: bool) -> String {
        Self::sanitize(func_name)
    }

    fn method_name(&self, _module_name: &str, object_name: &str, method_name: &str) -> String {
        format!("{}_{}", Self::sanitize(object_name), Self::sanitize(method_name))
    }

    fn global_var_name(&self, _module_name: &str, var_name: &str) -> String {
        Self::sanitize(var_name)
    }

    fn struct_name(&self, _module_name: &str, struct_name: &str) -> String {
        Self::sanitize(struct_name)
    }

    fn union_name(&self, _module_name: &str, union_name: &str) -> String {
        Self::sanitize(union_name)
    }

    fn enum_name(&self, _module_name: &str, enum_name: &str) -> String {
        Self::sanitize(enum_name)
    }

    fn enum_variant_name(&self, _module_name: &str, enum_name: &str, variant_name: &str) -> String {
        format!("{}_{}", Self::sanitize(enum_name), Self::sanitize(variant_name))
    }
}
