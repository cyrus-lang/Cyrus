use crate::visibility::Visibility;

/// Trait that defines how to generate ABI-safe names for the language.
/// This allows multiple ABIs to coexist with different name mangling rules.
pub trait ABINameMangling {
    /// Function names
    fn func_name(&self, module_name: &str, func_name: &str, vis: Visibility) -> String;

    /// Global variable names
    fn global_var_name(&self, module_name: &str, var_name: &str, vis: Visibility) -> String;

    /// Struct names
    fn struct_name(&self, module_name: &str, struct_name: &str) -> String;

    /// Union names
    fn union_name(&self, module_name: &str, union_name: &str) -> String;

    /// Enum names
    fn enum_name(&self, module_name: &str, enum_name: &str) -> String;

    /// Enum variant names
    fn enum_variant_name(&self, module_name: &str, enum_name: &str, variant_name: &str) -> String;

    /// Monomorphized struct name
    fn monomorph_struct_name(&self, module_name: &str, struct_name: &str, monomorph_id: &str) -> String;

    /// Monomorphized union name
    fn monomorph_union_name(&self, module_name: &str, union_name: &str, monomorph_id: &str) -> String;

    /// Monomorphized enum name
    fn monomorph_enum_name(&self, module_name: &str, enum_name: &str, monomorph_id: &str) -> String;
}

/// Default ABI mangler
#[allow(non_camel_case_types)]
pub struct Cyrus_ABI;

impl Cyrus_ABI {
    fn type_name(module_name: &str, type_name: &str) -> String {
        format!("{module_name}_{type_name}")
    }

    fn monomorphic_type_name(module_name: &str, type_name: &str, monomorph_id: &str) -> String {
        format!("{module_name}_{type_name}@{monomorph_id}")
    }
}

impl ABINameMangling for Cyrus_ABI {
    fn func_name(&self, module_name: &str, func_name: &str, vis: Visibility) -> String {
        if func_name == "main" {
            func_name.to_string()
        } else {
            format!("{module_name}.{func_name}")
        }
    }

    fn global_var_name(&self, _: &str, var_name: &str, _: Visibility) -> String {
        var_name.to_string()
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
        format!("{module_name}_{enum_name}_{variant_name}")
    }

    fn monomorph_struct_name(&self, module_name: &str, struct_name: &str, monomorph_id: &str) -> String {
        Self::monomorphic_type_name(module_name, struct_name, monomorph_id)
    }

    fn monomorph_union_name(&self, module_name: &str, union_name: &str, monomorph_id: &str) -> String {
        Self::monomorphic_type_name(module_name, union_name, monomorph_id)
    }

    fn monomorph_enum_name(&self, module_name: &str, enum_name: &str, monomorph_id: &str) -> String {
        Self::monomorphic_type_name(module_name, enum_name, monomorph_id)
    }
}

#[allow(non_camel_case_types)]
pub struct C_ABI;

impl C_ABI {
    fn sanitize(name: &str) -> String {
        // Replace non-alphanumeric characters with underscores
        name.chars()
            .map(|c| if c.is_ascii_alphanumeric() || c == '_' { c } else { '_' })
            .collect()
    }

    fn monomorphic_suffix(monomorph_id: &str) -> String {
        if monomorph_id.is_empty() {
            "".to_string()
        } else {
            format!("_{}", monomorph_id)
        }
    }
}

impl ABINameMangling for C_ABI {
    fn func_name(&self, _module_name: &str, func_name: &str, _vis: Visibility) -> String {
        Self::sanitize(func_name)
    }

    fn global_var_name(&self, _module_name: &str, var_name: &str, _vis: Visibility) -> String {
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

    fn monomorph_struct_name(&self, _module_name: &str, struct_name: &str, monomorph_id: &str) -> String {
        format!(
            "{}{}",
            Self::sanitize(struct_name),
            Self::monomorphic_suffix(monomorph_id)
        )
    }

    fn monomorph_union_name(&self, _module_name: &str, union_name: &str, monomorph_id: &str) -> String {
        format!(
            "{}{}",
            Self::sanitize(union_name),
            Self::monomorphic_suffix(monomorph_id)
        )
    }

    fn monomorph_enum_name(&self, _module_name: &str, enum_name: &str, monomorph_id: &str) -> String {
        format!(
            "{}{}",
            Self::sanitize(enum_name),
            Self::monomorphic_suffix(monomorph_id)
        )
    }
}
