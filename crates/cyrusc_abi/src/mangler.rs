/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::modifiers::{FuncModifiers, GlobalVarModifiers};
use once_cell::sync::Lazy;
use std::path::{Path, PathBuf};

pub static CYRUS_ABI: Lazy<Cyrus_ABI_Impl> = Lazy::new(|| Cyrus_ABI_Impl::new());
pub static C_ABI: Lazy<C_ABI_Impl> = Lazy::new(|| C_ABI_Impl::new());

pub static DEFAULT_ABI: Lazy<Cyrus_ABI_Impl> = Lazy::new(Cyrus_ABI_Impl::new);

/// Trait that defines how to generate ABI-safe names for the language.
/// This allows multiple ABIs to coexist with different name mangling rules.
pub trait ABINameMangler: Send + Sync {
    /// Function names
    fn func_name(&self, module_name: &str, func_name: &str) -> String;

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

    /// VTable instances names
    fn vtable_name(&self, interface_name: &str, vtable_id: &str) -> String;
}

/// Default ABI mangler
#[allow(non_camel_case_types)]
pub struct Cyrus_ABI_Impl;

impl Cyrus_ABI_Impl {
    pub fn new() -> Self {
        Self {}
    }

    fn type_name(module_name: &str, type_name: &str) -> String {
        format!("{module_name}${type_name}")
    }

    fn sanitize(name: &str) -> String {
        let name = name.trim_start_matches('_');
        name.chars()
            .map(|c| if c.is_ascii_alphanumeric() || c == '_' { c } else { '_' })
            .collect()
    }
}

impl ABINameMangler for Cyrus_ABI_Impl {
    fn func_name(&self, module_name: &str, func_name: &str) -> String {
        if func_name == "main" {
            func_name.to_string()
        } else {
            format!("{}${}", Self::sanitize(module_name), Self::sanitize(func_name))
        }
    }

    fn method_name(&self, module_name: &str, object_name: &str, method_name: &str) -> String {
        format!(
            "{}${}.{}",
            Self::sanitize(module_name),
            Self::sanitize(object_name),
            Self::sanitize(method_name)
        )
    }

    fn global_var_name(&self, module_name: &str, var_name: &str) -> String {
        format!("{}${}", Self::sanitize(module_name), Self::sanitize(var_name))
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
            "{}${}${}",
            Self::sanitize(module_name),
            Self::sanitize(enum_name),
            Self::sanitize(variant_name)
        )
    }

    fn vtable_name(&self, interface_name: &str, vtable_id: &str) -> String {
        format!("__vtable_{}_{}", interface_name, vtable_id)
    }
}

#[allow(non_camel_case_types)]
pub struct C_ABI_Impl;

impl C_ABI_Impl {
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

impl ABINameMangler for C_ABI_Impl {
    fn func_name(&self, _module_name: &str, func_name: &str) -> String {
        Self::sanitize(func_name)
    }

    fn method_name(&self, _module_name: &str, object_name: &str, method_name: &str) -> String {
        format!("{}__{}", Self::sanitize(object_name), Self::sanitize(method_name))
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
        format!("{}__{}", Self::sanitize(enum_name), Self::sanitize(variant_name))
    }

    fn vtable_name(&self, interface_name: &str, vtable_id: &str) -> String {
        format!("__vtable_{}_{}", interface_name, vtable_id)
    }
}

pub fn mangle_global_var(modifiers: &GlobalVarModifiers, module_name: &str, var_name: &str) -> String {
    match &modifiers.linkage {
        Some(linkage) => linkage.abi_mangler().global_var_name(module_name, var_name),
        None => CYRUS_ABI.global_var_name(module_name, var_name),
    }
}

pub fn mangle_func(modifiers: &FuncModifiers, module_name: &str, name: &str) -> String {
    match &modifiers.linkage {
        Some(linkage) => linkage.abi_mangler().func_name(module_name, name),
        None => CYRUS_ABI.func_name(module_name, name),
    }
}

pub fn mangle_method(module_name: &str, object_name: &str, name: &str) -> String {
    CYRUS_ABI.method_name(module_name, object_name, name)
}

pub fn make_module_name_from_filepath<P: AsRef<Path>>(
    path: P,
    base_path: Option<&Path>,
    stdlib_path: Option<&Path>,
) -> String {
    let path_ref = path.as_ref();
    let path = path_ref.canonicalize().unwrap_or_else(|_| PathBuf::from(path_ref));

    // try to strip the base path if provided
    let relative_path = if let Some(base) = base_path {
        path.strip_prefix(base).unwrap_or(&path).to_path_buf()
    } else {
        path.clone()
    };

    let mut parts: Vec<String> = relative_path
        .iter()
        .filter_map(|c| {
            let s = c.to_string_lossy();
            if s.is_empty() { None } else { Some(s.to_string()) }
        })
        .collect();

    // remove extension from last component
    if let Some(last) = parts.last_mut() {
        if let Some(stripped) = last.strip_suffix(".cyrus") {
            *last = stripped.to_string();
        }
    }

    // detect if path belongs to stdlib
    let is_stdlib = stdlib_path.map(|s| path.starts_with(s)).unwrap_or(false);

    let mut module_name = parts.join("_");

    // avoid double `stdlib_stdlib` prefix
    if module_name.starts_with("stdlib_") && is_stdlib {
        // already prefixed
    } else if is_stdlib {
        module_name = format!("stdlib_{}", module_name);
    }

    // remove leading underscores
    module_name = module_name.trim_start_matches('_').to_string();

    // sanitize
    module_name
        .chars()
        .map(|ch| if ch.is_alphanumeric() || ch == '_' { ch } else { '_' })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn tmp_module_with_base_path() {
        let name = make_module_name_from_filepath(
            "/home/taha/Code/Cyrus/tmp/main.cyrus",
            Some(Path::new("/home/taha/Code/Cyrus")),
            None,
        );
        assert_eq!(name, "tmp_main");
    }

    #[test]
    fn stdlib_module_with_prefix() {
        let name = make_module_name_from_filepath(
            "/home/taha/Code/Cyrus/stdlib/math.cyrus",
            Some(Path::new("/home/taha/Code/Cyrus")),
            Some(Path::new("/home/taha/Code/Cyrus/stdlib")),
        );
        assert_eq!(name, "stdlib_math");
    }

    #[test]
    fn stdlib_nested_module() {
        let name = make_module_name_from_filepath(
            "/home/taha/Code/Cyrus/stdlib/libc/abi.cyrus",
            Some(Path::new("/home/taha/Code/Cyrus")),
            Some(Path::new("/home/taha/Code/Cyrus/stdlib")),
        );
        assert_eq!(name, "stdlib_libc_abi");
    }

    #[test]
    fn non_stdlib_nested_module() {
        let name = make_module_name_from_filepath(
            "/home/taha/Code/Cyrus/app/core/math_utils.cyrus",
            Some(Path::new("/home/taha/Code/Cyrus")),
            Some(Path::new("/home/taha/Code/Cyrus/stdlib")),
        );
        assert_eq!(name, "app_core_math_utils");
    }

    #[test]
    fn handles_weird_characters() {
        let name = make_module_name_from_filepath(
            "/weird-paths/foo-bar/baz@v1.cyrus",
            Some(Path::new("/weird-paths")),
            None,
        );
        assert_eq!(name, "foo_bar_baz_v1");
    }
}
