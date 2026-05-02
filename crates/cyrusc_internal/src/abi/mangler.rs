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

use cyrusc_ast::{
    abi::{CallConv, Linkage},
    modifiers::{FuncModifiers, GlobalVarModifiers},
};
use cyrusc_typed_ast::{
    stmts::{TypedFuncTypeVariadicParam, TypedTypeArg, TypedTypeArgs},
    types::{SemaType, TypeDeclID, TypedArrayCapacity},
};
use once_cell::sync::Lazy;

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
        format!("{}${}.{}", module_name, object_name, method_name)
    }

    fn global_var_name(&self, module_name: &str, var_name: &str) -> String {
        format!("{}${}", Self::sanitize(module_name), Self::sanitize(var_name))
    }

    fn vtable_name(&self, interface_name: &str, vtable_id: &str) -> String {
        format!("__vtable${}_{}", interface_name, vtable_id)
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

    fn vtable_name(&self, interface_name: &str, vtable_id: &str) -> String {
        format!("__vtable_{}_{}", interface_name, vtable_id)
    }
}

pub fn mangle_global_var(modifiers: &GlobalVarModifiers, module_name: &str, var_name: &str) -> String {
    match &modifiers.linkage {
        Some(linkage) => abi_mangler_from_linkage(linkage).global_var_name(module_name, var_name),
        None => CYRUS_ABI.global_var_name(module_name, var_name),
    }
}

pub fn mangle_func(modifiers: &FuncModifiers, module_name: &str, name: &str) -> String {
    match &modifiers.linkage {
        Some(linkage) => abi_mangler_from_linkage(linkage).func_name(module_name, name),
        None => CYRUS_ABI.func_name(module_name, name),
    }
}

pub fn mangle_monomorphized_func(
    modifiers: &FuncModifiers,
    module_name: &str,
    name: &str,
    type_args: &TypedTypeArgs,
) -> String {
    let base = mangle_func(modifiers, module_name, name);

    if type_args.is_empty() {
        return base;
    }

    let type_args = mangle_type_args(type_args);

    format!("{base}<{type_args}>")
}

pub fn mangle_monomorphized_method(
    module_name: &str,
    id: usize,
    method_name: &str,
    type_args: &TypedTypeArgs,
) -> String {
    let base = mangle_method(module_name, &id.to_string(), method_name);

    if type_args.is_empty() {
        return base;
    }

    let type_args = mangle_type_args(type_args);

    format!("{base}<{type_args}>")
}

fn mangle_type_args(type_args: &TypedTypeArgs) -> String {
    type_args
        .iter()
        .map(|type_arg| match type_arg {
            TypedTypeArg::Type(sema_type, _) => mangle_sema_type(sema_type),
            TypedTypeArg::Infer => unreachable!(),
        })
        .collect::<Vec<_>>()
        .join("_")
}

fn mangle_sema_type(sema_type: &SemaType) -> String {
    match sema_type {
        SemaType::Named(named_type) => {
            let decl_id = match named_type.type_decl_id {
                TypeDeclID::Struct(struct_decl_id) => format!("struct_{}", struct_decl_id.0),
                TypeDeclID::Enum(enum_decl_id) => format!("enum_{}", enum_decl_id.0),
                TypeDeclID::Union(union_decl_id) => format!("union_{}", union_decl_id.0),
                TypeDeclID::Interface(interface_decl_id) => format!("interface_{}", interface_decl_id.0),
                TypeDeclID::Typedef(_) => unreachable!(),
            };

            if named_type.type_args.is_empty() {
                format!("{decl_id}")
            } else {
                let type_args = mangle_type_args(&named_type.type_args);

                format!("{decl_id}<{type_args}>")
            }
        }
        SemaType::InterfaceObject(interface_object) => {
            mangle_sema_type(&SemaType::InterfaceObject(interface_object.clone()))
        }
        SemaType::Plain(plain_type) => {
            // Use the provided to_string() method for PlainType
            plain_type.to_string()
        }
        SemaType::Array(typed_array_type) => {
            let element = mangle_sema_type(&typed_array_type.element_type);

            let capacity = match &typed_array_type.capacity {
                TypedArrayCapacity::Fixed(expr) => expr.literal_const_int_value().unwrap().to_string(),
                TypedArrayCapacity::Dynamic => "".to_string(),
            };

            format!("{element}[{capacity}]")
        }
        SemaType::Const(inner_type) => {
            format!("const_{}", mangle_sema_type(inner_type))
        }
        SemaType::Pointer(inner_type) => {
            format!("ptr_{}", mangle_sema_type(inner_type))
        }
        SemaType::FuncType(func_type) => {
            let ret_type = mangle_sema_type(&func_type.ret_type);

            let params = func_type
                .params
                .list
                .iter()
                .map(mangle_sema_type)
                .collect::<Vec<_>>()
                .join("_");

            let variadic = func_type.params.variadic.clone().map(|variadic| match *variadic {
                TypedFuncTypeVariadicParam::UntypedCStyle => "cvar".to_string(),
                TypedFuncTypeVariadicParam::Typed(sema_type) => format!("var_{}", mangle_sema_type(&sema_type)),
            });

            if let Some(variadic) = variadic {
                format!("func_{ret_type}_{params}_{variadic}")
            } else {
                format!("func_{ret_type}_{params}")
            }
        }
        SemaType::Tuple(typed_tuple_type) => {
            let elements = typed_tuple_type
                .elements
                .iter()
                .map(mangle_sema_type)
                .collect::<Vec<_>>()
                .join("_");

            format!("tuple_{elements}")
        }

        SemaType::Err(_)
        | SemaType::Unresolved(_)
        | SemaType::SelfType(_)
        | SemaType::GenericParam(_)
        | SemaType::InferVar(_)
        | SemaType::Placeholder => {
            unreachable!(
                "SemaType variants like Err, Unresolved, SelfType, GenericParam, InferVar, Placeholder should not be directly mangled."
            )
        }
    }
}

pub fn mangle_method(module_name: &str, id: &str, name: &str) -> String {
    CYRUS_ABI.method_name(module_name, id, name)
}

pub fn abi_mangler_from_linkage(linkage: &Linkage) -> &'static dyn ABINameMangler {
    match linkage {
        Linkage::Extern(call_conv_opt) => match call_conv_opt {
            Some(call_conv) => match call_conv {
                CallConv::C
                | CallConv::Stdcall
                | CallConv::Fastcall
                | CallConv::Thiscall
                | CallConv::Vectorcall
                | CallConv::SysV64
                | CallConv::Win64
                | CallConv::System => &*C_ABI,

                CallConv::Naked | CallConv::Interrupt | CallConv::Fast | CallConv::Cold | CallConv::Aapcs => {
                    &*CYRUS_ABI
                }
            },
            None => &*C_ABI,
        },
        Linkage::Weak => &*CYRUS_ABI,
        Linkage::LinkOnce => &*CYRUS_ABI,
    }
}
