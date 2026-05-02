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

use crate::llvm::abi::callconv::LLVMCallConv;
use cyrusc_ast::{
    abi::{ExportKind, Inlining, Linkage, OptionalFlag, Prologue},
    modifiers::{FuncModifiers, GlobalVarModifiers},
};
use inkwell::{
    DLLStorageClass,
    attributes::{Attribute, AttributeLoc},
    context::Context,
    module::Linkage as LLVMLinkage,
    values::{FunctionValue, GlobalValue},
};

fn llvm_dll_storage_class(export: &ExportKind) -> DLLStorageClass {
    match export {
        ExportKind::DllImport => DLLStorageClass::Import,
        ExportKind::DllExport => DLLStorageClass::Export,
    }
}

fn llvm_linkage(linkage: &Linkage) -> LLVMLinkage {
    match linkage {
        Linkage::Extern(_) => LLVMLinkage::External,
        Linkage::Weak => LLVMLinkage::WeakAny,
        Linkage::LinkOnce => LLVMLinkage::LinkOnceAny,
    }
}

fn llvm_inline(inline: &Inlining) -> &'static str {
    match inline {
        Inlining::Inline => "inlinehint",
        Inlining::NoInline => "noinline",
        Inlining::AlwaysInline => "alwaysinline",
    }
}

fn llvm_prologue(prologue: &Prologue) -> Option<&'static str> {
    match prologue {
        Prologue::Naked => Some("naked"),
    }
}

fn apply_optional_flags<'ll>(llvmctx: &'ll Context, func: &FunctionValue<'ll>, flags: &[OptionalFlag]) {
    for flag in flags {
        let attr = match flag {
            OptionalFlag::NoReturn => {
                Some(llvmctx.create_enum_attribute(Attribute::get_named_enum_kind_id("noreturn"), 0))
            }
            OptionalFlag::NoUnwind => {
                Some(llvmctx.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 0))
            }
            OptionalFlag::Cold => Some(llvmctx.create_enum_attribute(Attribute::get_named_enum_kind_id("cold"), 0)),
            OptionalFlag::Hot => Some(llvmctx.create_enum_attribute(Attribute::get_named_enum_kind_id("hot"), 0)),
            OptionalFlag::OptSize => {
                Some(llvmctx.create_enum_attribute(Attribute::get_named_enum_kind_id("optsize"), 0))
            }
            OptionalFlag::OptNone => {
                Some(llvmctx.create_enum_attribute(Attribute::get_named_enum_kind_id("optnone"), 0))
            }
            OptionalFlag::NoSanitize(kind) => Some(llvmctx.create_string_attribute("no_sanitize", kind)),
        };

        if let Some(a) = attr {
            func.add_attribute(AttributeLoc::Function, a);
        }
    }
}

pub(crate) fn apply_global_var_modifiers<'ll>(global_value: &GlobalValue<'ll>, modifiers: &GlobalVarModifiers) {
    if let Some(export) = &modifiers.export {
        global_value.set_dll_storage_class(llvm_dll_storage_class(export));
    }

    if let Some(linkage) = &modifiers.linkage {
        global_value.set_linkage(llvm_linkage(linkage));
    }

    if modifiers.weak {
        global_value.set_unnamed_addr(true);
    }

    if let Some(section) = &modifiers.section {
        global_value.set_section(Some(section.0.as_str()));
    }
}

pub(crate) fn apply_inlining_func<'a>(llvmctx: &'a Context, llvm_func_value: &FunctionValue<'a>, inline: Inlining) {
    let attr_name = llvm_inline(&inline);
    let enum_kind_id = Attribute::get_named_enum_kind_id(attr_name);
    let enum_attr = llvmctx.create_enum_attribute(enum_kind_id, 0);
    llvm_func_value.add_attribute(AttributeLoc::Function, enum_attr);
}

pub(crate) fn apply_func_modifiers<'ll>(llvmctx: &'ll Context, func: &FunctionValue<'ll>, modifiers: &FuncModifiers) {
    if let Some(export) = &modifiers.export {
        func.as_global_value()
            .set_dll_storage_class(llvm_dll_storage_class(export));
    }

    if let Some(linkage) = &modifiers.linkage {
        func.set_linkage(llvm_linkage(linkage));
    }

    if let Some(inline) = &modifiers.inline {
        let attr_name = llvm_inline(inline);
        let enum_kind_id = Attribute::get_named_enum_kind_id(attr_name);
        let enum_attr = llvmctx.create_enum_attribute(enum_kind_id, 0);
        func.add_attribute(AttributeLoc::Function, enum_attr);
    }

    if let Some(prologue) = &modifiers.prologue {
        if let Some(attr_name) = llvm_prologue(prologue) {
            let attr = llvmctx.create_string_attribute(attr_name, "");
            func.add_attribute(AttributeLoc::Function, attr);
        }
    }

    if let Some(callconv) = &modifiers.callconv {
        let llvm_callconv = LLVMCallConv::from(callconv).as_u32();
        func.set_call_conventions(llvm_callconv);
    }

    for section in &modifiers.placement {
        func.set_section(Some(section.0.as_str()));
    }
    if let Some(section) = &modifiers.section {
        func.set_section(Some(section.0.as_str()));
    }

    apply_optional_flags(llvmctx, func, &modifiers.optional_flags);
}
