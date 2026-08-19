// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::llvm::abi::callconv::LLVMCallConv;
use cyrusc_ast::{
    abi::{Inlining, OptionalFlag, Prologue},
    modifiers::{FuncModifiers, GlobalVarModifiers},
};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    context::Context,
    module::Linkage as LLVMLinkage,
    values::{FunctionValue, GlobalValue},
};

fn llvm_inline(inline: &Inlining) -> &'static str {
    match inline {
        Inlining::Hint => "inlinehint",
        Inlining::Never => "noinline",
        Inlining::Always => "alwaysinline",
    }
}

fn llvm_prologue(prologue: &Prologue) -> &'static str {
    match prologue {
        Prologue::Naked => "naked",
    }
}

fn apply_optional_flags<'ll>(llvm_ctx: &'ll Context, func: &FunctionValue<'ll>, flags: &[OptionalFlag]) {
    for flag in flags {
        let attr = match flag {
            OptionalFlag::NoReturn => {
                Some(llvm_ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("noreturn"), 0))
            }
            OptionalFlag::NoUnwind => {
                Some(llvm_ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 0))
            }
            OptionalFlag::Cold => Some(llvm_ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("cold"), 0)),
            OptionalFlag::Hot => Some(llvm_ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("hot"), 0)),
            OptionalFlag::OptSize => {
                Some(llvm_ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("optsize"), 0))
            }
            OptionalFlag::OptNone => {
                Some(llvm_ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("optnone"), 0))
            }
            OptionalFlag::NoSanitize(kind) => Some(llvm_ctx.create_string_attribute("no_sanitize", kind)),
        };

        if let Some(a) = attr {
            func.add_attribute(AttributeLoc::Function, a);
        }
    }
}

pub(crate) fn apply_global_var_modifiers<'ll>(global_value: &GlobalValue<'ll>, modifiers: &GlobalVarModifiers) {
    if modifiers.extrn.is_some() {
        global_value.set_linkage(LLVMLinkage::External);
    }

    if modifiers.weak {
        global_value.set_linkage(LLVMLinkage::ExternalWeak);
    }

    if modifiers.link_once {
        global_value.set_linkage(LLVMLinkage::LinkOnceODR);
    }

    if modifiers.thread_local {
        global_value.set_thread_local(true);
    }

    if let Some(section) = &modifiers.section {
        global_value.set_section(Some(section.as_str()));
    }
}

pub(crate) fn apply_func_modifiers<'ll>(llvm_ctx: &'ll Context, func: &FunctionValue<'ll>, modifiers: &FuncModifiers) {
    if modifiers.extrn.is_some() {
        func.set_linkage(LLVMLinkage::External);
    }

    if modifiers.weak {
        func.set_linkage(LLVMLinkage::ExternalWeak);
    }

    if modifiers.link_once {
        func.set_linkage(LLVMLinkage::LinkOnceODR);
    }

    if let Some(inline) = &modifiers.inline {
        let attr_name = llvm_inline(inline);
        let enum_kind_id = Attribute::get_named_enum_kind_id(attr_name);
        let enum_attr = llvm_ctx.create_enum_attribute(enum_kind_id, 0);
        func.add_attribute(AttributeLoc::Function, enum_attr);
    }

    if let Some(prologue) = &modifiers.prologue {
        let attr_name = llvm_prologue(prologue);

        let attr = llvm_ctx.create_string_attribute(attr_name, "");
        func.add_attribute(AttributeLoc::Function, attr);

        // Naked function shall have `frame-pointer=none` to support all targets properly.
        // Check this out:
        // https://github.com/llvm/llvm-project/pull/106014
        let attr = llvm_ctx.create_string_attribute("frame-pointer", "none");
        func.add_attribute(AttributeLoc::Function, attr);
    }

    if let Some(callconv) = &modifiers.callconv {
        let llvm_callconv = LLVMCallConv::from(callconv).as_u32();
        func.set_call_conventions(llvm_callconv);
    }

    if let Some(section) = &modifiers.section {
        func.set_section(Some(section.as_str()));
    }

    apply_optional_flags(llvm_ctx, func, &modifiers.optional_flags);
}
