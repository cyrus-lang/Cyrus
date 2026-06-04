// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::OwnedModule;
use cyrusc_asan_wrapper::{SanitizerOptions, run_sanitizers};
use cyrusc_diagcentral::exit_with_msg;
use cyrusc_internal::compiler_options::{CompilerOption_Sanitizer, CompilerOptions};
use inkwell::{
    context::Context,
    module::{FlagBehavior, Module},
    targets::TargetMachine,
    values::BasicMetadataValueEnum,
};

pub(crate) fn enable_asan_for_owned_module(
    opts: &CompilerOptions,
    owned_module: &OwnedModule,
    tm: &TargetMachine,
    opt_level: i32,
) {
    let llvmmodule = owned_module.module.borrow();
    let context = &owned_module.context;

    add_asan_metadata_flags_to_module(opts, context, &llvmmodule);

    opts.sanitizer.iter().for_each(|sanitizer| {
        if matches!(sanitizer, CompilerOption_Sanitizer::HWAddress) && !is_hwasan_supported() {
            exit_with_msg!("HWASan not supported on this platform.".to_string());
        }

        if !is_sanitizer_support_in_linker(&opts.linker.as_ref().unwrap(), sanitizer) {
            exit_with_msg!(format!(
                "Sanitizer not supported for linker: '{}'.",
                opts.linker.as_ref().unwrap()
            ));
        }
    });

    let sanitizer_opts = SanitizerOptions {
        address_sanitize: opts.sanitizer.iter().any(|s| matches!(s, CompilerOption_Sanitizer::Address)),
        thread_sanitize: opts.sanitizer.iter().any(|s| matches!(s, CompilerOption_Sanitizer::Thread)),
        mem_sanitize: opts.sanitizer.iter().any(|s| matches!(s, CompilerOption_Sanitizer::Memory)),
        hwaddress_sanitize: opts.sanitizer.iter().any(|s| matches!(s, CompilerOption_Sanitizer::HWAddress)),
        recover: false,
        asan_use_after_scope: true,
        asan_use_after_return: false,
    };

    run_sanitizers(&llvmmodule, tm, opt_level, sanitizer_opts);

    drop(llvmmodule);
}

fn add_asan_metadata_flags_to_module<'ctx>(opts: &CompilerOptions, context: &'ctx Context, module: &Module<'ctx>) {
    for sanitizer in &opts.sanitizer {
        let name = match sanitizer {
            CompilerOption_Sanitizer::Address => "asan",
            CompilerOption_Sanitizer::Memory => "msan",
            CompilerOption_Sanitizer::Thread => "tsan",
            CompilerOption_Sanitizer::HWAddress => "hwasan",
        };

        if module.get_flag(name).is_some() {
            // skip if flag already exists
            continue;
        }

        let flag_value = context.i32_type().const_int(1, false);
        let flag_metadata = context.metadata_node(&[BasicMetadataValueEnum::IntValue(flag_value)]);

        module.add_metadata_flag(name, FlagBehavior::Error, flag_metadata);
    }
}

#[inline]
fn is_sanitizer_support_in_linker(linker: &str, sanitizer: &CompilerOption_Sanitizer) -> bool {
    match sanitizer {
        CompilerOption_Sanitizer::Memory => linker.contains("clang"),
        _ => true,
    }
}

#[inline]
fn is_hwasan_supported() -> bool {
    cfg!(target_arch = "aarch64")
}
