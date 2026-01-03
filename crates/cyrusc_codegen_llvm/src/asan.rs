// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use crate::OwnedModule;
use cyrusc_asan_wrapper::{SanitizerOptions, run_sanitizers};
use cyrusc_compiler::options::{CodeGenOptions, CodeGenSanitizer};
use cyrusc_diagcentral::display_single_custom_diag;
use inkwell::{
    context::Context,
    module::{FlagBehavior, Module},
    targets::TargetMachine,
    values::BasicMetadataValueEnum,
};

pub(crate) fn enable_asan_for_owned_module(
    opts: &CodeGenOptions,
    owned_module: &OwnedModule,
    tm: &TargetMachine,
    opt_level: i32,
) {
    let llvmmodule = owned_module.module.borrow();
    let context = &owned_module.context;

    add_asan_metadata_flags_to_module(opts, context, &llvmmodule);

    opts.sanitizer.iter().for_each(|sanitizer| {
        if matches!(sanitizer, CodeGenSanitizer::HWAddress) && !is_hwasan_supported() {
            display_single_custom_diag!("HWASan not supported on this platform.".to_string());
        }

        if !is_sanitizer_support_in_linker(&opts.linker.as_ref().unwrap(), sanitizer) {
            display_single_custom_diag!(format!(
                "Sanitizer not supported for linker: '{}'.",
                opts.linker.as_ref().unwrap()
            ));
        }
    });

    let sanitizer_opts = SanitizerOptions {
        address_sanitize: opts.sanitizer.iter().any(|s| matches!(s, CodeGenSanitizer::Address)),
        thread_sanitize: opts.sanitizer.iter().any(|s| matches!(s, CodeGenSanitizer::Thread)),
        mem_sanitize: opts.sanitizer.iter().any(|s| matches!(s, CodeGenSanitizer::Memory)),
        hwaddress_sanitize: opts.sanitizer.iter().any(|s| matches!(s, CodeGenSanitizer::HWAddress)),
        recover: false,
        asan_use_after_scope: true,
        asan_use_after_return: false,
    };

    run_sanitizers(&llvmmodule, tm, opt_level, sanitizer_opts);

    drop(llvmmodule);
}

fn add_asan_metadata_flags_to_module<'ctx>(opts: &CodeGenOptions, context: &'ctx Context, module: &Module<'ctx>) {
    for sanitizer in &opts.sanitizer {
        let name = match sanitizer {
            CodeGenSanitizer::Address => "asan",
            CodeGenSanitizer::Memory => "msan",
            CodeGenSanitizer::Thread => "tsan",
            CodeGenSanitizer::HWAddress => "hwasan",
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

fn is_sanitizer_support_in_linker(linker: &str, sanitizer: &CodeGenSanitizer) -> bool {
    match sanitizer {
        CodeGenSanitizer::Memory => linker.contains("clang"),
        _ => true,
    }
}

fn is_hwasan_supported() -> bool {
    cfg!(target_arch = "aarch64")
}
