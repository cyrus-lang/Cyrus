use crate::OwnedModule;
use cyrusc_asan_wrapper::{SanitizerOptions, run_sanitizers};
use cyrusc_compiler::options::{CodeGenOptions, CodeGenSanitizer};
use inkwell::{module::FlagBehavior, targets::TargetMachine, values::BasicMetadataValueEnum};

pub(crate) fn enable_asan_for_owned_module(
    opts: &CodeGenOptions,
    owned_module: &OwnedModule,
    tm: &TargetMachine,
    opt_level: i32,
) {
    let llvmmodule = owned_module.module.borrow();
    let context = &owned_module.context;

    for sanitizer in &opts.sanitizer {
        let name = match sanitizer {
            CodeGenSanitizer::Address => "SanitizeAddress",
            CodeGenSanitizer::Memory => "SanitizeMemory",
            CodeGenSanitizer::Thread => "SanitizeThread",
            CodeGenSanitizer::HWAddress => "SanitizeHWAddress",
        };

        let flag_value = context.i32_type().const_int(1, false);
        let flag_metadata = owned_module
            .context
            .metadata_node(&[BasicMetadataValueEnum::IntValue(flag_value)]);

        llvmmodule.add_metadata_flag(name, FlagBehavior::Error, flag_metadata);
    }

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
