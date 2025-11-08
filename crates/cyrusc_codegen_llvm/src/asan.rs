use crate::OwnedModule;
use cyrusc_compiler::options::{CodeGenOptions, CodeGenSanitizer};
use inkwell::{module::FlagBehavior, passes::PassManager, values::BasicMetadataValueEnum};

pub(crate) fn enable_asan_for_owned_module(opts: &CodeGenOptions, owned_module: &OwnedModule) {
    let llvmmodule = owned_module.module.borrow();
    let context = &owned_module.context;

    for sanitizer in &opts.sanitizer {
        let name = match sanitizer {
            CodeGenSanitizer::Address => "SanitizeAddress",
            CodeGenSanitizer::Undefined => "SanitizeUndefined",
            CodeGenSanitizer::Thread => "SanitizeThread",
        };

        let flag_value = context.i32_type().const_int(1, false);
        let flag_metadata = owned_module
            .context
            .metadata_node(&[BasicMetadataValueEnum::IntValue(flag_value)]);

        llvmmodule.add_metadata_flag(name, FlagBehavior::Error, flag_metadata);
    }

    drop(llvmmodule);
}
