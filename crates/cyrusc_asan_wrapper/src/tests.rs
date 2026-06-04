// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#[cfg(test)]
mod tests {
    use crate::{SanitizerOptions, run_sanitizers};
    use inkwell::OptimizationLevel;
    use inkwell::context::Context;
    use inkwell::targets::{InitializationConfig, Target, TargetTriple};

    #[test]
    fn test_run_sanitizers_on_simple_module() {
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        let target = Target::from_triple(&triple).expect("could not find target");
        let tm = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Default,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("could not create target machine");

        let context = Context::create();
        let module = context.create_module("asan_test_module");

        let i32_type = context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = module.add_function("foo", fn_type, None);
        let entry = context.append_basic_block(function, "entry");
        let builder = context.create_builder();
        builder.position_at_end(entry);
        builder.build_return(Some(&i32_type.const_int(42, false))).unwrap();

        let opts = SanitizerOptions {
            address_sanitize: true,
            thread_sanitize: false,
            mem_sanitize: false,
            hwaddress_sanitize: false,
            recover: true,
            asan_use_after_scope: true,
            asan_use_after_return: false,
        };

        run_sanitizers(&module, &tm, 2, opts);

        module
            .verify()
            .expect("module verification failed after sanitizer passes");
    }
}
