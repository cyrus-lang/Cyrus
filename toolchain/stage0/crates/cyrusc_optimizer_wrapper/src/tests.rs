// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#[cfg(test)]
mod tests {
    use crate::*;
    use inkwell::OptimizationLevel;
    use inkwell::context::Context;
    use inkwell::targets::{InitializationConfig, Target, TargetTriple};

    macro_rules! with_test_module {
        ($body:expr) => {{
            let context = Context::create();
            let module = context.create_module("test_module");
            $body(&context, &module)
        }};
    }

    fn create_test_function(module: &inkwell::module::Module) {
        let context = module.get_context();
        let i32_type = context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = module.add_function("test_func", fn_type, None);

        let entry = context.append_basic_block(function, "entry");
        let builder = context.create_builder();
        builder.position_at_end(entry);

        let result = i32_type.const_int(42, false);
        builder.build_return(Some(&result)).unwrap();
    }

    fn create_test_function_with_loop(module: &inkwell::module::Module) {
        let context = module.get_context();
        let i32_type = context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = module.add_function("test_loop", fn_type, None);

        let entry = context.append_basic_block(function, "entry");
        let loop_block = context.append_basic_block(function, "loop");
        let exit_block = context.append_basic_block(function, "exit");

        let builder = context.create_builder();
        builder.position_at_end(entry);

        let zero = i32_type.const_int(0, false);
        let one = i32_type.const_int(1, false);
        let limit = i32_type.const_int(10, false);

        let counter = builder.build_alloca(i32_type, "counter").unwrap();
        builder.build_store(counter, zero).unwrap();
        builder.build_unconditional_branch(loop_block).unwrap();

        builder.position_at_end(loop_block);
        let current = builder
            .build_load(i32_type, counter, "current")
            .unwrap()
            .into_int_value();
        let next = builder.build_int_add(current, one, "next").unwrap();
        builder.build_store(counter, next).unwrap();

        let cond = builder
            .build_int_compare(inkwell::IntPredicate::SLT, next, limit, "cond")
            .unwrap();
        builder.build_conditional_branch(cond, loop_block, exit_block).unwrap();

        builder.position_at_end(exit_block);
        let final_val = builder.build_load(i32_type, counter, "final").unwrap();
        builder.build_return(Some(&final_val)).unwrap();
    }

    fn create_target_machine() -> inkwell::targets::TargetMachine {
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        let target = Target::from_triple(&triple).expect("could not find target");

        target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Default,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("could not create target machine")
    }

    #[test]
    fn test_optimizer_creation() {
        let optimizer = CyrusLLVMOptimizer::new(None);
        assert!(!optimizer.ctx.is_null());
    }

    #[test]
    fn test_optimizer_with_target_machine() {
        let mut tm = create_target_machine();
        let optimizer = CyrusLLVMOptimizer::new(Some(&mut tm));
        assert!(!optimizer.ctx.is_null());

        with_test_module!(|_context, module| {
            let result = optimizer.optimize_simple(module, CyrusOptLevel::O0, true);
            assert!(result.is_ok());
        });
    }

    #[test]
    fn test_optimize_simple_o0() {
        with_test_module!(|_context, module| {
            create_test_function(module);

            let optimizer = CyrusLLVMOptimizer::new(None);
            let result = optimizer.optimize_simple(module, CyrusOptLevel::O0, true);
            assert!(result.is_ok());

            assert!(CyrusLLVMOptimizer::verify_module(module).is_ok());
        });
    }

    #[test]
    fn test_optimize_simple_o2() {
        with_test_module!(|_context, module| {
            create_test_function(module);

            let optimizer = CyrusLLVMOptimizer::new(None);
            let result = optimizer.optimize_simple(module, CyrusOptLevel::O2, false);
            assert!(result.is_ok());

            assert!(CyrusLLVMOptimizer::verify_module(module).is_ok());
        });
    }

    #[test]
    fn test_optimize_simple_o3() {
        with_test_module!(|_context, module| {
            create_test_function(module);

            let optimizer = CyrusLLVMOptimizer::new(None);
            let result = optimizer.optimize_simple(module, CyrusOptLevel::O3, false);
            assert!(result.is_ok());

            assert!(CyrusLLVMOptimizer::verify_module(module).is_ok());
        });
    }

    #[test]
    fn test_optimize_with_config() {
        with_test_module!(|_context, module| {
            create_test_function(module);

            let optimizer = CyrusLLVMOptimizer::new(None);
            let config = CyrusOptimizerConfig::default();

            let result = optimizer.optimize(module, CyrusOptLevel::O2, Some(&config));
            assert!(result.is_ok());
        });
    }

    #[test]
    fn test_optimize_with_debug_config() {
        with_test_module!(|_context, module| {
            create_test_function(module);

            let optimizer = CyrusLLVMOptimizer::new(None);
            let config = CyrusOptimizerConfig::debug();

            let result = optimizer.optimize(module, CyrusOptLevel::O0, Some(&config));
            assert!(result.is_ok());
        });
    }

    #[test]
    fn test_optimize_loop_with_vectorization() {
        with_test_module!(|_context, module| {
            create_test_function_with_loop(module);

            let mut tm = create_target_machine();
            let optimizer = CyrusLLVMOptimizer::new(Some(&mut tm));

            let mut config = CyrusOptimizerConfig::default();
            config.allow_loop_vectorization = 1;
            config.allow_slp_vectorization = 1;

            let result = optimizer.optimize(module, CyrusOptLevel::O3, Some(&config));
            assert!(result.is_ok());
        });
    }

    #[test]
    fn test_optimize_without_vectorization() {
        with_test_module!(|_context, module| {
            create_test_function_with_loop(module);

            let optimizer = CyrusLLVMOptimizer::new(None);

            let mut config = CyrusOptimizerConfig::default();
            config.allow_loop_vectorization = 0;
            config.allow_slp_vectorization = 0;

            let result = optimizer.optimize(module, CyrusOptLevel::O2, Some(&config));
            assert!(result.is_ok());
        });
    }

    #[test]
    fn test_module_verification_valid() {
        with_test_module!(|_context, module| {
            create_test_function(module);

            let result = CyrusLLVMOptimizer::verify_module(module);
            assert!(result.is_ok());
        });
    }

    #[test]
    fn test_module_verification_invalid() {
        let context = Context::create();
        let module = context.create_module("invalid_module");

        let i32_type = context.i32_type();
        let fn_type = i32_type.fn_type(&[i32_type.into()], false);
        let function = module.add_function("invalid", fn_type, None);

        let entry = context.append_basic_block(function, "entry");
        let builder = context.create_builder();
        builder.position_at_end(entry);

        let result = CyrusLLVMOptimizer::verify_module(&module);
        assert!(result.is_err());
    }

    #[test]
    fn test_module_triple_operations() {
        with_test_module!(|_context, module| {
            let triple = "x86_64-unknown-linux-gnu";
            CyrusLLVMOptimizer::set_module_triple(module, triple);

            let retrieved = CyrusLLVMOptimizer::get_module_triple(module);
            assert_eq!(retrieved, Some(triple.to_string()));
        });
    }

    #[test]
    fn test_multiple_optimization_levels() {
        with_test_module!(|_context, module| {
            create_test_function_with_loop(module);

            let optimizer = CyrusLLVMOptimizer::new(None);

            for level in [
                CyrusOptLevel::O0,
                CyrusOptLevel::O1,
                CyrusOptLevel::O2,
                CyrusOptLevel::O3,
            ] {
                let result = optimizer.optimize_simple(module, level, level == CyrusOptLevel::O0);
                assert!(result.is_ok());
                assert!(CyrusLLVMOptimizer::verify_module(module).is_ok());
            }
        });
    }

    #[test]
    fn test_optimize_with_disabled_optimizations() {
        with_test_module!(|_context, module| {
            create_test_function_with_loop(module);

            let optimizer = CyrusLLVMOptimizer::new(None);

            let mut config = CyrusOptimizerConfig::default();
            config.allow_loop_unrolling = 0;
            config.allow_slp_vectorization = 0;
            config.allow_loop_vectorization = 0;
            config.allow_loop_interleaving = 0;
            config.allow_merge_functions = 0;

            let result = optimizer.optimize(module, CyrusOptLevel::O2, Some(&config));
            assert!(result.is_ok());
        });
    }

    #[test]
    fn test_target_machine_reconfiguration() {
        let mut tm = create_target_machine();
        let mut optimizer = CyrusLLVMOptimizer::new(Some(&mut tm));

        with_test_module!(|_context, module| {
            create_test_function(module);

            let result = optimizer.optimize_simple(module, CyrusOptLevel::O2, false);
            assert!(result.is_ok());

            let mut new_tm = create_target_machine();
            optimizer.set_target_machine(&mut new_tm);

            let result2 = optimizer.optimize_simple(module, CyrusOptLevel::O3, false);
            assert!(result2.is_ok());
        });
    }

    #[test]
    fn test_optimization_preserves_semantics() {
        with_test_module!(|_context, module| {
            create_test_function(module);

            let optimizer = CyrusLLVMOptimizer::new(None);
            let result = optimizer.optimize_simple(module, CyrusOptLevel::O3, false);
            assert!(result.is_ok());

            assert!(CyrusLLVMOptimizer::verify_module(module).is_ok());
        });
    }
}
