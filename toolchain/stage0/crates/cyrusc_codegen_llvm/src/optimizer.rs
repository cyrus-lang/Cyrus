// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_internal::compiler_options::CompilerOption_Optimize;
use cyrusc_optimizer_wrapper::{CyrusLLVMOptimizer, CyrusOptLevel, CyrusOptimizerConfig};
use inkwell::module::Module;

pub fn optimize_module_debug<'ctx>(
    module: &Module<'ctx>,
    target_machine: Option<&mut inkwell::targets::TargetMachine>,
) -> Result<(), String> {
    let optimizer = CyrusLLVMOptimizer::new(target_machine);
    let config = CyrusOptimizerConfig::debug();
    optimizer.optimize(module, CyrusOptLevel::O0, Some(&config))
}

pub fn optimize_module_release<'ctx>(
    module: &Module<'ctx>,
    opt_level: CompilerOption_Optimize,
    target_machine: Option<&mut inkwell::targets::TargetMachine>,
) -> Result<(), String> {
    let is_debug = opt_level == CompilerOption_Optimize::O0;
    let optimizer = CyrusLLVMOptimizer::new(target_machine);
    let level = match opt_level {
        CompilerOption_Optimize::O0 => CyrusOptLevel::O0,
        CompilerOption_Optimize::O1 => CyrusOptLevel::O1,
        CompilerOption_Optimize::O2 => CyrusOptLevel::O2,
        CompilerOption_Optimize::O3 => CyrusOptLevel::O3,
        CompilerOption_Optimize::Os => CyrusOptLevel::Os,
        CompilerOption_Optimize::Oz => CyrusOptLevel::Oz,
    };
    optimizer.optimize_simple(module, level, is_debug)
}
