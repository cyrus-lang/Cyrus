use clap::*;
use gccjit_sys::{gcc_jit_context, gcc_jit_context_set_int_option};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum OptimizationLevel {
    None,
    O1,
    O2,
    O3,
}

impl OptimizationLevel {
    pub fn to_gccjit(&self, context: *mut gcc_jit_context) {
        let value = match self {
            OptimizationLevel::None => 0,
            OptimizationLevel::O1 => 1,
            OptimizationLevel::O2 => 2,
            OptimizationLevel::O3 => 3,
        };

        unsafe {
            gcc_jit_context_set_int_option(
                context,
                gccjit_sys::gcc_jit_int_option::GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
                value,
            )
        }
    }
}

#[derive(Parser, Debug, Clone)]
pub struct CompilerOptions {
    #[clap(long, value_enum, default_value_t = OptimizationLevel::None, help = "Set optimization level")]
    optimization_level: OptimizationLevel,

    #[clap(long, value_name = "PATH", help = "Add a library search path")]
    library_path: Vec<String>,

    #[clap(long = "library", value_name = "LIB", help = "Link a library")]
    libraries: Vec<String>,
}
