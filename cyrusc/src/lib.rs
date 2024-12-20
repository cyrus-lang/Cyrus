use ast::ast::*;
use execution_engine::{LLVMCreateExecutionEngineForModule, LLVMExecutionEngineRef};
use llvm_sys::core::*;
use llvm_sys::*;
use prelude::*;
use target::{LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeTarget};
use std::collections::HashMap;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::sync::Mutex;

pub mod compile_expressions;
mod compile_expressions_test;
mod compile_statements;
mod compiler_errors;

pub struct AllocTable {
    pub alloc: LLVMValueRef,
    pub ty: LLVMTypeRef,
}

pub struct Compiler {
    pub builder: LLVMBuilderRef,
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub alloc_table: Mutex<HashMap<String, AllocTable>>,
}

pub unsafe fn compile(node: Node, module_name: &str) -> (LLVMModuleRef, Option<LLVMValueRef>) {
    let context = LLVMContextCreate();
    let module_id: *const c_char = module_name.as_ptr() as *const c_char;
    let module = LLVMModuleCreateWithNameInContext(module_id, context);
    let builder = LLVMCreateBuilderInContext(context);
    let alloc_table = Mutex::new(HashMap::new());

    let mut compiler = Compiler {
        builder,
        context,
        module,
        alloc_table,
    };

    let result: Option<LLVMValueRef> = match node {
        Node::Program(program) => compiler.compile_block_statements(program.body),
        Node::Statement(statement) => todo!(),
        Node::Expression(expression) => compiler.compile_expressions(expression),
    };

    (module, result)
}

pub unsafe fn print_llvm_module(ty: *mut LLVMModule) {
    let result: *mut i8 = LLVMPrintModuleToString(ty);
    let c_str = CStr::from_ptr(result);
    let value = c_str.to_str().unwrap().to_string();
    println!("{}", value);
}

pub unsafe fn compile_native(module: LLVMModuleRef) {
    if LLVM_InitializeNativeTarget() != 0 || LLVM_InitializeNativeAsmPrinter() != 0 {
        compiler_error!("failed to initialize native target");
    }

    let mut engine: LLVMExecutionEngineRef = std::ptr::null_mut();
    if LLVMCreateExecutionEngineForModule(&mut engine, module, 0 as *mut *mut i8) != 0 {
        compiler_error!("failed to create execution engine");
    }    
}
