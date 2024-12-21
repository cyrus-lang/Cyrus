use ast::ast::*;
use execution_engine::{LLVMCreateExecutionEngineForModule, LLVMGetFunctionAddress};
use llvm_sys::core::*;
use llvm_sys::*;
use prelude::*;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::sync::Mutex;
use std::{collections::HashMap, ffi::CString};
use target::{
    LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeTarget,
};

mod compile_builtins;
pub mod compile_expressions;
mod compile_expressions_test;
mod compile_statements;
mod compiler_errors;

#[derive(Debug)]
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
    LLVM_InitializeNativeTarget();
    LLVM_InitializeNativeAsmPrinter();
    LLVM_InitializeNativeAsmParser();

    let mut engine = std::ptr::null_mut();
    let mut error = std::ptr::null_mut();
    if LLVMCreateExecutionEngineForModule(&mut engine, module, &mut error) != 0 {
        compiler_error!("bootstrap execution engine failed");
    }

    let main_func_name = CString::new("main").unwrap();
    let main_func = LLVMGetNamedFunction(module, main_func_name.as_ptr());

    if !main_func.is_null() {
        let main_return_value: u64;

        let main_func_ptr = LLVMGetFunctionAddress(engine, main_func_name.as_ptr());

        if main_func_ptr == 0 {
            compiler_error!("could not get function address for main");
        }

        let main_typed_ptr = std::mem::transmute::<
            u64,
            extern "C" fn(i32, *const *const c_char) -> i32,
        >(main_func_ptr);

        main_return_value = main_typed_ptr(0, Vec::new().as_ptr()) as u64;

        std::process::exit(main_return_value as i32);
    }
}
