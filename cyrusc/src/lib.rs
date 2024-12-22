use analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
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
use target_machine::{
    LLVMCodeModel, LLVMCreateTargetMachine, LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple,
    LLVMRelocMode,
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
    let module_id = module_name.as_ptr() as *const i8;
    let module = LLVMModuleCreateWithNameInContext(module_id, context);

    let data_layout = CString::new("e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128").unwrap();
    LLVMSetDataLayout(module, data_layout.as_ptr());

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
        Node::Statement(statement) => compiler.compile_statement(statement),
        Node::Expression(expression) => compiler.compile_expressions(expression),
    };

    (module, result)
}

pub unsafe fn verify_module(module: LLVMModuleRef) {
    let mut error_message: *mut i8 = std::ptr::null_mut();
    let verification_result = LLVMVerifyModule(
        module,
        LLVMVerifierFailureAction::LLVMAbortProcessAction,
        &mut error_message,
    );

    if verification_result != 0 {
        if !error_message.is_null() {
            let error_str = CString::from_raw(error_message).into_string().unwrap();
            compiler_error!(format!("build program failed:\n{}", error_str));
        } else {
            compiler_error!("build program failed with no error message provided.");
        }
    }
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
    let main_func = LLVMGetNamedFunction(module, main_func_name.as_ptr() as *const i8);

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
