use alloc_table::AllocTable;
use analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
use ast::ast::*;
use execution_engine::*;
use llvm_sys::core::*;
use llvm_sys::*;
use prelude::*;
use std::sync::Mutex;
use std::{collections::HashMap, ffi::CString};
use target::*;
use target_machine::{
    LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetDataLayout, LLVMCreateTargetMachine,
    LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetRef,
};
use types::cstr;

mod alloc_table;
mod builtins;
mod codegen;
mod errors;
mod types;
mod tests;

pub struct Compiler {
    pub builder: LLVMBuilderRef,
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub alloc_table: Mutex<HashMap<String, AllocTable>>,
}

pub fn compile(node: Node, module_name: &str) -> (LLVMModuleRef, Option<LLVMValueRef>) {
    let context = unsafe { LLVMContextCreate() };
    let module_id = module_name.as_ptr() as *const i8;
    let module = unsafe { LLVMModuleCreateWithNameInContext(module_id, context) };

    let builder = unsafe { LLVMCreateBuilderInContext(context) };
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

pub fn verify_module(module: LLVMModuleRef) {
    let mut error_message: *mut i8 = std::ptr::null_mut();
    let verification_result = unsafe {
        LLVMVerifyModule(
            module,
            LLVMVerifierFailureAction::LLVMAbortProcessAction,
            &mut error_message,
        )
    };

    if verification_result != 0 {
        if !error_message.is_null() {
            let error_str = unsafe { CString::from_raw(error_message).into_string().unwrap() };
            compiler_error!(format!("build program failed:\n{}", error_str));
        } else {
            compiler_error!("build program failed with no error message provided.");
        }
    }
}

pub fn print_llvm_module(ty: *mut LLVMModule) {
    let module_str = unsafe { CString::from_raw(LLVMPrintModuleToString(ty)) };
    print!("{}", module_str.into_string().unwrap());
}

pub fn compile_native(module: LLVMModuleRef) {
    unsafe {
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmParsers();
        LLVM_InitializeAllAsmPrinters();
    };

    let mut engine = std::ptr::null_mut();
    let mut error = std::ptr::null_mut();
    if unsafe { LLVMCreateExecutionEngineForModule(&mut engine, module, &mut error) } != 0 {
        compiler_error!("Initializing execution engine failed.");
    }

    let target_triple = unsafe { LLVMGetDefaultTargetTriple() };
    unsafe { LLVMSetTarget(module, target_triple) };
    let target: *mut LLVMTargetRef = Box::into_raw(Box::new(std::ptr::null_mut()));
    let err_msg: *mut *mut i8 = Box::into_raw(Box::new(std::ptr::null_mut()));
    if unsafe { LLVMGetTargetFromTriple(target_triple, target, err_msg) } != 0 {
        let err = unsafe { CString::from_raw(*err_msg.as_ref().unwrap()) };
        compiler_error!(format!(
            "Retrieving LLVM target failed: {}",
            err.to_str().unwrap()
        ));
    }
    unsafe { LLVMDisposeMessage(*err_msg.as_ref().unwrap()) };

    let target_machine = unsafe {
        LLVMCreateTargetMachine(
            *target,
            target_triple,
            cstr("generic"),
            cstr(""),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            LLVMRelocMode::LLVMRelocDefault,
            LLVMCodeModel::LLVMCodeModelDefault,
        )
    };
    let target_data = unsafe { LLVMCreateTargetDataLayout(target_machine) };
    unsafe { LLVMSetModuleDataLayout(module, target_data) };

    // verify_module(module);
    execute_main_func(module, engine);
}

pub fn execute_main_func(module: LLVMModuleRef, engine:*mut LLVMOpaqueExecutionEngine) {
    let main_func_name = CString::new("main").unwrap();
    let main_func = unsafe { LLVMGetNamedFunction(module, main_func_name.as_ptr() as *const i8) };

    if !main_func.is_null() {
        let main_return_value: u64;

        let main_func_ptr = unsafe { LLVMGetFunctionAddress(engine, main_func_name.as_ptr()) };
        if main_func_ptr == 0 {
            compiler_error!("could not get function address for main");
        }

        let main_typed_ptr = unsafe {
            std::mem::transmute::<u64, extern "C" fn(i32, *const *const i8) -> i32>(main_func_ptr)
        };

        main_return_value = main_typed_ptr(0, Vec::new().as_ptr()) as u64;

        std::process::exit(main_return_value as i32);
    }
}
