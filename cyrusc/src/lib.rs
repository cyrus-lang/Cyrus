use std::ffi::CStr;
use std::os::raw::c_char;
use ast::ast::*;
use compile_statements::compile_block_statements;
use llvm_sys::core::*;
use llvm_sys::*;
use compile_expressions::*;

pub mod compile_expressions;
mod compile_expressions_test;
mod compile_statements;

pub unsafe fn compile(node: Node, module_name: &str) -> *mut LLVMModule {
    let context = LLVMContextCreate();
    let module_id: *const c_char = module_name.as_ptr() as *const c_char;
    let module = LLVMModuleCreateWithNameInContext(module_id, context);
    let builder = LLVMCreateBuilderInContext(context);

    match node {
        Node::Program(program) => compile_block_statements(&program.body),
        Node::Statement(statement) => todo!(),
        Node::Expression(expression) => compile_expressions(expression),
    };

    // cleanup llvm objects
    // LLVMDisposeBuilder(builder);
    // LLVMDisposeModule(module);
    // LLVMContextDispose(context);

    return module;
}

pub unsafe fn print_llvm_module(ty: *mut LLVMModule) {
    let result: *mut i8 = LLVMPrintModuleToString(ty);
    let c_str = CStr::from_ptr(result);
    let value = c_str.to_str().unwrap().to_string();
    println!("{}", value);
}