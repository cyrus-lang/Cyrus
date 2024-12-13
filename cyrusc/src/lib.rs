use ast::ast::*;
use llvm_sys::core::*;
use llvm_sys::*;
use prelude::*;
use std::ffi::CStr;
use std::os::raw::c_char;

pub mod compile_expressions;
mod compile_expressions_test;
mod compile_statements;

type CompileErr = &'static str;
type CompileResult<T> = Result<T, CompileErr>;

pub struct Compiler {
    pub builder: LLVMBuilderRef,
    pub context: LLVMContextRef
}

pub unsafe fn compile(node: Node, module_name: &str) -> CompileResult<(LLVMModuleRef, LLVMValueRef)> {
    let context = LLVMContextCreate();
    let module_id: *const c_char = module_name.as_ptr() as *const c_char;
    let module = LLVMModuleCreateWithNameInContext(module_id, context);
    let builder = LLVMCreateBuilderInContext(context);

    let mut compiler = Compiler { builder, context };

    let result: LLVMValueRef = match node {
        Node::Program(program) => compiler.compile_block_statements(program.body),
        Node::Statement(statement) => todo!(),
        Node::Expression(expression) => compiler.compile_expressions(expression),
    }.unwrap();

    // cleanup llvm objects
    LLVMDisposeBuilder(builder);
    LLVMDisposeModule(module);
    LLVMContextDispose(context);

    Ok((module, result))
}

pub unsafe fn print_llvm_module(ty: *mut LLVMModule) {
    let result: *mut i8 = LLVMPrintModuleToString(ty);
    let c_str = CStr::from_ptr(result);
    let value = c_str.to_str().unwrap().to_string();
    println!("{}", value);
}
