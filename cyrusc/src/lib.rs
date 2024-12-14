use ast::ast::*;
use llvm_sys::core::*;
use llvm_sys::*;
use prelude::*;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::sync::{LazyLock, Mutex};

pub mod compile_expressions;
mod compile_expressions_test;
mod compile_statements;

type CompileErr = &'static str;
type CompileResult<T> = Result<T, CompileErr>;

static NAMED_VALUES: LazyLock<Mutex<Vec<(&'static str, i32)>>> = LazyLock::new(|| Mutex::new(vec![]));

pub struct Compiler {
    pub builder: LLVMBuilderRef,
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef
}

pub unsafe fn compile(node: Node, module_name: &str) -> CompileResult<(LLVMModuleRef, LLVMValueRef)> {
    let context = LLVMContextCreate();
    let module_id: *const c_char = module_name.as_ptr() as *const c_char;
    let module = LLVMModuleCreateWithNameInContext(module_id, context);
    let builder = LLVMCreateBuilderInContext(context);

    // FIXME - Just for testing 
    let int_type = LLVMInt32Type();
    let function_type = LLVMFunctionType(int_type, std::ptr::null_mut(), 0, 0);
    let function = LLVMAddFunction(module, CString::new("main").unwrap().as_ptr(), function_type);
    let basic_block = LLVMAppendBasicBlockInContext(context, function, CString::new("entry").unwrap().as_ptr());
    LLVMPositionBuilderAtEnd(builder, basic_block);

    let mut compiler = Compiler { builder, context, module };

    let result: LLVMValueRef = match node {
        Node::Program(program) => compiler.compile_block_statements(program.body)?,
        Node::Statement(statement) => todo!(),
        Node::Expression(expression) => compiler.compile_expressions(expression)?,
    };

    // cleanup llvm objects
    // LLVMDisposeBuilder(builder);
    // LLVMDisposeModule(module);
    // LLVMContextDispose(context);

    Ok((module, result))
}

pub unsafe fn print_llvm_module(ty: *mut LLVMModule) {
    let result: *mut i8 = LLVMPrintModuleToString(ty);
    let c_str = CStr::from_ptr(result);
    let value = c_str.to_str().unwrap().to_string();
    println!("{}", value);
}
