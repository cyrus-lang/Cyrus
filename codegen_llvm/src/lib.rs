use ast::ast::*;
use ast::token::{Span, Token, TokenKind};
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::llvm_sys::LLVMType;
use inkwell::llvm_sys::core::LLVMFunctionType;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::{AsTypeRef, FunctionType};
use opts::CodeGenLLVMOptions;
use utils::compile_time_errors::errors::*;
use utils::compiler_error;

mod common;
pub mod opts;
mod scope;
mod tests;

pub struct CodeGenLLVM<'ctx> {
    opts: CodeGenLLVMOptions,
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    program: ProgramTree,
    file_path: String,
    file_name: String,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn new(
        context: &'ctx Context,
        file_path: String,
        file_name: String,
        program: ProgramTree,
    ) -> Result<Self, LLVMString> {
        let module = context.create_module(&file_name);
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;

        Ok(CodeGenLLVM {
            opts: CodeGenLLVMOptions::default(),
            context,
            module,
            builder,
            execution_engine,
            program,
            file_path,
            file_name,
        })
    }

    pub fn new_context() -> Context {
        Context::create()
    }

    pub fn set_opts(&mut self, opts: CodeGenLLVMOptions) {
        self.opts = opts;
    }

    pub fn compile(&mut self) {
        self.compile_statements(self.program.body.clone());
    }

    pub fn execute(&mut self) {
        unimplemented!();
    }

    pub(crate) fn compile_statements(&mut self, stmts: Vec<Statement>) {
        for stmt in stmts {
            self.compile_statement(stmt.clone());
        }

        println!("{}", self.module.print_to_string())
    }

    pub(crate) fn compile_statement(&mut self, stmt: Statement) {
        match stmt {
            Statement::Variable(variable) => todo!(),
            Statement::Expression(expression) => todo!(),
            Statement::If(_) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::FuncDef(func_def) => todo!(),
            Statement::FuncDecl(func_decl) => self.compile_func_decl(func_decl),
            Statement::For(_) => todo!(),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Import(import) => todo!(),
            Statement::BlockStatement(block_statement) => todo!(),
            Statement::Break(location) => todo!(),
            Statement::Continue(location) => todo!(),
        }
    }

    pub(crate) fn compile_func_decl(&mut self, func_decl: FuncDecl) {
        let is_var_args = func_decl.params.variadic.is_some();
        let mut param_types: Vec<*mut LLVMType> = func_decl
            .params
            .list
            .iter()
            .map(|param| {
                if let Some(param_type_token) = &param.ty {
                    self.token_as_data_type(param_type_token.clone()).as_type_ref()
                } else {
                    // FIXME
                    // Move to central diagnostics crate
                    compiler_error!(
                        format!(
                            "Type annotation required for parameter '{}' in function '{}'.",
                            param.identifier.name.clone(),
                            func_decl.name
                        ),
                        self.file_path.clone()
                    );
                }
            })
            .collect();

        let return_type = self.token_as_data_type(
            func_decl
                .return_type
                .unwrap_or(Token {
                    kind: TokenKind::Void,
                    span: Span::default(),
                })
                .kind,
        );

        let fn_type = unsafe {
            FunctionType::new(LLVMFunctionType(
                return_type.as_type_ref(),
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                is_var_args as i32,
            ))
        };

        let func_linkage = self.vis_type_as_linkage(func_decl.vis_type);
        let func = self.module.add_function(&func_decl.name, fn_type, Some(func_linkage));
    }
}
