use std::process::exit;

use ast::ast::*;
use diag::DiagReporter;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Linkage, Module};
use inkwell::support::LLVMString;
use opts::CodeGenLLVMOptions;

mod funcs;
pub mod opts;
mod scope;
mod tests;
mod types;
mod build;
mod diag;

pub struct CodeGenLLVM<'ctx> {
    opts: CodeGenLLVMOptions,
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    program: ProgramTree,
    file_path: String,
    file_name: String,
    reporter: DiagReporter,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn new(
        context: &'ctx Context,
        file_path: String,
        file_name: String,
        program: ProgramTree,
    ) -> Result<Self, LLVMString> {
        let reporter = DiagReporter::new();
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
            reporter,
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

        if self.reporter.has_errors() {
            self.reporter.display_diags();
            exit(1);
        }
    }

    pub(crate) fn compile_statements(&mut self, stmts: Vec<Statement>) {
        for stmt in stmts {
            self.compile_statement(stmt.clone());
        }
    }

    pub(crate) fn compile_statement(&mut self, stmt: Statement) {
        match stmt {
            Statement::Variable(variable) => todo!(),
            Statement::Expression(expression) => todo!(),
            Statement::If(_) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::FuncDef(func_def) => {
                self.compile_func_def(func_def);
            }
            Statement::FuncDecl(func_decl) => {
                self.compile_func_decl(func_decl);
            }
            Statement::For(_) => todo!(),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Import(import) => todo!(),
            Statement::BlockStatement(block_statement) => todo!(),
            Statement::Break(location) => todo!(),
            Statement::Continue(location) => todo!(),
        }
    }

    pub(crate) fn build_linkage(&self, vis_type: VisType) -> Linkage {
        match vis_type {
            VisType::Extern => Linkage::External,
            VisType::Pub => Linkage::AvailableExternally,
            VisType::Internal => Linkage::Private,
            VisType::Inline => todo!(),
        }
    }
}
