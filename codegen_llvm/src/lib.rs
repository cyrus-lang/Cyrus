use ast::ast::*;
use ast::token::{Location, TokenKind};
use diag::*;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::values::{AnyValueEnum, PointerValue};
use opts::CodeGenLLVMOptions;
use std::process::exit;

mod build;
mod diag;
mod funcs;
mod linkage;
pub mod opts;
mod scope;
mod tests;
mod exprs;
mod types;

pub struct CodeGenLLVM<'ctx> {
    #[allow(dead_code)]
    opts: CodeGenLLVMOptions,
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    target_machine: TargetMachine,
    program: ProgramTree,
    file_path: String,
    reporter: DiagReporter,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn new(
        context: &'ctx Context,
        file_path: String,
        file_name: String,
        program: ProgramTree,
        opts: CodeGenLLVMOptions,
    ) -> Result<Self, LLVMString> {
        let reporter = DiagReporter::new();
        let module = context.create_module(&file_name);
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;

        // FIXME
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::None,
                inkwell::targets::RelocMode::PIC,
                inkwell::targets::CodeModel::Default,
            )
            .unwrap();

        Ok(CodeGenLLVM {
            opts,
            context,
            module,
            builder,
            execution_engine,
            program,
            file_path,
            reporter,
            target_machine,
        })
    }

    pub fn new_context() -> Context {
        Context::create()
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
            Statement::Variable(variable) => self.compile_variable(variable),
            Statement::Expression(expression) => {
                self.build_expr(expression);
            },
            Statement::If(_) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::FuncDef(func_def) => {
                self.build_func_def(func_def);
            }
            Statement::FuncDecl(func_decl) => {
                self.build_func_decl(func_decl);
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

    pub(crate) fn build_alloca(
        &self,
        var_type_token: TokenKind,
        var_name: String,
        loc: Location,
        span_end: usize,
    ) -> PointerValue {
        let any_type = self.build_type(var_type_token, loc.clone(), span_end);
        let ptr_value = match any_type {
            inkwell::types::AnyTypeEnum::IntType(int_type) => self.builder.build_alloca(int_type, &var_name),
            inkwell::types::AnyTypeEnum::FloatType(float_type) => self.builder.build_alloca(float_type, &var_name),
            inkwell::types::AnyTypeEnum::PointerType(pointer_type) => {
                self.builder.build_alloca(pointer_type, &var_name)
            }
            inkwell::types::AnyTypeEnum::StructType(struct_type) => todo!(),
            inkwell::types::AnyTypeEnum::VectorType(vector_type) => todo!(),
            inkwell::types::AnyTypeEnum::ArrayType(array_type) => todo!(),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot allocate memory for non-basic type.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        };
        match ptr_value {
            Ok(ptr_value) => return ptr_value,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Failed to allocate memory for value:\n{}", err.to_string())),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        }
    }

    pub(crate) fn build_store(
        &self,
        ptr: PointerValue,
        value: AnyValueEnum,
    ) {
        match value {
            AnyValueEnum::IntValue(int_value) => {
                self.builder.build_store(ptr, int_value);
            },
            AnyValueEnum::FloatValue(float_value) => {
                self.builder.build_store(ptr, float_value);
            },
            AnyValueEnum::PointerValue(pointer_value) => todo!(),
            AnyValueEnum::VectorValue(vector_value) => todo!(),
            AnyValueEnum::ArrayValue(array_value) => todo!(),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Cannot store non-basic value in pointer.")),
                    location: None,
                });
                exit(1);
            }
        };
    }

    pub(crate) fn compile_variable(&self, variable: Variable) {
        let var_type_token = match variable.ty {
            Some(ty) => ty,
            None => {
                todo!();
            }
        };

        let ptr = self.build_alloca(
            var_type_token,
            variable.name.clone(),
            variable.loc.clone(),
            variable.span.end,
        );

        if let Some(expr)  = variable.expr {
            let value = self.build_expr(expr);
            self.build_store(ptr, value);
        }
    }
}
