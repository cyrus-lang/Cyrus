use ast::ast::*;
use ast::token::{Location, TokenKind};
use diag::*;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::AnyTypeEnum;
use inkwell::values::{AnyValueEnum, AsValueRef, FunctionValue, PointerValue};
use opts::CodeGenLLVMOptions;
use scope::{Scope, ScopeRef};
use std::cell::RefCell;
use std::process::exit;
use std::rc::Rc;

mod build;
mod diag;
mod exprs;
mod funcs;
mod linkage;
pub mod opts;
mod scope;
mod tests;
mod types;

pub struct CodeGenLLVM<'ctx> {
    #[allow(dead_code)]
    opts: CodeGenLLVMOptions,
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    target_machine: TargetMachine,
    program: ProgramTree,
    file_path: String,
    reporter: DiagReporter,
    entry_point: Option<FunctionValue<'ctx>>,
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

        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let target_triple = TargetMachine::get_default_triple();
        let cpu = TargetMachine::get_host_cpu_name().to_string();
        let features = TargetMachine::get_host_cpu_features().to_string();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                &cpu,
                &features,
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();
        module.set_triple(&target_triple);
        module.set_data_layout(&target_machine.get_target_data().get_data_layout());

        Ok(CodeGenLLVM {
            opts,
            context,
            module,
            builder,
            program,
            file_path,
            reporter,
            target_machine,
            entry_point: None,
        })
    }

    pub fn new_context() -> Context {
        Context::create()
    }

    pub fn compile(&mut self) {
        let scope: ScopeRef = Rc::new(RefCell::new(Scope::new()));
        self.build_statements(Rc::clone(&scope), self.program.body.clone());

        if self.reporter.has_errors() {
            self.reporter.display_diags();
            exit(1);
        }

        self.optimize();
        self.build_entry_point();
    }

    pub(crate) fn build_statements(&mut self, scope: ScopeRef, stmts: Vec<Statement>) {
        for stmt in stmts {
            self.build_statement(Rc::clone(&scope), stmt.clone());
        }
    }

    pub(crate) fn build_statement(&mut self, scope: ScopeRef, stmt: Statement) {
        match stmt {
            Statement::BlockStatement(block_statement) => {
                self.build_statements(Rc::clone(&scope), block_statement.exprs);
            }
            Statement::Expression(expression) => {
                self.build_expr(Rc::clone(&scope), expression);
            }
            Statement::Variable(variable) => self.build_variable(Rc::clone(&scope), variable),
            Statement::If(_) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::FuncDef(func_def) => {
                self.build_func_def(func_def.clone());
            }
            Statement::FuncDecl(func_decl) => {
                self.build_func_decl(func_decl);
            }
            Statement::For(_) => todo!(),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Break(location) => todo!(),
            Statement::Continue(location) => todo!(),
            Statement::Import(import) => todo!(),
            Statement::Enum(_) => todo!(),
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
            AnyTypeEnum::IntType(int_type) => self.builder.build_alloca(int_type, &var_name),
            AnyTypeEnum::FloatType(float_type) => self.builder.build_alloca(float_type, &var_name),
            AnyTypeEnum::PointerType(pointer_type) => self.builder.build_alloca(pointer_type, &var_name),
            AnyTypeEnum::StructType(struct_type) => todo!(),
            AnyTypeEnum::VectorType(vector_type) => todo!(),
            AnyTypeEnum::ArrayType(array_type) => self.builder.build_alloca(array_type, &var_name),
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

    pub(crate) fn build_store(&self, ptr: PointerValue, value: AnyValueEnum) {
        let result = match value {
            AnyValueEnum::IntValue(int_value) => self.builder.build_store(ptr, int_value),
            AnyValueEnum::FloatValue(float_value) => self.builder.build_store(ptr, float_value),
            AnyValueEnum::ArrayValue(array_value) => self.builder.build_store(ptr, array_value),
            AnyValueEnum::PointerValue(pointer_value) => self.builder.build_store(ptr, pointer_value),
            AnyValueEnum::VectorValue(vector_value) => self.builder.build_store(ptr, vector_value),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Cannot store non-basic value in pointer.")),
                    location: None,
                });
                exit(1);
            }
        };

        if let Err(err) = result {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!("Cannot store value in pointer:\n{}", err.to_string())),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn build_variable(&self, scope: ScopeRef, variable: Variable) {
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

        if let Some(expr) = variable.expr {
            let value = self.build_expr(Rc::clone(&scope), expr);
            self.build_store(ptr, value);
        }

        scope.borrow_mut().insert(
            variable.name,
            scope::ScopeRecord {
                ptr: ptr.as_value_ref(),
            },
        );
    }
}
