use ast::ast::*;
use ast::token::{Location, TokenKind};
use diag::*;
use funcs::FuncTable;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::llvm_sys::prelude::LLVMValueRef;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{AnyTypeEnum, AsTypeRef, BasicTypeEnum};
use inkwell::values::{AnyValueEnum, AsValueRef, FunctionValue, PointerValue};
use opts::CodeGenLLVMOptions;
use scope::{Scope, ScopeRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::process::exit;
use std::rc::Rc;

mod build;
mod diag;
mod exprs;
mod funcs;
mod linkage;
pub mod opts;
mod runtime;
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
    entry_point: Option<FuncDef>,
    func_table: FuncTable<'ctx>,
    internal_funcs_table: HashMap<String, LLVMValueRef>,
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

        let mut codegen_llvm = CodeGenLLVM {
            opts,
            context,
            module,
            builder,
            program,
            file_path,
            reporter,
            target_machine,
            entry_point: None,
            func_table: FuncTable::new(),
            internal_funcs_table: HashMap::new(),
        };

        codegen_llvm.load_runtime();
        Ok(codegen_llvm)
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
            Statement::Return(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Cannot build return statement outside of a function.")),
                    location: None,
                });
                exit(1);
            }
            Statement::FuncDef(func_def) => {
                if func_def.name == "main" {
                    if self.entry_point.is_some() {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(String::from("Multiple entry point not allowed.")),
                            location: None,
                        });
                        exit(1);
                    }

                    self.entry_point = Some(func_def);
                } else {
                    self.build_func_def(func_def.clone());
                }
            }
            Statement::FuncDecl(func_decl) => {
                self.build_func_decl(func_decl);
            }
            Statement::If(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::Switch(_) => todo!(),
            Statement::Break(location) => todo!(),
            Statement::Continue(location) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Enum(_) => todo!(),
            Statement::Import(import) => todo!(),
        }
    }

    pub(crate) fn build_alloca(
        &self,
        var_type_token: TokenKind,
        var_name: String,
        loc: Location,
        span_end: usize,
    ) -> (PointerValue, BasicTypeEnum) {
        let any_type = self.build_type(var_type_token, loc.clone(), span_end);
        match any_type {
            AnyTypeEnum::StructType(struct_type) => todo!(),
            AnyTypeEnum::VectorType(vector_type) => todo!(),
            AnyTypeEnum::IntType(int_type) => {
                (self.builder.build_alloca(int_type, &var_name).unwrap(), int_type.into())
            }
            AnyTypeEnum::FloatType(float_type) => (
                self.builder.build_alloca(float_type, &var_name).unwrap(),
                float_type.into(),
            ),
            AnyTypeEnum::PointerType(pointer_type) => (
                self.builder.build_alloca(pointer_type, &var_name).unwrap(),
                pointer_type.into(),
            ),
            AnyTypeEnum::ArrayType(array_type) => (
                self.builder.build_alloca(array_type, &var_name).unwrap(),
                array_type.into(),
            ),
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
        match variable.ty {
            Some(var_type_token) => {
                let (ptr, ty) = self.build_alloca(
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
                        ty: ty.as_type_ref(),
                    },
                );
            }
            None => {
                if let Some(expr) = variable.expr {
                    let value = self.build_expr(Rc::clone(&scope), expr);
                    let var_type = unsafe { BasicTypeEnum::new(value.get_type().as_type_ref()) };
                    let ptr = self.builder.build_alloca(var_type, &variable.name).unwrap();

                    self.build_store(ptr, value);

                    scope.borrow_mut().insert(
                        variable.name,
                        scope::ScopeRecord {
                            ptr: ptr.as_value_ref(),
                            ty: var_type.as_type_ref(),
                        },
                    );
                } else {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::TypeAnnotationRequired,
                        location: None,
                    });
                    exit(1);
                }
            }
        }
    }

    fn unescape_string(&self, s: &str) -> String {
        s.replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\\"", "\"")
            .replace("\\'", "'")
            .replace("\\\\", "\\")
    }
}
