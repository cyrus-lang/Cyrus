use crate::{CodeGenLLVM, scope::ScopeRef};
use crate::{diag::*, scope};
use ast::ast::{Statement, Variable};
use inkwell::types::{AsTypeRef, BasicTypeEnum};
use inkwell::values::AsValueRef;
use std::process::exit;
use std::rc::Rc;

impl<'ctx> CodeGenLLVM<'ctx> {
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
            Statement::Struct(struct_statement) => self.build_struct(struct_statement),
            Statement::Enum(enum_statement) => self.build_enum(enum_statement),
            Statement::Import(import) => todo!(),
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
}
