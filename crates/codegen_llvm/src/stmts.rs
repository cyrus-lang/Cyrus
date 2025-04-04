use crate::scope::ScopeRecord;
use crate::{CodeGenLLVM, scope::ScopeRef};
use crate::{diag::*, scope};
use ast::ast::{If, Statement, Variable};
use inkwell::types::{AsTypeRef, BasicTypeEnum};
use std::process::exit;
use std::rc::Rc;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_statements(&mut self, scope: ScopeRef<'ctx>, stmts: Vec<Statement>) {
        for stmt in stmts {
            self.build_statement(Rc::clone(&scope), stmt.clone());
        }
    }

    pub(crate) fn build_statement(&mut self, scope: ScopeRef<'ctx>, stmt: Statement) {
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
            Statement::If(if_statement) => self.build_if(Rc::clone(&scope), if_statement),
            Statement::For(_) => todo!(),
            Statement::Switch(_) => todo!(),
            Statement::Break(location) => todo!(),
            Statement::Continue(location) => todo!(),
            Statement::Struct(struct_statement) => self.build_struct(struct_statement),
            Statement::Enum(enum_statement) => self.build_enum(enum_statement),
            Statement::Import(import) => todo!(),
        }
    }

    pub(crate) fn build_if(&mut self, scope: ScopeRef<'ctx>, if_statement: If) {
        if let Some(current_func) = self.current_func_ref {
            let then_block = self.context.append_basic_block(current_func, "if.then");
            let else_block = self.context.append_basic_block(current_func, "if.else");
            let end_block = self.context.append_basic_block(current_func, "if.end");

            if let Some(current_block) = self.current_block_ref {
                let cond = self.build_cond(
                    Rc::clone(&scope),
                    if_statement.condition,
                    if_statement.loc.clone(),
                    if_statement.span.end,
                );

                // construct conditional branch for current_block
                self.builder.position_at_end(current_block);
                self.builder
                    .build_conditional_branch(cond, then_block, else_block)
                    .unwrap();

                self.builder.position_at_end(then_block);
                self.build_statements(Rc::clone(&scope), if_statement.consequent.exprs);
                self.builder.build_unconditional_branch(end_block).unwrap();

                self.builder.position_at_end(else_block);
                if let Some(alternate) = if_statement.alternate {
                    self.build_statements(Rc::clone(&scope), alternate.exprs);
                }
                self.builder.build_unconditional_branch(end_block).unwrap();

                self.builder.position_at_end(end_block);
                self.current_block_ref = Some(end_block);
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(
                        "Cannot build if statement without having reference to current block.".to_string(),
                    ),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: if_statement.loc.line,
                        column: if_statement.loc.column,
                        length: if_statement.span.end,
                    }),
                });
                exit(1);
            }
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom("Cannot build if statement outside of a function.".to_string()),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: if_statement.loc.line,
                    column: if_statement.loc.column,
                    length: if_statement.span.end,
                }),
            });
            exit(1);
        }
    }

    pub(crate) fn build_variable(&self, scope: ScopeRef<'ctx>, variable: Variable) {
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

                scope.borrow_mut().insert(variable.name.clone(), ScopeRecord { ptr, ty });
            }
            None => {
                if let Some(expr) = variable.expr {
                    let value = self.build_expr(Rc::clone(&scope), expr);
                    let var_type = unsafe { BasicTypeEnum::new(value.get_type().as_type_ref()) };
                    let ptr = self.builder.build_alloca(var_type, &variable.name).unwrap();

                    self.build_store(ptr, value);

                    scope
                        .borrow_mut()
                        .insert(variable.name, ScopeRecord { ptr, ty: var_type });
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
