use crate::diag::*;
use crate::scope::ScopeRecord;
use crate::{CodeGenLLVM, scope::ScopeRef};
use ast::ast::{If, Statement, TypeSpecifier, Variable};
use ast::token::TokenKind;
use inkwell::AddressSpace;
use inkwell::basic_block::BasicBlock;
use inkwell::values::{AnyValue, BasicValueEnum};
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
            Statement::Return(statement) => {
                self.build_return(
                    self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), statement.argument)),
                    statement.loc,
                    statement.span.end,
                );
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
            Statement::Struct(struct_statement) => {
                self.build_global_struct(struct_statement);
            }
            Statement::Enum(enum_statement) => self.build_enum(enum_statement),
            Statement::Import(import) => self.build_import(import),
        }
    }

    pub(crate) fn block_terminated(&self, block: BasicBlock<'ctx>) -> bool {
        self.terminated_blocks.contains(&block)
    }

    pub(crate) fn mark_block_terminated(&mut self, block: BasicBlock<'ctx>) {
        self.terminated_blocks.push(block);
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

                self.builder.position_at_end(current_block);
                if !self.block_terminated(current_block) {
                    self.builder
                        .build_conditional_branch(cond, then_block, else_block)
                        .unwrap();
                    self.mark_block_terminated(current_block);
                }

                self.builder.position_at_end(then_block);
                self.current_block_ref = Some(then_block);
                self.build_statements(Rc::clone(&scope), if_statement.consequent.exprs);
                if !self.block_terminated(then_block) {
                    self.builder.build_unconditional_branch(end_block).unwrap();
                    self.mark_block_terminated(then_block);
                }

                let mut current_else_block = else_block;
                for else_if in if_statement.branches {
                    let new_else_block = self.context.append_basic_block(current_func, "else_if");
                    let new_then_block = self.context.append_basic_block(current_func, "else_if.then");

                    self.builder.position_at_end(current_else_block);
                    let else_if_cond = self.build_cond(
                        Rc::clone(&scope),
                        else_if.condition,
                        else_if.loc.clone(),
                        else_if.span.end,
                    );

                    if !self.block_terminated(current_else_block) {
                        self.builder
                            .build_conditional_branch(else_if_cond, new_then_block, new_else_block)
                            .unwrap();
                        self.mark_block_terminated(current_else_block);
                    }

                    self.builder.position_at_end(new_then_block);
                    self.current_block_ref = Some(new_then_block);
                    self.build_statements(Rc::clone(&scope), else_if.consequent.exprs);
                    if !self.block_terminated(new_then_block) {
                        self.builder.build_unconditional_branch(end_block).unwrap();
                        self.mark_block_terminated(new_then_block);
                    }

                    current_else_block = new_else_block;
                }

                // handle the final "else" block
                self.builder.position_at_end(current_else_block);
                self.current_block_ref = Some(current_else_block);
                if let Some(alternate) = if_statement.alternate {
                    self.build_statements(Rc::clone(&scope), alternate.exprs);
                }
                if !self.block_terminated(current_else_block) {
                    self.builder.build_unconditional_branch(end_block).unwrap();
                    self.mark_block_terminated(current_else_block);
                }

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
            Some(type_specifier) => {
                if let TypeSpecifier::TypeToken(type_token) = type_specifier.clone() {
                    if type_token.kind == TokenKind::Void {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom("Cannot declare a variable with 'void' type.".to_string()),
                            location: None,
                        });
                        exit(1);
                    }
                }

                let (ptr, ty) = self.build_alloca(
                    type_specifier.clone(),
                    variable.name.clone(),
                    variable.loc.clone(),
                    variable.span.end,
                );

                if let Some(expr) = variable.expr {
                    let rvalue = self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), expr));

                    if !self.compatible_types(ty.clone(), rvalue.get_type(self.string_type.clone())) {
                        // FIXME We need accurate type name tracking here
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(format!(
                                "Cannot assign value of type '{}' to lvalue of type '{}'.",
                                rvalue
                                    .get_type(self.string_type.clone())
                                    .to_basic_type(self.context.ptr_type(AddressSpace::default())),
                                ty.to_basic_type(self.context.ptr_type(AddressSpace::default())),
                            )),
                            location: None,
                        });
                        exit(1);
                    };

                    let final_rvalue = self.implicit_cast(
                        rvalue,
                        self.build_type(type_specifier, variable.loc.clone(), variable.span.end),
                    );

                    self.builder.build_store(ptr, final_rvalue).unwrap();
                } else if ty.is_const_type() && variable.expr.is_none() {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Variable '{}' is declared as constant but has no initializer.",
                            variable.name
                        )),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: variable.loc.line,
                            column: variable.loc.column,
                            length: variable.span.end,
                        }),
                    });
                    exit(1);
                } else {
                    let zero_init = self.build_zero_initialized_internal_value(ty.clone());
                    let final_rvalue: BasicValueEnum =
                        zero_init.to_basic_metadata().as_any_value_enum().try_into().unwrap();
                    self.builder.build_store(ptr, final_rvalue).unwrap();
                }

                let mut scope_borrow = scope.borrow_mut();

                if scope_borrow.get(variable.name.clone()).is_some() {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!(
                            "Variable '{}' would shadow a previous declaration.",
                            variable.name
                        )),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: variable.loc.line,
                            column: variable.loc.column,
                            length: variable.span.end,
                        }),
                    });
                    exit(1);
                }

                scope_borrow.insert(variable.name.clone(), ScopeRecord { ptr, ty });
            }
            None => {
                if let Some(expr) = variable.expr {
                    let rvalue = self.internal_value_as_rvalue(self.build_expr(Rc::clone(&scope), expr));
                    let var_type = rvalue.get_type(self.string_type.clone());

                    let ptr = self
                        .builder
                        .build_alloca(
                            var_type.to_basic_type(self.context.ptr_type(AddressSpace::default())),
                            &variable.name,
                        )
                        .unwrap();

                    self.build_store(ptr, rvalue);

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
