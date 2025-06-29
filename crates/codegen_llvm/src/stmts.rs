use crate::diag::*;
use crate::scope::ScopeRecord;
use crate::{CodeGenLLVM, scope::ScopeRef};
use ast::ast::{BlockStatement, Break, Continue, Expression, For, If, Literal, Statement, TypeSpecifier, Variable};
use ast::token::{Location, TokenKind};
use inkwell::AddressSpace;
use inkwell::basic_block::BasicBlock;
use inkwell::values::{AnyValue, BasicValueEnum, FunctionValue};
use std::cell::RefCell;
use std::process::exit;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct LoopBlockRefs<'a> {
    pub cond_block: BasicBlock<'a>,
    pub end_block: BasicBlock<'a>,
}

#[derive(Debug, Clone)]
pub struct TerminatedBlockMetadata<'a> {
    pub basic_block: BasicBlock<'a>,
    pub terminated_with_return: bool,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_statements(&mut self, scope: ScopeRef<'ctx>, stmts: Vec<Statement>) {
        for stmt in stmts {
            self.build_statement(Rc::clone(&scope), stmt.clone());
        }
    }

    pub(crate) fn build_statement(&mut self, scope: ScopeRef<'ctx>, stmt: Statement) {
        match stmt {
            Statement::BlockStatement(block_statement) => {
                self.build_statements(
                    Rc::new(RefCell::new(scope.borrow().deep_clone_detached())),
                    block_statement.exprs,
                );
            }
            Statement::Expression(expression) => {
                self.build_expr(Rc::clone(&scope), expression);
            }
            Statement::Variable(variable) => self.build_variable(Rc::clone(&scope), variable),
            Statement::Return(statement) => {
                self.build_return(Rc::clone(&scope), statement);
            }
            Statement::FuncDef(func_def) => {
                if func_def.name == "main" {
                    // entry_point gonna be evaluated in the final step of code generation
                    // that is why here we store it in the context
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
                    let param_types: Vec<*mut inkwell::llvm_sys::LLVMType> = self.build_func_params(
                        func_def.name.clone(),
                        func_def.loc.clone(),
                        func_def.span.end,
                        func_def.params.list.clone(),
                    );

                    self.build_func_def(func_def.clone(), param_types, false);
                }
            }
            Statement::FuncDecl(func_decl) => {
                let param_types = self.build_func_params(
                    func_decl.name.clone(),
                    func_decl.loc.clone(),
                    func_decl.span.end,
                    func_decl.params.list.clone(),
                );

                self.build_func_decl(func_decl, param_types);
            }
            Statement::If(if_statement) => self.build_if(Rc::clone(&scope), if_statement),
            Statement::For(for_statement) => self.build_for_statement(
                Rc::new(RefCell::new(scope.borrow().deep_clone_detached())),
                for_statement,
            ),
            Statement::Switch(_) => todo!(),
            Statement::Break(break_statement) => {
                self.build_break_statement(break_statement);
            }
            Statement::Continue(continue_statement) => self.build_continue_statement(continue_statement),
            Statement::Struct(struct_statement) => {
                self.build_global_struct(struct_statement);
            }
            Statement::Enum(enum_statement) => self.build_enum(enum_statement),
            Statement::Import(import) => self.build_import(import),
        }
    }

    pub(crate) fn is_block_terminated(&self, basic_block: BasicBlock<'ctx>) -> bool {
        self.terminated_blocks
            .iter()
            .find(|metadata| metadata.basic_block == basic_block)
            .is_some()
    }

    pub(crate) fn get_block_terminated_metadata(
        &self,
        basic_block: BasicBlock<'ctx>,
    ) -> Option<TerminatedBlockMetadata<'ctx>> {
        self.terminated_blocks
            .iter()
            .find(|metadata| metadata.basic_block == basic_block)
            .cloned()
    }

    pub(crate) fn mark_block_terminated(&mut self, basic_block: BasicBlock<'ctx>, terminated_with_return: bool) {
        self.terminated_blocks.push(TerminatedBlockMetadata {
            basic_block,
            terminated_with_return,
        });
    }

    pub(crate) fn get_current_block(&self, stmt_name: &'ctx str, loc: Location, span_end: usize) -> BasicBlock<'ctx> {
        match self.current_block_ref {
            Some(bb) => bb,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Cannot build {} without having reference to current block.",
                        stmt_name
                    )),
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

    pub(crate) fn get_current_func(&self, stmt_name: &'ctx str, loc: Location, span_end: usize) -> FunctionValue<'ctx> {
        match self.current_func_ref {
            Some(bb) => bb,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!("Cannot build {} outside of a function.", stmt_name)),
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

    pub(crate) fn build_continue_statement(&mut self, continue_statement: Continue) {
        let current_block = self.get_current_block(
            "continue statement",
            continue_statement.loc.clone(),
            continue_statement.span.end,
        );

        let loop_end_block = match &self.current_loop_ref {
            Some(loop_block_refs) => loop_block_refs.cond_block,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Continue statement can only be used inside of a for loop.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: continue_statement.loc.line,
                        column: continue_statement.loc.column,
                        length: continue_statement.span.end,
                    }),
                });
                exit(1);
            }
        };

        self.mark_block_terminated(current_block, false);
        self.builder.build_unconditional_branch(loop_end_block).unwrap();
        self.builder.position_at_end(loop_end_block);
    }

    pub(crate) fn build_break_statement(&mut self, break_statement: Break) {
        let current_block =
            self.get_current_block("break statement", break_statement.loc.clone(), break_statement.span.end);

        let loop_end_block = match &self.current_loop_ref {
            Some(loop_block_refs) => loop_block_refs.end_block,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Break statement can only be used inside of a for loop.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: break_statement.loc.line,
                        column: break_statement.loc.column,
                        length: break_statement.span.end,
                    }),
                });
                exit(1);
            }
        };

        self.mark_block_terminated(current_block, false);
        self.builder.build_unconditional_branch(loop_end_block).unwrap();
        self.builder.position_at_end(loop_end_block);
    }

    pub(crate) fn build_infinite_for_statement(&mut self, scope: ScopeRef<'ctx>, for_statement: For) {
        let always_true_condition = Expression::Literal(Literal::Bool(true));

        let current_block = self.get_current_block("for statement", for_statement.loc.clone(), for_statement.span.end);
        let current_func = self.get_current_func("for statement", for_statement.loc.clone(), for_statement.span.end);

        let cond_block = self.context.append_basic_block(current_func, "loop.cond");
        let body_block = self.context.append_basic_block(current_func, "loop.body");
        let end_block = self.context.append_basic_block(current_func, "loop.end");

        // track current_loop
        let previous_loop_ref = self.current_loop_ref.clone();
        self.current_loop_ref = Some(LoopBlockRefs { cond_block, end_block });

        self.builder.position_at_end(current_block);
        self.builder.build_unconditional_branch(cond_block).unwrap();
        self.mark_block_terminated(current_block, false);

        self.builder.position_at_end(cond_block);
        let condition = self.build_cond(
            Rc::clone(&scope),
            always_true_condition,
            for_statement.loc.clone(),
            for_statement.span.end,
        );
        self.builder
            .build_conditional_branch(condition, body_block, end_block)
            .unwrap();
        self.mark_block_terminated(cond_block, false);

        self.current_block_ref = Some(body_block);
        self.builder.position_at_end(body_block);

        for stmt in for_statement.body.exprs {
            let current_block =
                self.get_current_block("for statement", for_statement.loc.clone(), for_statement.span.end);
            if self.is_block_terminated(current_block) {
                break;
            }

            self.build_statement(Rc::clone(&scope), stmt.clone());
        }

        let after_body_block =
            self.get_current_block("for statement", for_statement.loc.clone(), for_statement.span.end);

        if !self.is_block_terminated(after_body_block) {
            self.builder.position_at_end(after_body_block);
            self.builder.build_unconditional_branch(cond_block).unwrap();
            self.mark_block_terminated(after_body_block, false);
        }

        self.current_block_ref = Some(end_block);
        self.builder.position_at_end(end_block);

        // clear current_loop
        self.current_loop_ref = previous_loop_ref;
    }

    pub(crate) fn build_conditional_for_statement(
        &mut self,
        scope: ScopeRef<'ctx>,
        condition: Expression,
        body: BlockStatement,
        increment: Option<Expression>,
        loc: Location,
        span_end: usize,
    ) {
        let current_block = self.get_current_block("for statement", loc.clone(), span_end);
        let current_func = self.get_current_func("for statement", loc.clone(), span_end);

        let cond_block = self.context.append_basic_block(current_func, "loop.cond");
        let body_block = self.context.append_basic_block(current_func, "loop.body");
        let end_block = self.context.append_basic_block(current_func, "loop.end");

        // track current_loop
        let previous_loop_ref = self.current_loop_ref.clone();
        self.current_loop_ref = Some(LoopBlockRefs { cond_block, end_block });

        self.builder.position_at_end(current_block);
        self.builder.build_unconditional_branch(cond_block).unwrap();
        self.mark_block_terminated(current_block, false);

        self.builder.position_at_end(cond_block);
        let condition = self.build_cond(Rc::clone(&scope), condition, loc.clone(), span_end);
        self.builder
            .build_conditional_branch(condition, body_block, end_block)
            .unwrap();
        self.mark_block_terminated(cond_block, false);

        self.current_block_ref = Some(body_block);
        self.builder.position_at_end(body_block);

        for stmt in body.exprs {
            let current_block = self.get_current_block("for statement", loc.clone(), span_end);
            if self.is_block_terminated(current_block) {
                break;
            }

            self.build_statement(Rc::clone(&scope), stmt.clone());
        }

        let after_body_block = self.get_current_block("for statement", loc.clone(), span_end);

        if !self.is_block_terminated(after_body_block) {
            self.builder.position_at_end(after_body_block);

            // build increment expression
            if let Some(increment) = increment {
                self.build_expr(Rc::clone(&scope), increment);
            }

            self.builder.build_unconditional_branch(cond_block).unwrap();
            self.mark_block_terminated(after_body_block, false);
        }

        self.current_block_ref = Some(end_block);
        self.builder.position_at_end(end_block);

        // clear current_loop
        self.current_loop_ref = previous_loop_ref;
    }

    pub(crate) fn build_for_statement(&mut self, scope: ScopeRef<'ctx>, for_statement: For) {
        // unconditional for loop
        if for_statement.condition.is_none() && for_statement.increment.is_none() {
            if let Some(initializer) = for_statement.initializer.clone() {
                let scope_cloned = Rc::new(RefCell::new(scope.borrow_mut().deep_clone_detached()));
                self.build_variable(Rc::clone(&scope_cloned), initializer);
                self.build_infinite_for_statement(Rc::clone(&scope_cloned), for_statement);
            } else {
                self.build_infinite_for_statement(scope, for_statement);
            }
        } else if for_statement.increment.is_none() {
            let scope_cloned = Rc::new(RefCell::new(scope.borrow_mut().deep_clone_detached()));
            self.build_variable(Rc::clone(&scope_cloned), for_statement.initializer.unwrap());
            self.build_conditional_for_statement(
                scope_cloned,
                for_statement.condition.unwrap(),
                *for_statement.body,
                None,
                for_statement.loc.clone(),
                for_statement.span.end,
            );
        } else {
            let scope_cloned = Rc::new(RefCell::new(scope.borrow_mut().deep_clone_detached()));
            self.build_variable(Rc::clone(&scope_cloned), for_statement.initializer.unwrap());
            self.build_conditional_for_statement(
                scope_cloned,
                for_statement.condition.unwrap(),
                *for_statement.body,
                Some(for_statement.increment.unwrap()),
                for_statement.loc.clone(),
                for_statement.span.end,
            );
        }
    }

    pub(crate) fn build_if(&mut self, scope: ScopeRef<'ctx>, if_statement: If) {
        let current_block = self.get_current_block("for statement", if_statement.loc.clone(), if_statement.span.end);
        let current_func = self.get_current_func("for statement", if_statement.loc.clone(), if_statement.span.end);

        let then_block = self.context.append_basic_block(current_func, "if.then");
        let else_block = self.context.append_basic_block(current_func, "if.else");
        let end_block = self.context.append_basic_block(current_func, "if.end");

        let cond = self.build_cond(
            Rc::clone(&scope),
            if_statement.condition,
            if_statement.loc.clone(),
            if_statement.span.end,
        );

        // build condition to enter then_block and else_block
        self.builder.position_at_end(current_block);
        if !self.is_block_terminated(current_block) {
            self.builder
                .build_conditional_branch(cond, then_block, else_block)
                .unwrap();
            self.mark_block_terminated(current_block, false);
        }

        self.builder.position_at_end(then_block);
        self.current_block_ref = Some(then_block);
        self.build_statements(
            Rc::new(RefCell::new(scope.borrow().deep_clone_detached())),
            if_statement.consequent.exprs,
        );
        if !self.is_block_terminated(then_block) {
            self.builder.build_unconditional_branch(end_block).unwrap();
            self.mark_block_terminated(then_block, false);
        }

        let mut branches_terminated_with_return: Vec<bool> = vec![
            self.get_block_terminated_metadata(then_block)
                .unwrap()
                .terminated_with_return,
        ];

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

            if !self.is_block_terminated(current_else_block) {
                self.builder
                    .build_conditional_branch(else_if_cond, new_then_block, new_else_block)
                    .unwrap();
                self.mark_block_terminated(current_else_block, false);
            }

            self.builder.position_at_end(new_then_block);
            self.current_block_ref = Some(new_then_block);
            self.build_statements(
                Rc::new(RefCell::new(scope.borrow().deep_clone_detached())),
                else_if.consequent.exprs,
            );
            if !self.is_block_terminated(new_then_block) {
                self.builder.build_unconditional_branch(end_block).unwrap();
                self.mark_block_terminated(new_then_block, false);
            }

            branches_terminated_with_return.push(
                self.get_block_terminated_metadata(new_then_block)
                    .unwrap()
                    .terminated_with_return,
            );

            current_else_block = new_else_block;
        }

        // handle the final "else" block
        self.builder.position_at_end(current_else_block);
        self.current_block_ref = Some(current_else_block);
        if let Some(alternate) = if_statement.alternate {
            self.build_statements(
                Rc::new(RefCell::new(scope.borrow().deep_clone_detached())),
                alternate.exprs,
            );
        }
        if !self.is_block_terminated(current_else_block) {
            self.builder.build_unconditional_branch(end_block).unwrap();
            self.mark_block_terminated(current_else_block, false);
        }
        branches_terminated_with_return.push(
            self.get_block_terminated_metadata(then_block)
                .unwrap()
                .terminated_with_return,
        );

        // here we're sure that all of the branches are terminated with return statement,
        // so the final branch will never be terminated and llvm gonna raise errors. so we try to safely
        // remove the ending block because it's not required anymore.
        if branches_terminated_with_return.iter().all(|&x| x) {
            end_block.remove_from_function().unwrap();
            return;
        }

        self.builder.position_at_end(end_block);
        self.current_block_ref = Some(end_block);
    }

    pub(crate) fn build_variable(&self, scope: ScopeRef<'ctx>, variable: Variable) {
        match variable.ty {
            Some(type_specifier) => {
                if let TypeSpecifier::TypeToken(type_token) = type_specifier.clone() {
                    if type_token.kind == TokenKind::Void {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom("Cannot declare a variable with 'void' type.".to_string()),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: variable.loc.line,
                                column: variable.loc.column,
                                length: variable.span.end,
                            }),
                        });
                        exit(1);
                    }
                }

                let (ptr, var_internal_type) = self.build_alloca(
                    type_specifier.clone(),
                    variable.name.clone(),
                    variable.loc.clone(),
                    variable.span.end,
                );

                if let Some(expr) = variable.expr {
                    let rvalue = self.internal_value_as_rvalue(
                        self.build_expr(Rc::clone(&scope), expr),
                        variable.loc.clone(),
                        variable.span.end,
                    );

                    if !self.compatible_types(var_internal_type.clone(), rvalue.get_type(self.string_type.clone())) {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(format!(
                                "Cannot assign value of type '{}' to lvalue of type '{}'.",
                                rvalue.get_type(self.string_type.clone()),
                                var_internal_type,
                            )),
                            location: None,
                        });
                        exit(1);
                    };

                    let final_rvalue = self.implicit_cast(
                        rvalue,
                        self.build_type(type_specifier, variable.loc.clone(), variable.span.end),
                        variable.loc.clone(),
                        variable.span.end,
                    );

                    self.builder.build_store(ptr, final_rvalue).unwrap();
                } else if var_internal_type.is_const_type() && variable.expr.is_none() {
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
                    let zero_init = self.build_zero_initialized_internal_value(
                        var_internal_type.clone(),
                        variable.loc.clone(),
                        variable.span.end,
                    );
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

                scope_borrow.insert(
                    variable.name.clone(),
                    ScopeRecord {
                        ptr,
                        ty: var_internal_type,
                    },
                );
            }
            None => {
                if let Some(expr) = variable.expr {
                    let rvalue = self.internal_value_as_rvalue(
                        self.build_expr(Rc::clone(&scope), expr),
                        variable.loc.clone(),
                        variable.span.end,
                    );
                    let var_internal_type = rvalue.get_type(self.string_type.clone());

                    let var_basic_type =
                        match var_internal_type.to_basic_type(self.context.ptr_type(AddressSpace::default())) {
                            Ok(basic_type) => basic_type,
                            Err(err) => {
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::Custom(err.to_string()),
                                    location: Some(DiagLoc {
                                        file: self.file_path.clone(),
                                        line: variable.loc.line,
                                        column: variable.loc.column,
                                        length: variable.span.end,
                                    }),
                                });
                                exit(1);
                            }
                        };

                    let ptr = self.builder.build_alloca(var_basic_type, &variable.name).unwrap();

                    self.build_store(ptr, rvalue);

                    scope.borrow_mut().insert(
                        variable.name,
                        ScopeRecord {
                            ptr,
                            ty: var_internal_type,
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
