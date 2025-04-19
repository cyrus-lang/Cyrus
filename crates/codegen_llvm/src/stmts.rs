use crate::diag::*;
use crate::scope::ScopeRecord;
use crate::structs::StructMetadata;
use crate::{CodeGenLLVM, scope::ScopeRef};
use ast::ast::{If, Statement, Variable};
use inkwell::basic_block::BasicBlock;
use inkwell::AddressSpace;
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
                self.build_return(self.build_expr(Rc::clone(&scope), statement.argument));
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
                self.build_func_decl(func_decl, false);
            }
            Statement::If(if_statement) => self.build_if(Rc::clone(&scope), if_statement),
            Statement::For(_) => todo!(),
            Statement::Switch(_) => todo!(),
            Statement::Break(location) => todo!(),
            Statement::Continue(location) => todo!(),
            Statement::Struct(struct_statement) => {
                let struct_type = self.build_struct(struct_statement.clone());
                self.struct_table.insert(
                    struct_statement.name,
                    StructMetadata {
                        struct_type,
                        fields: struct_statement.fields,
                        vis_type: struct_statement.vis_type,
                        inherits: struct_statement.inherits,
                    },
                );
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

                scope
                    .borrow_mut()
                    .insert(variable.name.clone(), ScopeRecord { ptr, ty });
            }
            None => {
                if let Some(expr) = variable.expr {
                    let value = self.build_expr(Rc::clone(&scope), expr);
                    let var_type = value.get_type(self.string_type.clone());
                    let ptr = self
                        .builder
                        .build_alloca(var_type.to_basic_type(self.context.ptr_type(AddressSpace::default())), &variable.name)
                        .unwrap();

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
