use super::module::{CodeGenBuilder, LocalIRValue};
use crate::builder::module::{LoopBlockRefs, TerminatedBlockMetadata};
use inkwell::{
    basic_block::BasicBlock,
    types::{BasicTypeEnum, StructType},
    values::FunctionValue,
};
use resolver::scope::{LocalScopeRef, ResolvedFunction};
use typed_ast::{
    SymbolID, TypedBlockStatement, TypedBreak, TypedContinue, TypedExpression, TypedFor, TypedIf, TypedReturn,
    TypedStatement, TypedStruct,
};

/// A macro to build the LLVM IR for a loop structure.
///
/// This macro abstracts the common logic for creating loop-related basic blocks
/// (`cond`, `body`, `end`), handling the loop condition, iterating over the body's
/// statements, and optionally building an increment expression.
///
/// # Arguments
///
/// * `$self`: The mutable compiler state instance.
/// * `$scope`: The current variable scope (`ScopeRef`).
/// * `$condition`: The `BasicMetadataValueEnum` to be evaluated as the loop's condition.
/// * `$body`: An iterable of statements that form the loop's body.
/// * `$increment`: An `Option<Expression>` for the loop's increment step.
/// * `$loc`: The source code location for error reporting.
/// * `$span_end`: The end of the source code span for error reporting.
#[macro_export]
macro_rules! build_loop_statement {
    (
        $self:expr,
        $scope:expr,
        $condition:tt,
        $body:tt,
        $increment:tt
    ) => {{
        let current_block = $self.blockreg.current_block_ref.unwrap();
        let current_func = $self.blockreg.current_func_ref.unwrap();

        let cond_block = $self.llvmctx.append_basic_block(current_func, "loop.cond");
        let body_block = $self.llvmctx.append_basic_block(current_func, "loop.body");
        let inc_block = $self.llvmctx.append_basic_block(current_func, "loop.inc");
        let end_block = $self.llvmctx.append_basic_block(current_func, "loop.end");

        let previous_loop_ref = $self.blockreg.current_loop_ref.clone();
        $self.blockreg.current_loop_ref = Some(LoopBlockRefs { cond_block, end_block, inc_block });

        $self.llvmbuilder.position_at_end(current_block);
        $self.llvmbuilder.build_unconditional_branch(cond_block).unwrap();
        $self.mark_block_terminated(current_block, false);

        // increment block
        {
            $self.llvmbuilder.position_at_end(inc_block);
            $increment
            $self.llvmbuilder.build_unconditional_branch(cond_block).unwrap();
        }

        // cond block
        {
            $self.llvmbuilder.position_at_end(cond_block);

            let cond_value = $condition;

            $self
                .llvmbuilder
                .build_conditional_branch(cond_value, body_block, end_block)
                .unwrap();

            $self.mark_block_terminated(cond_block, false);
        }

        // body block
        {
            $self.blockreg.current_block_ref = Some(body_block);
            $self.llvmbuilder.position_at_end(body_block);
            $body
        }

        let after_body_block = $self.blockreg.current_block_ref.unwrap();

        if !$self.is_block_terminated(after_body_block) {
            $self.blockreg.current_block_ref = Some(after_body_block);
            $self.llvmbuilder.position_at_end(after_body_block);
            $self.llvmbuilder.build_unconditional_branch(inc_block).unwrap();
            $self.mark_block_terminated(after_body_block, false);
        }

        $self.blockreg.current_block_ref = Some(end_block);
        $self.llvmbuilder.position_at_end(end_block);
        $self.blockreg.current_loop_ref = previous_loop_ref;
    }};
}

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_toplevel_statements(&mut self, stmts: &Vec<TypedStatement>) {
        self.build_forward_decls(stmts);

        for stmt in stmts {
            match stmt {
                TypedStatement::FuncDef(typed_func_def) => self.build_func_def(typed_func_def),
                TypedStatement::Struct(typed_struct) => self.build_struct_def(typed_struct),
                TypedStatement::Enum(typed_enum) => self.build_enum_def(typed_enum),
                TypedStatement::GlobalVariable(typed_global_var) => self.build_global_var_def(typed_global_var),
                TypedStatement::Interface(typed_interface) => todo!(),
                TypedStatement::FuncDecl(_) => continue,
                _ => continue,
            }
        }
    }

    pub(crate) fn get_or_declare_func(
        &mut self,
        symbol_id: SymbolID,
        resolved_func: &ResolvedFunction,
    ) -> FunctionValue<'a> {
        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&symbol_id).cloned();
        drop(irreg);

        let fn_value = match local_ir_value {
            Some(local_ir_value) => local_ir_value.as_func().unwrap().clone(),
            None => self.build_func_decl(
                resolved_func.func_sig.name.clone(),
                resolved_func.func_sig.params.clone(),
                resolved_func.func_sig.return_type.clone(),
                resolved_func.func_sig.vis.clone(),
                None,
            ),
        };

        fn_value
    }

    fn build_forward_decls(&mut self, stmts: &Vec<TypedStatement>) {
        let insert_forward_decl_to_registry = |this: &Self, symbol_id: SymbolID, local_value: LocalIRValue<'a>| {
            let mut irreg = this.irreg.borrow_mut();
            irreg.insert(symbol_id, local_value);
            drop(irreg);
        };

        for stmt in stmts {
            match stmt {
                TypedStatement::Struct(typed_struct) => {
                    let struct_type = self.build_struct_decl(&typed_struct.name);
                    insert_forward_decl_to_registry(self, typed_struct.symbol_id, LocalIRValue::Struct(struct_type));
                }
                TypedStatement::Enum(typed_enum) => {
                    let struct_type = self.build_enum_decl(&typed_enum.name);
                    insert_forward_decl_to_registry(self, typed_enum.symbol_id, LocalIRValue::Struct(struct_type));
                }
                TypedStatement::Interface(typed_interface) => todo!(),
                _ => continue,
            }
        }

        for stmt in stmts {
            match stmt {
                TypedStatement::GlobalVariable(typed_global_var) => {
                    let global_value = self.build_global_var_decl(typed_global_var);
                    insert_forward_decl_to_registry(
                        self,
                        typed_global_var.symbol_id,
                        LocalIRValue::GlobalValue(global_value),
                    );
                }
                TypedStatement::FuncDef(typed_func_def) => {
                    let fn_value = self.build_func_decl(
                        typed_func_def.name.clone(),
                        typed_func_def.params.clone(),
                        typed_func_def.return_type.clone(),
                        typed_func_def.vis.clone(),
                        None,
                    );
                    insert_forward_decl_to_registry(self, typed_func_def.symbol_id, LocalIRValue::Func(fn_value));
                }
                TypedStatement::FuncDecl(typed_func_decl) => {
                    let fn_value = self.build_func_decl(
                        typed_func_decl.name.clone(),
                        typed_func_decl.params.clone(),
                        typed_func_decl.return_type.clone(),
                        typed_func_decl.vis.clone(),
                        None,
                    );
                    insert_forward_decl_to_registry(self, typed_func_decl.symbol_id, LocalIRValue::Func(fn_value));
                }
                _ => continue,
            }
        }
    }

    fn build_struct_decl(&self, name: &String) -> StructType<'a> {
        self.llvmctx.opaque_struct_type(name)
    }

    fn build_local_struct_def(&mut self, typed_struct: &TypedStruct) {
        let field_types: Vec<BasicTypeEnum<'a>> = typed_struct
            .fields
            .iter()
            .map(|field| self.build_concrete_type(None, field.ty.clone()).try_into().unwrap())
            .collect();

        let struct_type = self.llvmctx.struct_type(&field_types, typed_struct.packed);

        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(typed_struct.symbol_id, LocalIRValue::Struct(struct_type));
        drop(irreg);
    }

    fn build_struct_def(&mut self, typed_struct: &TypedStruct) {
        let field_types: Vec<BasicTypeEnum<'a>> = typed_struct
            .fields
            .iter()
            .map(|field| self.build_concrete_type(None, field.ty.clone()).try_into().unwrap())
            .collect();

        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&typed_struct.symbol_id).unwrap();

        let struct_type = local_ir_value.as_struct().unwrap();
        struct_type.set_body(&field_types, typed_struct.packed);
        drop(irreg);
    }

    fn build_enum_decl(&self, name: &String) -> StructType<'a> {
        self.llvmctx.opaque_struct_type(name)
    }

    pub(crate) fn build_block_statement(&mut self, block_stmt: &TypedBlockStatement) {
        let local_scope_opt = Some(
            self.resolver
                .get_scope_ref(self.module_id, block_stmt.scope_id)
                .unwrap(),
        );

        for stmt in &block_stmt.exprs {
            match stmt {
                TypedStatement::Variable(typed_variable) => {
                    self.build_local_variable(local_scope_opt.clone(), typed_variable);
                }
                TypedStatement::If(typed_if) => self.build_if(local_scope_opt.clone(), typed_if),
                TypedStatement::For(typed_for) => self.build_for(local_scope_opt.clone(), typed_for),
                TypedStatement::Return(typed_return) => self.build_return(local_scope_opt.clone(), typed_return),
                TypedStatement::Break(typed_break) => self.build_break(typed_break),
                TypedStatement::Continue(typed_continue) => self.build_continue(typed_continue),
                TypedStatement::Switch(typed_switch) => todo!(),
                TypedStatement::Struct(typed_struct) => {
                    self.build_local_struct_def(typed_struct);
                }
                TypedStatement::Enum(typed_enum) => {
                    self.build_local_enum_def(typed_enum);
                }
                TypedStatement::Expression(typed_expr) => {
                    let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
                    self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
                }
                TypedStatement::BlockStatement(typed_block_statement) => {
                    self.build_block_statement(typed_block_statement);
                }
                TypedStatement::Interface(typed_interface) => todo!(),
                // Skipped statements
                TypedStatement::Typedef(_) => continue,
                // Invalid statements
                TypedStatement::FuncDef(_) => unreachable!(),
                TypedStatement::FuncDecl(_) => unreachable!(),
                TypedStatement::Import(_) => unreachable!(),
                TypedStatement::GlobalVariable(_) => unreachable!(),
            }
        }
    }

    fn build_return(&mut self, local_scope_opt: Option<LocalScopeRef>, return_stmt: &TypedReturn) {
        match &return_stmt.argument {
            Some(argument) => {
                let lvalue = self.build_expr(local_scope_opt.clone(), argument);
                let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt, lvalue);
                self.llvmbuilder.build_return(Some(&rvalue.as_basic_value())).unwrap();
            }
            None => {
                self.llvmbuilder.build_return(None).unwrap();
            }
        }
    }

    fn build_continue(&mut self, _: &TypedContinue) {
        let current_block = self.blockreg.current_block_ref.unwrap();

        let loop_end_block = match &self.blockreg.current_loop_ref {
            Some(loop_block_refs) => loop_block_refs.inc_block,
            None => {
                panic!("Invalid continue statement.");
            }
        };

        self.mark_block_terminated(current_block, false);
        self.llvmbuilder.build_unconditional_branch(loop_end_block).unwrap();
        self.llvmbuilder.position_at_end(loop_end_block);
    }

    fn build_break(&mut self, _: &TypedBreak) {
        let current_block = self.blockreg.current_block_ref.unwrap();
        self.mark_block_terminated(current_block, false);

        let target_block = match match &self.blockreg.current_loop_ref {
            Some(loop_block_refs) => Some(loop_block_refs.end_block),
            None => match &self.blockreg.current_switch {
                Some(switch_block_refs) => Some(switch_block_refs.exit_block),
                None => None,
            },
        } {
            Some(basic_block) => basic_block,
            None => {
                panic!("Invalid break statement.");
            }
        };

        self.llvmbuilder.build_unconditional_branch(target_block).unwrap();
        self.llvmbuilder.position_at_end(target_block);
    }

    fn build_infinite_for_statement(&mut self, for_stmt: &TypedFor) {
        let always_true_condition = self.llvmctx.bool_type().const_int(1, false);

        build_loop_statement!(
            self,
            scope,
            always_true_condition,
            {
                self.build_block_statement(&for_stmt.body);
            },
            {}
        );
    }

    pub(crate) fn build_conditional_for_statement(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        unevaluated_cond_value: &TypedExpression,
        body: &TypedBlockStatement,
        increment: Option<TypedExpression>,
    ) {
        build_loop_statement!(
            self,
            scope,
            {
                let cond_value = self.build_expr(local_scope_opt.clone(), &unevaluated_cond_value);
                cond_value.as_basic_value().into_int_value()
            },
            {
                self.build_block_statement(body);
            },
            {
                if let Some(increment_expr) = increment {
                    self.build_expr(local_scope_opt.clone(), &increment_expr);
                }
            }
        );
    }

    fn build_for(&mut self, local_scope_opt: Option<LocalScopeRef>, typed_for: &TypedFor) {
        // unconditional for loop
        if typed_for.condition.is_none() && typed_for.increment.is_none() {
            if let Some(initializer) = &typed_for.initializer {
                self.build_local_variable(local_scope_opt.clone(), initializer);
                self.build_infinite_for_statement(typed_for);
            } else {
                self.build_infinite_for_statement(typed_for);
            }
        } else if typed_for.increment.is_none() {
            self.build_local_variable(local_scope_opt.clone(), &typed_for.initializer.clone().unwrap());

            self.build_conditional_for_statement(
                local_scope_opt,
                &typed_for.condition.clone().unwrap(),
                &typed_for.body,
                None,
            );
        } else {
            self.build_local_variable(local_scope_opt.clone(), &typed_for.initializer.clone().unwrap());

            self.build_conditional_for_statement(
                local_scope_opt,
                &typed_for.condition.clone().unwrap(),
                &typed_for.body,
                Some(typed_for.increment.clone().unwrap()),
            );
        }
    }

    fn build_if(&mut self, local_scope_opt: Option<LocalScopeRef>, typed_if: &TypedIf) {
        let current_block = self.blockreg.current_block_ref.unwrap();
        let current_func = self.blockreg.current_func_ref.unwrap();

        let then_block = self.llvmctx.append_basic_block(current_func, "if.then");
        let else_block = self.llvmctx.append_basic_block(current_func, "if.else");
        let end_block = self.llvmctx.append_basic_block(current_func, "if.end");

        let cond = self.build_expr(local_scope_opt.clone(), &typed_if.condition);

        self.llvmbuilder.position_at_end(current_block);
        if !self.is_block_terminated(current_block) {
            self.llvmbuilder
                .build_conditional_branch(cond.as_basic_value().into_int_value(), then_block, else_block)
                .unwrap();
            self.mark_block_terminated(current_block, false);
        }

        // build then block
        self.llvmbuilder.position_at_end(then_block);
        self.blockreg.current_block_ref = Some(then_block);
        self.build_block_statement(&typed_if.consequent);
        if !self.is_block_terminated(then_block) {
            self.llvmbuilder.build_unconditional_branch(end_block).unwrap();
            self.mark_block_terminated(then_block, false);
        }

        let mut current_else_block = else_block;

        for else_if in &typed_if.branches {
            let new_else_block = self.llvmctx.append_basic_block(current_func, "else_if");
            let new_then_block = self.llvmctx.append_basic_block(current_func, "else_if.then");

            self.llvmbuilder.position_at_end(current_else_block);
            let else_if_cond = self.build_expr(local_scope_opt.clone(), &else_if.condition);

            if !self.is_block_terminated(current_else_block) {
                self.llvmbuilder
                    .build_conditional_branch(
                        else_if_cond.as_basic_value().into_int_value(),
                        new_then_block,
                        new_else_block,
                    )
                    .unwrap();

                self.mark_block_terminated(current_else_block, false);
            }

            self.llvmbuilder.position_at_end(new_then_block);
            self.blockreg.current_block_ref = Some(new_then_block);
            self.build_block_statement(&else_if.consequent);
            if !self.is_block_terminated(new_then_block) {
                self.llvmbuilder.build_unconditional_branch(end_block).unwrap();
                self.mark_block_terminated(new_then_block, false);
            }

            current_else_block = new_else_block;
        }

        // build else block
        self.llvmbuilder.position_at_end(current_else_block);
        self.blockreg.current_block_ref = Some(current_else_block);
        if let Some(alternate) = &typed_if.alternate {
            self.build_block_statement(&alternate);
        }

        let current_block = self.blockreg.current_block_ref.unwrap();
        if !self.is_block_terminated(current_block) {
            self.llvmbuilder.build_unconditional_branch(end_block).unwrap();
            self.mark_block_terminated(current_block, false);
        }

        self.llvmbuilder.position_at_end(end_block);
        self.blockreg.current_block_ref = Some(end_block);
    }

    pub(crate) fn is_block_terminated(&self, basic_block: BasicBlock<'a>) -> bool {
        self.blockreg
            .terminated_blocks
            .iter()
            .find(|metadata| metadata.basic_block == basic_block)
            .is_some()
    }

    pub(crate) fn mark_block_terminated(&mut self, basic_block: BasicBlock<'a>, terminated_with_return: bool) {
        self.blockreg.terminated_blocks.push(TerminatedBlockMetadata {
            basic_block,
            terminated_with_return,
        });
    }
}
