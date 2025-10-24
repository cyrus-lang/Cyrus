use crate::builder::{
    module::{CodeGenBuilder, LocalIRValue, LoopBlockRefs, SwitchBlockRefs, TerminatedBlockMetadata},
    values::{InternalValue, InternalValueKind},
};
use ast::LiteralKind;
use inkwell::{
    basic_block::BasicBlock,
    values::{BasicValue, BasicValueEnum, IntValue},
};
use resolver::symbols::{LocalScopeRef, LocalSymbolKind, ResolvedEnum};
use std::collections::HashMap;
use typed_ast::{
    SymbolID, TypedBlockStmt, TypedBreakStmt, TypedContinueStmt, TypedEnumVariant, TypedExprStmt, TypedExprKind,
    TypedForStmt, TypedIfStmt, TypedReturnStmt, TypedSwitchStmt, TypedSwitchCasePattern, TypedWhileStmt,
    types::{BasicType, SemanticType},
};

#[derive(Debug, Clone)]
struct SwitchCaseItem<'a> {
    block_id: u32,
    value: InternalValue<'a>,
    body: Box<TypedBlockStmt>,
}

type SwitchCaseList<'a> = Vec<SwitchCaseItem<'a>>;

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
        $self.blockreg.current_loop_ref = Some(LoopBlockRefs { end_block, inc_block });

        $self.llvmbuilder.position_at_end(current_block);
        $self.llvmbuilder.build_unconditional_branch(cond_block).unwrap();
        $self.mark_block_terminated(current_block);

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

            $self.mark_block_terminated(cond_block);
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
            $self.mark_block_terminated(after_body_block);
        }

        $self.blockreg.current_block_ref = Some(end_block);
        $self.llvmbuilder.position_at_end(end_block);
        $self.blockreg.current_loop_ref = previous_loop_ref;
    }};
}

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_switch(&mut self, local_scope_opt: Option<LocalScopeRef>, switch: &TypedSwitchStmt) {
        let smart_switch = self.detect_smart_switch(switch);

        let lvalue = self.build_expr(local_scope_opt.clone(), &switch.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

        if let Some(enum_symbol_id) = rvalue.value_type.as_enum_symbol_id() {
            return self.build_switch_on_enum(local_scope_opt.clone(), switch, rvalue.clone(), enum_symbol_id);
        } else if let Some(generic_type) = rvalue.value_type.as_generic_type() {
            return self.build_switch_on_enum(local_scope_opt.clone(), switch, rvalue.clone(), generic_type.base);
        }

        // Temporarily remove reference to the loop-blocks. It's state must be returned after building the switch statement.
        let current_loop_ref_copy: Option<LoopBlockRefs<'a>> = self.blockreg.current_loop_ref.clone();
        self.blockreg.current_loop_ref = None;

        let case_list = self.collect_switch_case_list(local_scope_opt.clone(), switch);

        if case_list.len() == 0 {
            return;
        }

        if smart_switch {
            self.build_smart_switch(local_scope_opt.clone(), &rvalue, &case_list, &switch.default_case);
        } else {
            self.build_traditional_switch(&rvalue, &case_list, &switch.default_case);
        }

        self.blockreg.current_loop_ref = current_loop_ref_copy;
    }

    pub(crate) fn build_return(&mut self, local_scope_opt: Option<LocalScopeRef>, return_stmt: &TypedReturnStmt) {
        let current_block = self.blockreg.current_block_ref.unwrap();
        self.mark_block_terminated(current_block);

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

    pub(crate) fn build_continue(&mut self, _: &TypedContinueStmt) {
        let current_block = self.blockreg.current_block_ref.unwrap();

        let loop_end_block = match &self.blockreg.current_loop_ref {
            Some(loop_block_refs) => loop_block_refs.inc_block,
            None => {
                panic!("Invalid continue statement.");
            }
        };

        self.mark_block_terminated(current_block);
        self.llvmbuilder.build_unconditional_branch(loop_end_block).unwrap();
        self.llvmbuilder.position_at_end(loop_end_block);
    }

    pub(crate) fn build_break(&mut self, _: &TypedBreakStmt) {
        let current_block = self.blockreg.current_block_ref.unwrap();
        self.mark_block_terminated(current_block);

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

    pub(crate) fn build_conditional_for_statement(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        unevaluated_cond_value: &TypedExprStmt,
        body: &TypedBlockStmt,
        increment: Option<TypedExprStmt>,
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

    pub(crate) fn build_for(&mut self, local_scope_opt: Option<LocalScopeRef>, typed_for: &TypedForStmt) {
        // unconditional for loop
        if typed_for.condition.is_none() && typed_for.increment.is_none() {
            if let Some(initializer) = &typed_for.initializer {
                self.build_local_variable(local_scope_opt.clone(), initializer, true);
                self.build_infinite_for_statement(typed_for);
            } else {
                self.build_infinite_for_statement(typed_for);
            }
        } else if typed_for.increment.is_none() {
            self.build_local_variable(local_scope_opt.clone(), &typed_for.initializer.clone().unwrap(), true);

            self.build_conditional_for_statement(
                local_scope_opt,
                &typed_for.condition.clone().unwrap(),
                &typed_for.body,
                None,
            );
        } else {
            self.build_local_variable(local_scope_opt.clone(), &typed_for.initializer.clone().unwrap(), true);

            self.build_conditional_for_statement(
                local_scope_opt,
                &typed_for.condition.clone().unwrap(),
                &typed_for.body,
                Some(typed_for.increment.clone().unwrap()),
            );
        }
    }

    pub(crate) fn build_while(&mut self, local_scope_opt: Option<LocalScopeRef>, typed_while: &TypedWhileStmt) {
        build_loop_statement!(
            self,
            scope,
            {
                let cond_value = self.build_expr(local_scope_opt.clone(), &typed_while.condition);
                cond_value.as_basic_value().into_int_value()
            },
            {
                self.build_block_statement(&typed_while.body);
            },
            {}
        );
    }

    pub(crate) fn build_if(&mut self, local_scope_opt: Option<LocalScopeRef>, typed_if: &TypedIfStmt) {
        // important to build condition before building new blocks
        let cond_lvalue = self.build_expr(local_scope_opt.clone(), &typed_if.condition);
        let cond_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), cond_lvalue);
        let cond_i1 = cond_rvalue.as_basic_value().into_int_value();

        let current_block = self.blockreg.current_block_ref.unwrap();
        let current_func = self.blockreg.current_func_ref.unwrap();
        let end_block = self.llvmctx.append_basic_block(current_func, "if.end");

        let then_has_body = !typed_if.consequent.exprs.is_empty();
        let else_has_body = typed_if
            .alternate
            .as_ref()
            .map_or(false, |block_stmt| !block_stmt.exprs.is_empty());

        let then_block = if then_has_body {
            self.llvmctx.append_basic_block(current_func, "if.then")
        } else {
            end_block
        };

        let else_block = if else_has_body {
            self.llvmctx.append_basic_block(current_func, "if.else")
        } else {
            end_block
        };

        self.llvmbuilder.position_at_end(current_block);

        if then_block != else_block {
            self.llvmbuilder
                .build_conditional_branch(cond_i1, then_block, else_block)
                .unwrap();
            self.mark_block_terminated(current_block);
        } else {
            self.llvmbuilder.build_unconditional_branch(end_block).unwrap();
            self.mark_block_terminated(current_block);
        }

        if then_block != end_block {
            self.llvmbuilder.position_at_end(then_block);
            self.blockreg.current_block_ref = Some(then_block);

            self.build_block_statement(&typed_if.consequent);

            let then_insert = self.llvmbuilder.get_insert_block().unwrap();
            if then_insert.get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(end_block).unwrap();
                self.mark_block_terminated(then_block);
            }
        }

        let mut current_else_block = else_block;

        for else_if in &typed_if.branches {
            // create blocks for this else-if pair
            let new_else_block = self.llvmctx.append_basic_block(current_func, "else_if");
            let new_then_block = self.llvmctx.append_basic_block(current_func, "else_if.then");

            // finish off previous else-like block by emitting cond -> (new_then, new_else)
            self.llvmbuilder.position_at_end(current_else_block);
            if !self.is_block_terminated(current_else_block) {
                let else_if_cond_lvalue = self.build_expr(local_scope_opt.clone(), &else_if.condition);
                let else_if_cond_rvalue =
                    self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), else_if_cond_lvalue);
                let else_if_i1 = else_if_cond_rvalue.as_basic_value().into_int_value();

                self.llvmbuilder
                    .build_conditional_branch(else_if_i1, new_then_block, new_else_block)
                    .unwrap();
                self.mark_block_terminated(current_else_block);
            }

            // emit the else-if's then body
            self.llvmbuilder.position_at_end(new_then_block);
            self.blockreg.current_block_ref = Some(new_then_block);
            self.build_block_statement(&else_if.consequent);

            let then_insert = self.llvmbuilder.get_insert_block().unwrap();
            if then_insert.get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(end_block).unwrap();
                self.mark_block_terminated(new_then_block);
            }

            current_else_block = new_else_block;
        }

        if current_else_block != end_block {
            self.llvmbuilder.position_at_end(current_else_block);
            self.blockreg.current_block_ref = Some(current_else_block);
            if let Some(else_block) = &typed_if.else_block {
                self.build_block_statement(&alternate);
            }

            let else_insert = self.llvmbuilder.get_insert_block().unwrap();
            if else_insert.get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(end_block).unwrap();
                self.mark_block_terminated(current_else_block);
            }
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

    pub(crate) fn mark_block_terminated(&mut self, basic_block: BasicBlock<'a>) {
        self.blockreg
            .terminated_blocks
            .push(TerminatedBlockMetadata { basic_block });
    }

    fn detect_smart_switch(&self, switch: &TypedSwitchStmt) -> bool {
        for case in &switch.cases {
            match &case.pattern {
                TypedSwitchCasePattern::Expression(typed_expr, _) => match &typed_expr.kind {
                    TypedExprKind::Literal(typed_literal) => match &typed_literal.kind {
                        LiteralKind::Integer(..) | LiteralKind::Bool(..) | LiteralKind::Char(..) => continue,
                        LiteralKind::Float(..) | LiteralKind::String(..) | LiteralKind::Null => return true,
                    },
                    _ => return true,
                },
                TypedSwitchCasePattern::Identifier(..) => return true,
                TypedSwitchCasePattern::EnumVariant(..) => return true,
            }
        }
        false
    }

    fn collect_switch_on_enum_case_list(
        &mut self,
        switch: &TypedSwitchStmt,
        resolved_enum: &ResolvedEnum,
        operand_rvalue: InternalValue<'a>,
    ) -> SwitchCaseList<'a> {
        let mut case_list: SwitchCaseList<'a> = Vec::new();
        let mut block_id = 0;

        for case in &switch.cases {
            let case_int_value = match &case.pattern {
                TypedSwitchCasePattern::Expression(..) => unreachable!(),
                TypedSwitchCasePattern::Identifier(identifier, _) => {
                    let variant_idx = resolved_enum
                        .enum_sig
                        .variants
                        .iter()
                        .position(|variant| variant.get_identifier().as_string() == *identifier)
                        .unwrap();

                    self.llvmctx
                        .i32_type()
                        .const_int(variant_idx.try_into().unwrap(), false)
                }
                TypedSwitchCasePattern::EnumVariant(identifier, valued_fields, _) => {
                    let local_scope_rc = self.resolver.get_scope_ref(self.module_id, case.body.scope_id).unwrap();
                    let local_scope = local_scope_rc.borrow();

                    let variant_idx = resolved_enum
                        .enum_sig
                        .variants
                        .iter()
                        .position(|variant| variant.get_identifier().as_string() == *identifier)
                        .unwrap();

                    let variant_idx_int_value = self
                        .llvmctx
                        .i32_type()
                        .const_int(variant_idx.try_into().unwrap(), false);

                    let enum_struct_type = if let Some(generic_type) = operand_rvalue.value_type.as_generic_type() {
                        self.get_or_declare_enum_monomorph(resolved_enum, &Some(generic_type.type_args.clone()))
                            .0
                    } else {
                        let enum_valued_fields = match &resolved_enum.enum_sig.variants[variant_idx] {
                            TypedEnumVariant::Variant(_, enum_valued_fields) => enum_valued_fields,
                            _ => unreachable!(),
                        };

                        self.build_enum_valued_field_variant_struct_type(
                            Some(local_scope_rc.clone()),
                            enum_valued_fields.clone(),
                        )
                    };

                    let enum_struct_value = operand_rvalue.as_basic_value().into_struct_value();
                    let buffer = self.build_enum_extract_payload(enum_struct_value);
                    let payload_struct_value = self.intrinsic_copy_buffer_to_struct(buffer, enum_struct_type);
                    let payload_struct_type = payload_struct_value.get_type();
                    let payload_alloca = self.llvmbuilder.build_alloca(payload_struct_type, "alloca").unwrap();
                    self.llvmbuilder
                        .build_store(payload_alloca, payload_struct_value)
                        .unwrap();

                    for (valued_field_idx, valued_field) in valued_fields.iter().enumerate() {
                        let local_symbol = local_scope.resolve_with_symbol_id(valued_field.symbol_id).unwrap();
                        let resolved_variable = match &local_symbol.kind {
                            LocalSymbolKind::Variable(resolved_variable) => resolved_variable,
                            _ => unreachable!(),
                        };

                        let variable_type = resolved_variable.typed_variable.ty.clone().unwrap();

                        let field_pointer = self
                            .llvmbuilder
                            .build_struct_gep(
                                payload_struct_type,
                                payload_alloca,
                                valued_field_idx.try_into().unwrap(),
                                "struct_gep",
                            )
                            .unwrap();

                        self.insert_ir_value(
                            valued_field.symbol_id,
                            LocalIRValue::LValue(field_pointer, variable_type),
                        );
                    }

                    drop(local_scope);
                    variant_idx_int_value
                }
            };

            let case_internal_value = InternalValue::new(
                SemanticType::BasicType(BasicType::Int32),
                InternalValueKind::RValue(case_int_value.as_basic_value_enum()),
            );

            case_list.push(SwitchCaseItem {
                block_id,
                value: case_internal_value,
                body: case.body.clone(),
            });

            if !case.body.exprs.is_empty() {
                block_id += 1;
            }
        }
        case_list
    }

    fn build_switch_on_enum(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        switch: &TypedSwitchStmt,
        operand_rvalue: InternalValue<'a>,
        enum_symbol_id: SymbolID,
    ) {
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), enum_symbol_id)
            .unwrap();
        let resolved_enum = sym.as_enum().unwrap();
        let case_list = self.collect_switch_on_enum_case_list(switch, &resolved_enum, operand_rvalue.clone());

        if case_list.len() == 0 {
            return;
        }

        let current_func = self.blockreg.current_func_ref.unwrap();

        let parent_switch_copy = self.blockreg.current_switch.clone();
        let parent_block = self.blockreg.current_block_ref.clone().unwrap();

        let exit_block = self.llvmctx.append_basic_block(current_func, "switch.exit");

        let target_block = if let Some(block_statement) = switch.default_case.clone() {
            let default_block = self.llvmctx.append_basic_block(current_func, "switch.default");
            self.blockreg.current_block_ref = Some(default_block.clone());
            self.llvmbuilder.position_at_end(default_block);
            self.blockreg.current_switch = Some(SwitchBlockRefs { exit_block });
            self.build_block_statement(&block_statement);
            default_block
        } else {
            self.blockreg.current_switch = Some(SwitchBlockRefs { exit_block });
            exit_block
        };

        let mut case_blocks: HashMap<u32, BasicBlock<'a>> = HashMap::new();
        let max_block_id = case_list.iter().last().unwrap().block_id;

        for block_id in 0..(max_block_id + 1) {
            let case_block = self.llvmctx.append_basic_block(current_func, "switch.case");
            case_blocks.insert(block_id, case_block);
        }

        for block_id in 0..(max_block_id + 1) {
            let group = self.select_switch_grouped_cases(block_id, case_list.clone());
            let group_body = &group.iter().last().unwrap().body;

            let case_block = case_blocks.get(&block_id).unwrap();
            self.llvmbuilder.position_at_end(*case_block);
            self.blockreg.current_block_ref = Some(*case_block);
            self.build_block_statement(group_body);

            // Jump to the exit block if this is the last case, otherwise jump to the next case.
            let current_block = self.blockreg.current_block_ref.clone().unwrap();
            self.llvmbuilder.position_at_end(current_block);

            if !self.is_block_terminated(current_block) {
                match case_list.iter().find(|case| case.block_id == block_id + 1) {
                    Some(next_case) => {
                        self.mark_block_terminated(current_block);
                        let next_case_block = case_blocks.get(&next_case.block_id).unwrap();
                        self.llvmbuilder.build_unconditional_branch(*next_case_block).unwrap();
                        self.llvmbuilder.position_at_end(*next_case_block);
                    }
                    None => {}
                }
            }
        }

        // Check if ending case doesn't have a terminator...
        let current_block = self.blockreg.current_block_ref.clone().unwrap();
        if !self.is_block_terminated(current_block) {
            self.mark_block_terminated(current_block);
            self.llvmbuilder.build_unconditional_branch(target_block).unwrap();
            self.llvmbuilder.position_at_end(target_block);
        }

        let ir_cases: Vec<(IntValue<'a>, BasicBlock<'a>)> = case_list
            .iter()
            .map(|case| {
                let basic_block = case_blocks.get(&case.block_id).cloned().unwrap();
                (case.value.as_basic_value().into_int_value(), basic_block)
            })
            .collect();

        self.llvmbuilder.position_at_end(parent_block);

        // load index of the enum variant as operand
        let enum_struct_value = operand_rvalue.as_basic_value().into_struct_value();
        let enum_variant_idx_int_value = self.build_enum_extract_index(enum_struct_value);

        self.llvmbuilder
            .build_switch(enum_variant_idx_int_value, target_block, &ir_cases)
            .unwrap();

        if switch.default_case.is_some() {
            self.llvmbuilder.position_at_end(target_block);
            if !self.is_block_terminated(target_block) {
                self.llvmbuilder.position_at_end(target_block);
                self.mark_block_terminated(target_block);
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
            }
        }

        self.llvmbuilder.position_at_end(exit_block);
        self.blockreg.current_block_ref = Some(exit_block);
        self.blockreg.current_switch = parent_switch_copy;
    }

    fn collect_switch_case_list(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        switch: &TypedSwitchStmt,
    ) -> SwitchCaseList<'a> {
        let mut case_list: SwitchCaseList<'a> = Vec::new();
        let mut block_id = 0;
        for case in &switch.cases {
            let case_pattern_expr = match &case.pattern {
                TypedSwitchCasePattern::Expression(typed_expr, _) => typed_expr,
                _ => unreachable!(),
            };

            let case_lvalue = self.build_expr(local_scope_opt.clone(), case_pattern_expr);
            let case_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), case_lvalue);

            case_list.push(SwitchCaseItem {
                block_id,
                value: case_rvalue,
                body: case.body.clone(),
            });

            if !case.body.exprs.is_empty() {
                block_id += 1;
            }
        }
        case_list
    }

    fn build_traditional_switch(
        &mut self,
        operand: &InternalValue<'a>,
        case_list: &SwitchCaseList<'a>,
        default_case: &Option<TypedBlockStmt>,
    ) {
        let current_func = self.blockreg.current_func_ref.unwrap();

        let parent_switch_copy = self.blockreg.current_switch.clone();
        let parent_block = self.blockreg.current_block_ref.clone().unwrap();

        let exit_block = self.llvmctx.append_basic_block(current_func, "switch.exit");

        let target_block = if let Some(block_statement) = default_case.clone() {
            let default_block = self.llvmctx.append_basic_block(current_func, "switch.default");
            self.blockreg.current_block_ref = Some(default_block.clone());
            self.llvmbuilder.position_at_end(default_block);
            self.blockreg.current_switch = Some(SwitchBlockRefs { exit_block });
            self.build_block_statement(&block_statement);
            default_block
        } else {
            self.blockreg.current_switch = Some(SwitchBlockRefs { exit_block });
            exit_block
        };

        let mut case_blocks: HashMap<u32, BasicBlock<'a>> = HashMap::new();
        let max_block_id = case_list.iter().last().unwrap().block_id;

        for block_id in 0..(max_block_id + 1) {
            let case_block = self.llvmctx.append_basic_block(current_func, "switch.case");
            case_blocks.insert(block_id, case_block);
        }

        for block_id in 0..(max_block_id + 1) {
            let group = self.select_switch_grouped_cases(block_id, case_list.clone());
            let group_body = &group.iter().last().unwrap().body;

            let case_block = case_blocks.get(&block_id).unwrap();
            self.llvmbuilder.position_at_end(*case_block);
            self.blockreg.current_block_ref = Some(*case_block);
            self.build_block_statement(group_body);

            // Jump to the exit block if this is the last case, otherwise jump to the next case.
            let current_block = self.blockreg.current_block_ref.clone().unwrap();
            self.llvmbuilder.position_at_end(current_block);

            if !self.is_block_terminated(current_block) {
                match case_list.iter().find(|case| case.block_id == block_id + 1) {
                    Some(next_case) => {
                        self.mark_block_terminated(current_block);
                        let next_case_block = case_blocks.get(&next_case.block_id).unwrap();
                        self.llvmbuilder.build_unconditional_branch(*next_case_block).unwrap();
                        self.llvmbuilder.position_at_end(*next_case_block);
                    }
                    None => {}
                }
            }
        }

        // Check if ending case doesn't have a terminator...
        let current_block = self.blockreg.current_block_ref.clone().unwrap();
        if !self.is_block_terminated(current_block) {
            self.mark_block_terminated(current_block);
            self.llvmbuilder.build_unconditional_branch(target_block).unwrap();
            self.llvmbuilder.position_at_end(target_block);
        }

        let ir_cases: Vec<(IntValue<'a>, BasicBlock<'a>)> = case_list
            .iter()
            .map(|case| {
                let basic_block = case_blocks.get(&case.block_id).cloned().unwrap();
                (case.value.as_basic_value().into_int_value(), basic_block)
            })
            .collect();

        self.llvmbuilder.position_at_end(parent_block);
        self.llvmbuilder
            .build_switch(operand.as_basic_value().into_int_value(), target_block, &ir_cases)
            .unwrap();

        if default_case.is_some() {
            self.llvmbuilder.position_at_end(target_block);
            if !self.is_block_terminated(target_block) {
                self.llvmbuilder.position_at_end(target_block);
                self.mark_block_terminated(target_block);
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
            }
        }

        self.llvmbuilder.position_at_end(exit_block);
        self.blockreg.current_block_ref = Some(exit_block);
        self.blockreg.current_switch = parent_switch_copy;
    }

    fn build_smart_switch(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        operand: &InternalValue<'a>,
        case_list: &SwitchCaseList<'a>,
        default_case: &Option<TypedBlockStmt>,
    ) {
        let current_func = self.blockreg.current_func_ref.unwrap();

        let parent_switch_copy = self.blockreg.current_switch.clone();
        let parent_block = self.blockreg.current_block_ref.clone().unwrap();

        let exit_block = self.llvmctx.append_basic_block(current_func, "smart_switch.exit");
        let target_block = if let Some(block_statement) = default_case.clone() {
            let default_block = self.llvmctx.append_basic_block(current_func, "smart_switch.default");
            self.blockreg.current_block_ref = Some(default_block.clone());
            self.llvmbuilder.position_at_end(default_block);
            self.blockreg.current_switch = Some(SwitchBlockRefs { exit_block });
            self.build_block_statement(&block_statement);
            default_block
        } else {
            self.blockreg.current_switch = Some(SwitchBlockRefs { exit_block });
            exit_block
        };

        let mut case_blocks: HashMap<u32, BasicBlock<'a>> = HashMap::new();
        let max_block_id = case_list.iter().last().unwrap().block_id;

        for block_id in 0..(max_block_id + 1) {
            let case_block = self.llvmctx.append_basic_block(current_func, "smart_switch.case");
            case_blocks.insert(block_id, case_block);
        }

        let mut starting_check_block: Option<BasicBlock<'a>> = None;
        let mut groups: HashMap<
            u32,
            (
                BasicBlock<'a>,                           // case_block
                Vec<(InternalValue<'a>, BasicBlock<'a>)>, // cond, check_block
            ),
        > = HashMap::new();

        for block_id in 0..(max_block_id + 1) {
            let group = self.select_switch_grouped_cases(block_id, case_list.clone());
            let group_body = &group.iter().last().unwrap().body;

            // Build Case Block
            let case_block = case_blocks.get(&block_id).unwrap();
            self.llvmbuilder.position_at_end(*case_block);
            self.blockreg.current_block_ref = Some(*case_block);
            self.build_block_statement(&group_body);

            let current_block = self.blockreg.current_block_ref.clone().unwrap();
            if !self.is_block_terminated(current_block) {
                match case_list.iter().find(|case| case.block_id == block_id + 1) {
                    Some(next_case) => {
                        self.llvmbuilder.position_at_end(current_block);
                        self.mark_block_terminated(current_block);
                        let next_case_block = case_blocks.get(&next_case.block_id).unwrap();
                        self.llvmbuilder.build_unconditional_branch(*next_case_block).unwrap();
                        self.llvmbuilder.position_at_end(*next_case_block);
                    }
                    None => {
                        self.mark_block_terminated(current_block);
                        self.llvmbuilder.build_unconditional_branch(target_block).unwrap();
                        self.llvmbuilder.position_at_end(target_block);
                    }
                }
            }

            let current_group_check_blocks: Vec<(InternalValue<'a>, BasicBlock<'a>)> = group
                .iter()
                .map(|switch_case_item| {
                    let case_group_check_block = self
                        .llvmctx
                        .append_basic_block(current_func, "smart_switch.group.check");

                    (switch_case_item.value.clone(), case_group_check_block)
                })
                .collect();

            if starting_check_block.is_none() {
                starting_check_block = Some(current_group_check_blocks.first().unwrap().1.clone());
            }

            groups.insert(block_id, (case_block.clone(), current_group_check_blocks));
        }

        for (group_idx, (case_block, group_check_blocks)) in groups.iter() {
            // Build Check Blocks
            for idx in 0..group_check_blocks.len() {
                let (internal_value, check_block) = group_check_blocks.get(idx).unwrap();

                // Current group_check_blocks may not contain another check_block, Hence we gotta retrieve
                // the next group and retrieve it's group_check_blocks and extract the next_check_block.
                let next_check_block = match group_check_blocks.get(idx + 1) {
                    Some((_, basic_block)) => basic_block.clone(),
                    None => {
                        // REVIEW Consider to move this part to the higher scope to prevent redundant lookup.
                        let next_group_id = group_idx + 1;
                        let basic_block = match groups.get(&next_group_id) {
                            Some(next_group) => {
                                let group_check_blocks = next_group.1.clone();
                                match group_check_blocks.first() {
                                    Some(first_check_block) => first_check_block.1,
                                    None => target_block,
                                }
                            }
                            None => target_block,
                        };
                        basic_block
                    }
                };

                self.llvmbuilder.position_at_end(check_block.clone());
                let condition_internal_value = {
                    let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), internal_value.clone());
                    let operand_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), operand.clone());

                    self.build_cmp_eq(local_scope_opt.clone(), rvalue, operand_rvalue)
                };
                let condition_basic_value: BasicValueEnum<'a> = condition_internal_value.as_basic_value();

                self.llvmbuilder
                    .build_conditional_branch(
                        condition_basic_value.into_int_value(),
                        *case_block,
                        next_check_block.clone(),
                    )
                    .unwrap();
            }
        }

        // Check if ending case doesn't have a terminator...
        let current_block = self.blockreg.current_block_ref.clone().unwrap();
        if !self.is_block_terminated(current_block) {
            self.mark_block_terminated(current_block);
            self.llvmbuilder.build_unconditional_branch(target_block).unwrap();
            self.llvmbuilder.position_at_end(target_block);
        }

        // Starting point of the switch statement which retrieves first check_case and it's condition,
        // And builds the first comparison instruction and jumps to the linked blocks.
        {
            self.llvmbuilder.position_at_end(parent_block);

            match starting_check_block {
                Some(starting_check_block) => {
                    self.llvmbuilder
                        .build_unconditional_branch(starting_check_block)
                        .unwrap();
                }
                None => {
                    self.llvmbuilder.build_unconditional_branch(target_block).unwrap();
                }
            }
        }

        if default_case.is_some() {
            self.llvmbuilder.position_at_end(target_block);
            if !self.is_block_terminated(target_block) {
                self.llvmbuilder.position_at_end(target_block);
                self.mark_block_terminated(target_block);
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
            }
        }

        self.llvmbuilder.position_at_end(exit_block);
        self.blockreg.current_block_ref = Some(exit_block);
        self.blockreg.current_switch = parent_switch_copy;
    }

    fn select_switch_grouped_cases(&self, block_id: u32, case_list: SwitchCaseList<'a>) -> SwitchCaseList<'a> {
        case_list
            .iter()
            .filter(|case| case.block_id == block_id)
            .cloned()
            .collect()
    }

    fn build_infinite_for_statement(&mut self, for_stmt: &TypedForStmt) {
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
}
