use super::module::{CodeGenBuilder, LocalIRValue};
use crate::builder::{
    abi::{generate_enum_abi_name, generate_struct_abi_name, generate_union_abi_name},
    module::{LoopBlockRefs, SwitchBlockRefs, TerminatedBlockMetadata},
    values::InternalValue,
};
use ast::LiteralKind;
use inkwell::{
    basic_block::BasicBlock,
    module::Linkage,
    types::{AnyType, BasicTypeEnum, StructType},
    values::{BasicValueEnum, FunctionValue, IntValue},
};
use resolver::{
    declsign::{FuncSig, StructSig},
    scope::LocalScopeRef,
};
use std::collections::HashMap;
use typed_ast::{
    ModuleID, SymbolID, TypedBlockStatement, TypedBreak, TypedContinue, TypedExpression, TypedExpressionKind, TypedFor,
    TypedFuncVariadicParams, TypedIf, TypedReturn, TypedStatement, TypedStruct, TypedSwitch, TypedSwitchCasePattern,
    TypedUnion, TypedWhile,
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
    pub(crate) fn build_toplevel_statements(&mut self, stmts: &Vec<TypedStatement>) {
        self.build_forward_decls(stmts);

        for stmt in stmts {
            match stmt {
                TypedStatement::GlobalVariable(typed_global_var) => self.build_global_var_def(typed_global_var),
                TypedStatement::FuncDef(typed_func_def) => self.build_func_def(typed_func_def),
                TypedStatement::Struct(typed_struct) => self.build_struct_def(typed_struct),
                TypedStatement::Enum(typed_enum) => self.build_enum_def(typed_enum),
                TypedStatement::Union(typed_union) => self.build_union_def(typed_union),
                TypedStatement::Interface(_typed_interface) => todo!(),
                TypedStatement::FuncDecl(_) => continue,
                _ => continue,
            }
        }
    }

    pub(crate) fn get_or_declare_func(&mut self, symbol_id: SymbolID, func_sig: FuncSig) -> FunctionValue<'a> {
        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&symbol_id).cloned();
        drop(irreg);

        let fn_value = match local_ir_value {
            Some(local_ir_value) => local_ir_value.as_func().unwrap().clone(),
            None => {
                let fn_value = self.build_func_decl(
                    func_sig.name.clone(),
                    func_sig.is_func_decl,
                    Some(func_sig.module_id),
                    func_sig.params.clone(),
                    func_sig.return_type.clone(),
                    func_sig.vis.clone(),
                );
                fn_value.set_linkage(Linkage::External);

                self.insert_forward_decl_to_registry(symbol_id, LocalIRValue::Func(fn_value));
                fn_value
            }
        };

        fn_value
    }

    pub(crate) fn get_or_declare_struct(&mut self, symbol_id: SymbolID, struct_sig: &StructSig) -> StructType<'a> {
        let irreg = self.irreg.borrow();
        let local_ir_value_opt = irreg.get(&symbol_id).cloned();
        drop(irreg);

        match local_ir_value_opt {
            Some(local_ir_value) => *local_ir_value.as_struct().unwrap(),
            None => self.build_struct_only_type(struct_sig),
        }
    }

    pub(crate) fn insert_forward_decl_to_registry(&mut self, symbol_id: SymbolID, local_value: LocalIRValue<'a>) {
        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(symbol_id, local_value);
        drop(irreg);
    }

    fn build_forward_decls(&mut self, stmts: &Vec<TypedStatement>) {
        for stmt in stmts {
            match stmt {
                TypedStatement::Struct(typed_struct) => {
                    let struct_type = self.build_struct_decl(&typed_struct.name);
                    self.insert_forward_decl_to_registry(typed_struct.symbol_id, LocalIRValue::Struct(struct_type));
                }
                TypedStatement::Enum(typed_enum) => {
                    let struct_type = self.build_enum_decl(&typed_enum.name);
                    self.insert_forward_decl_to_registry(typed_enum.symbol_id, LocalIRValue::Struct(struct_type));
                }
                TypedStatement::Union(typed_union) => {
                    let struct_type = self.build_union_decl(&typed_union.name);
                    self.insert_forward_decl_to_registry(typed_union.symbol_id, LocalIRValue::Struct(struct_type));
                }
                TypedStatement::Interface(_typed_interface) => todo!(),
                _ => continue,
            }
        }

        for stmt in stmts {
            match stmt {
                TypedStatement::GlobalVariable(typed_global_var) => {
                    let global_value = self.build_global_var_decl(typed_global_var);
                    self.insert_forward_decl_to_registry(
                        typed_global_var.symbol_id,
                        LocalIRValue::GlobalValue(global_value, typed_global_var.ty.clone().unwrap()),
                    );
                }
                TypedStatement::FuncDef(typed_func_def) => {
                    let fn_value = self.build_func_decl(
                        typed_func_def.name.clone(),
                        false,
                        Some(typed_func_def.module_id),
                        typed_func_def.params.clone(),
                        typed_func_def.return_type.clone(),
                        typed_func_def.vis.clone(),
                    );
                    self.insert_forward_decl_to_registry(typed_func_def.symbol_id, LocalIRValue::Func(fn_value));
                }
                TypedStatement::FuncDecl(typed_func_decl) => {
                    let fn_value = self.build_func_decl(
                        typed_func_decl.name.clone(),
                        true,
                        None,
                        typed_func_decl.params.clone(),
                        typed_func_decl.return_type.clone(),
                        typed_func_decl.vis.clone(),
                    );
                    self.insert_forward_decl_to_registry(typed_func_decl.symbol_id, LocalIRValue::Func(fn_value));
                }
                _ => continue,
            }
        }
    }

    fn build_enum_decl(&self, name: &String) -> StructType<'a> {
        let module_name = self.get_module_name(self.module_id);
        self.llvmctx
            .opaque_struct_type(&generate_enum_abi_name(module_name, name.to_string()))
    }

    fn build_union_decl(&self, name: &String) -> StructType<'a> {
        let module_name = self.get_module_name(self.module_id);
        self.llvmctx
            .opaque_struct_type(&generate_union_abi_name(module_name, name.to_string()))
    }

    fn build_struct_decl(&self, name: &String) -> StructType<'a> {
        let module_name = self.get_module_name(self.module_id);
        self.llvmctx
            .opaque_struct_type(&generate_struct_abi_name(module_name, name.to_string()))
    }

    fn build_local_struct_def(&mut self, typed_struct: &TypedStruct) {
        let field_types: Vec<BasicTypeEnum<'a>> = typed_struct
            .fields
            .iter()
            .map(|field| self.build_concrete_type(None, field.ty.clone()).try_into().unwrap())
            .collect();

        let struct_type = self.llvmctx.struct_type(&field_types, typed_struct.packed);

        self.insert_forward_decl_to_registry(typed_struct.symbol_id, LocalIRValue::Struct(struct_type));
    }

    fn build_methods(&mut self, module_id: ModuleID, methods: &HashMap<String, SymbolID>) {
        for method_symbol_id in methods.values() {
            let symbol_entry = self
                .resolver
                .lookup_symbol_entry_with_id(module_id, *method_symbol_id)
                .unwrap();

            let resolved_method = symbol_entry.as_method().unwrap().clone();

            let fn_value = self.get_or_declare_func(*method_symbol_id, resolved_method.func_sig.clone());

            self.insert_forward_decl_to_registry(*method_symbol_id, LocalIRValue::Func(fn_value));

            self.blockreg.current_func_ref = Some(fn_value.clone());

            let entry_block = self.llvmctx.append_basic_block(fn_value, "entry");
            self.blockreg.current_block_ref = Some(entry_block);
            self.llvmbuilder.position_at_end(entry_block);

            let local_scope_opt = self
                .resolver
                .get_scope_ref(module_id, resolved_method.func_body.clone().unwrap().scope_id);

            self.build_func_params(local_scope_opt, &resolved_method.func_sig.params, fn_value);

            if let Some(variadic_params) = &resolved_method.func_sig.params.variadic {
                if let TypedFuncVariadicParams::Typed(_, _) = variadic_params {
                    todo!();
                }
            }

            self.build_block_statement(&resolved_method.func_body.clone().unwrap());

            let current_block = self.blockreg.current_block_ref.unwrap();
            if !self.is_block_terminated(current_block) {
                self.llvmbuilder.build_return(None).unwrap();
            }
        }
    }

    fn build_struct_only_type(&mut self, struct_sig: &StructSig) -> StructType<'a> {
        let field_types: Vec<BasicTypeEnum<'a>> = struct_sig
            .fields
            .iter()
            .map(|field| self.build_concrete_type(None, field.ty.clone()).try_into().unwrap())
            .collect();

        let struct_type = self.llvmctx.struct_type(&field_types, struct_sig.packed);
        struct_type.set_body(&field_types, struct_sig.packed);
        struct_type
    }

    fn build_struct_def(&mut self, typed_struct: &TypedStruct) {
        let field_types: Vec<BasicTypeEnum<'a>> = typed_struct
            .fields
            .iter()
            .map(|field| self.build_concrete_type(None, field.ty.clone()).try_into().unwrap())
            .collect();

        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&typed_struct.symbol_id).unwrap();

        let struct_type = local_ir_value.as_struct().unwrap().clone();
        drop(irreg);

        struct_type.set_body(&field_types, typed_struct.packed);
        self.build_methods(typed_struct.module_id, &typed_struct.methods);
    }

    fn build_union_def(&mut self, typed_union: &TypedUnion) {
        let field_types: Vec<BasicTypeEnum<'a>> = typed_union
            .fields
            .iter()
            .map(|field| self.build_concrete_type(None, field.ty.clone()).try_into().unwrap())
            .collect();

        let mut largest_field_type: BasicTypeEnum = BasicTypeEnum::IntType(self.llvmctx.bool_type());

        field_types.iter().for_each(|basic_type| {
            let largest_store_size = self
                .llvmtm
                .get_target_data()
                .get_store_size(&largest_field_type.as_any_type_enum());

            let field_store_size = self
                .llvmtm
                .get_target_data()
                .get_store_size(&basic_type.as_any_type_enum());

            if field_store_size > largest_store_size {
                largest_field_type = basic_type.clone();
            }
        });

        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&typed_union.symbol_id).unwrap();

        let struct_type = local_ir_value.as_struct().unwrap().clone();
        drop(irreg);

        struct_type.set_body(&[largest_field_type], false);
        self.build_methods(typed_union.module_id, &typed_union.methods);
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
                TypedStatement::While(typed_while) => self.build_while(local_scope_opt.clone(), typed_while),
                TypedStatement::Return(typed_return) => self.build_return(local_scope_opt.clone(), typed_return),
                TypedStatement::Break(typed_break) => self.build_break(typed_break),
                TypedStatement::Continue(typed_continue) => self.build_continue(typed_continue),
                TypedStatement::Switch(typed_switch) => self.build_switch(local_scope_opt.clone(), typed_switch),
                TypedStatement::Struct(typed_struct) => {
                    self.build_local_struct_def(typed_struct);
                }
                TypedStatement::Enum(typed_enum) => {
                    self.build_local_enum_def(typed_enum);
                }
                TypedStatement::Union(typed_union) => {
                    self.build_union_def(typed_union);
                }
                TypedStatement::Expression(typed_expr) => {
                    let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
                    self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
                }
                TypedStatement::BlockStatement(typed_block_statement) => {
                    self.build_block_statement(typed_block_statement);
                }
                TypedStatement::Interface(_typed_interface) => todo!(),
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

    // Analyze switch to determine it must be compiled as traditional-switch or smart-switch.
    fn detect_smart_switch(&self, switch: &TypedSwitch) -> bool {
        for case in &switch.cases {
            if case.patterns.len() > 1 {
                return true;
            }

            for pattern in &case.patterns {
                match pattern {
                    TypedSwitchCasePattern::Expression(typed_expr) => match &typed_expr.kind {
                        TypedExpressionKind::Literal(typed_literal) => match &typed_literal.kind {
                            LiteralKind::Integer(..) | LiteralKind::Bool(..) | LiteralKind::Char(..) => continue,
                            LiteralKind::Float(..) | LiteralKind::String(..) | LiteralKind::Null => return true,
                        },
                        _ => return true,
                    },
                    TypedSwitchCasePattern::Identifier(..) => return true,
                    TypedSwitchCasePattern::EnumVariant(..) => return true,
                }
            }
        }

        false
    }

    fn build_switch(&mut self, local_scope_opt: Option<LocalScopeRef>, switch: &TypedSwitch) {
        let smart_switch = self.detect_smart_switch(switch);

        let lvalue = self.build_expr(local_scope_opt.clone(), &switch.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

        // TODO build_enum_pattern_matching

        let mut case_list: SwitchCaseList<'a> = Vec::new();
        let mut block_id = 0;

        // Temporarily remove reference to the loop-blocks. It's state must be returned after building the switch statement.
        let current_loop_ref_copy: Option<LoopBlockRefs<'a>> = self.blockreg.current_loop_ref.clone();
        self.blockreg.current_loop_ref = None;

        for case in &switch.cases {
            for pattern in &case.patterns {
                let case_pattern_expr = match pattern {
                    TypedSwitchCasePattern::Expression(typed_expr) => typed_expr,
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
        }

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

    fn build_traditional_switch(
        &mut self,
        operand: &InternalValue<'a>,
        case_list: &SwitchCaseList<'a>,
        default_case: &Option<TypedBlockStatement>,
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
        default_case: &Option<TypedBlockStatement>,
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

                    self.build_cmp_eq(rvalue, operand_rvalue)
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

    fn build_return(&mut self, local_scope_opt: Option<LocalScopeRef>, return_stmt: &TypedReturn) {
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

    fn build_continue(&mut self, _: &TypedContinue) {
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

    fn build_break(&mut self, _: &TypedBreak) {
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

    fn build_while(&mut self, local_scope_opt: Option<LocalScopeRef>, typed_while: &TypedWhile) {
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
            self.mark_block_terminated(current_block);
        }

        // build then block
        self.llvmbuilder.position_at_end(then_block);
        self.blockreg.current_block_ref = Some(then_block);
        self.build_block_statement(&typed_if.consequent);
        if !self.is_block_terminated(then_block) {
            self.llvmbuilder.build_unconditional_branch(end_block).unwrap();
            self.mark_block_terminated(then_block);
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

                self.mark_block_terminated(current_else_block);
            }

            self.llvmbuilder.position_at_end(new_then_block);
            self.blockreg.current_block_ref = Some(new_then_block);
            self.build_block_statement(&else_if.consequent);
            if !self.is_block_terminated(new_then_block) {
                self.llvmbuilder.build_unconditional_branch(end_block).unwrap();
                self.mark_block_terminated(new_then_block);
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
            self.mark_block_terminated(current_block);
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
}

#[derive(Debug, Clone)]
struct SwitchCaseItem<'a> {
    block_id: u32,
    value: InternalValue<'a>,
    body: Box<TypedBlockStatement>,
}

type SwitchCaseList<'a> = Vec<SwitchCaseItem<'a>>;
