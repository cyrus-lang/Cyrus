use crate::context::CodeGenLLVM;
use crate::diag::*;
use crate::funcs::FuncMetadata;
use crate::scope::ScopeRef;
use crate::scope::{Scope, ScopeRecord};
use crate::types::{InternalBoolType, InternalIntType, InternalType};
use crate::values::InternalValue;
use ast::ast::{BlockStatement, Break, Continue, Expression, For, Foreach, If, Statement, Switch, TypeSpecifier};
use ast::token::{Location, Span, Token, TokenKind};
use inkwell::AddressSpace;
use inkwell::basic_block::BasicBlock;
use inkwell::values::{BasicValueEnum, IntValue};
use std::cell::RefCell;
use std::collections::{HashMap, LinkedList};
use std::process::exit;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct SwitchCaseItem<'a> {
    block_id: u32,
    value: InternalValue<'a>,
    body: BlockStatement,
    loc: Location,
    span_end: usize,
}

type SwitchCaseList<'a> = Vec<SwitchCaseItem<'a>>;

#[derive(Debug, Clone)]
pub struct LoopBlockRefs<'a> {
    pub cond_block: BasicBlock<'a>,
    pub end_block: BasicBlock<'a>,
}

#[derive(Debug, Clone)]
pub struct SwitchBlockRefs<'a> {
    pub exit_block: BasicBlock<'a>,
}

#[derive(Debug, Clone)]
pub struct TerminatedBlockMetadata<'a> {
    pub basic_block: BasicBlock<'a>,
    pub terminated_with_return: bool,
}

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
        $increment:tt,
        $loc:expr,
        $span_end:expr
    ) => {{
        let current_block = $self.get_current_block("for statement", $loc.clone(), $span_end);
        let current_func = $self.get_current_func("for statement", $loc.clone(), $span_end);
        let func_value = match $self.get_local_func_ir_value(current_func.local_ir_value_id) {
            Some(func_value) => func_value,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build loop statement outside of a function.".to_string()),
                    location: Some(DiagLoc {
                        file: $self.file_path.clone(),
                        line: $loc.line,
                        column: $loc.column,
                        length: $span_end,
                    }),
                });
                exit(1);
            }
        };

        let cond_block = $self.context.append_basic_block(func_value, "loop.cond");
        let body_block = $self.context.append_basic_block(func_value, "loop.body");
        let end_block = $self.context.append_basic_block(func_value, "loop.end");

        let previous_loop_ref = $self.block_registry.current_loop_ref.clone();
        $self.block_registry.current_loop_ref = Some(LoopBlockRefs { cond_block, end_block });

        $self.builder.position_at_end(current_block);
        $self.builder.build_unconditional_branch(cond_block).unwrap();
        $self.mark_block_terminated(current_block, false);

        $self.builder.position_at_end(cond_block);

        let condition_value = $condition;

        $self
            .builder
            .build_conditional_branch(condition_value, body_block, end_block)
            .unwrap();
        $self.mark_block_terminated(cond_block, false);

        $self.block_registry.current_block_ref = Some(body_block);
        $self.builder.position_at_end(body_block);

        $body

        let after_body_block = $self.get_current_block("for statement", $loc.clone(), $span_end);

        if !$self.is_block_terminated(after_body_block) {
            $self.builder.position_at_end(after_body_block);

            $increment

            $self.builder.build_unconditional_branch(cond_block).unwrap();
            $self.mark_block_terminated(after_body_block, false);
        }

        $self.block_registry.current_block_ref = Some(end_block);
        $self.builder.position_at_end(end_block);
        $self.block_registry.current_loop_ref = previous_loop_ref;
    }};
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
            Statement::Typedef(typedef) => self.build_typedef(typedef),
            Statement::GlobalVariable(global_variable) => self.build_global_variable(global_variable),
            Statement::Return(statement) => {
                self.build_return(Rc::clone(&scope), statement);
            }
            Statement::FuncDef(func_def) => {
                if func_def.name == "main" {
                    // entry_point gonna be compile in the final step of code generation
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
                    let params_metadata = self.build_func_params(
                        func_def.name.clone(),
                        func_def.loc.clone(),
                        func_def.span.end,
                        func_def.params.list.clone(),
                        func_def.params.variadic.clone(),
                        false,
                    );

                    let scope: ScopeRef<'ctx> = Rc::new(RefCell::new(Scope::new()));
                    self.build_func_def(scope, func_def.clone(), params_metadata, false);
                }
            }
            Statement::FuncDecl(func_decl) => {
                let param_types = self.build_func_params(
                    func_decl.name.clone(),
                    func_decl.loc.clone(),
                    func_decl.span.end,
                    func_decl.params.list.clone(),
                    func_decl.params.variadic.clone(),
                    false,
                );

                let return_type = self.build_type(
                    func_decl.return_type.clone().unwrap_or(TypeSpecifier::TypeToken(Token {
                        kind: TokenKind::Void,
                        span: Span::default(),
                        loc: Location::default(),
                    })),
                    func_decl.loc.clone(),
                    func_decl.span.end,
                );

                self.build_func_decl(func_decl, param_types, return_type, true);
            }
            Statement::If(if_statement) => self.build_if(Rc::clone(&scope), if_statement),
            Statement::For(for_statement) => self.build_for_statement(
                Rc::new(RefCell::new(scope.borrow().deep_clone_detached())),
                for_statement,
            ),
            Statement::Foreach(foreach) => self.build_foreach(Rc::clone(&scope), foreach),
            Statement::Switch(switch) => self.build_switch(Rc::clone(&scope), switch),
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
        self.block_registry
            .terminated_blocks
            .iter()
            .find(|metadata| metadata.basic_block == basic_block)
            .is_some()
    }

    pub(crate) fn get_block_terminated_metadata(
        &self,
        basic_block: BasicBlock<'ctx>,
    ) -> Option<TerminatedBlockMetadata<'ctx>> {
        self.block_registry
            .terminated_blocks
            .iter()
            .find(|metadata| metadata.basic_block == basic_block)
            .cloned()
    }

    pub(crate) fn mark_block_terminated(&mut self, basic_block: BasicBlock<'ctx>, terminated_with_return: bool) {
        self.block_registry.terminated_blocks.push(TerminatedBlockMetadata {
            basic_block,
            terminated_with_return,
        });
    }

    pub(crate) fn get_current_block(&self, stmt_name: &'ctx str, loc: Location, span_end: usize) -> BasicBlock<'ctx> {
        match self.block_registry.current_block_ref {
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

    pub(crate) fn get_current_func(&self, stmt_name: &'ctx str, loc: Location, span_end: usize) -> FuncMetadata<'ctx> {
        match &self.block_registry.current_func_ref {
            Some(func_metadata) => func_metadata.clone(),
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

    fn select_switch_grouped_cases(&self, block_id: u32, case_list: SwitchCaseList<'ctx>) -> SwitchCaseList<'ctx> {
        case_list
            .iter()
            .filter(|case| case.block_id == block_id)
            .cloned()
            .collect()
    }

    pub(crate) fn build_switch(&mut self, scope: ScopeRef<'ctx>, switch: Switch) {
        let lvalue = self.build_expr(Rc::clone(&scope), switch.value);
        let rvalue = self.internal_value_as_rvalue(lvalue, switch.loc.clone(), switch.span.end);

        // Analyze switch to determine it must be compiled as traditional-switch or smart-switch.
        let mut includes_non_integer_value = false;

        let mut case_list: SwitchCaseList<'ctx> = Vec::new();
        let mut block_id = 0;

        for case in switch.cases {
            let case_lvalue = self.build_expr(Rc::clone(&scope), case.raw.clone());
            let case_rvalue = self.internal_value_as_rvalue(case_lvalue, case.loc.clone(), case.span.end);
            match case_rvalue {
                InternalValue::IntValue(..) => {}
                _ => {
                    includes_non_integer_value = true;
                }
            }

            case_list.push(SwitchCaseItem {
                block_id,
                value: case_rvalue,
                body: case.body.clone(),
                loc: case.loc.clone(),
                span_end: case.span.end,
            });

            if !case.body.exprs.is_empty() {
                block_id += 1;
            }
        }

        // Temporarily remove reference to the loop-blocks. It's state must be returned after building the switch statement.
        let current_loop_ref_copy: Option<LoopBlockRefs<'ctx>> = self.block_registry.current_loop_ref.clone();
        self.block_registry.current_loop_ref = None;

        if includes_non_integer_value {
            self.build_smart_switch(
                Rc::clone(&scope),
                rvalue,
                case_list,
                switch.default,
                switch.loc.clone(),
                switch.span.end,
            );
        } else {
            self.build_traditional_switch(
                Rc::clone(&scope),
                rvalue,
                case_list,
                switch.default,
                switch.loc.clone(),
                switch.span.end,
            );
        }

        self.block_registry.current_loop_ref = current_loop_ref_copy;
    }

    fn build_traditional_switch(
        &mut self,
        scope: ScopeRef<'ctx>,
        value: InternalValue<'ctx>,
        case_list: SwitchCaseList<'ctx>,
        default_case: Option<BlockStatement>,
        switch_loc: Location,
        switch_end: usize,
    ) {
        let current_func_metadata = self.get_current_func("switch statement", switch_loc.clone(), switch_end);
        let current_func = match self.get_local_func_ir_value(current_func_metadata.local_ir_value_id) {
            Some(func_value) => func_value,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build switch statement outside of a function.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: switch_loc.line,
                        column: switch_loc.column,
                        length: switch_end,
                    }),
                });
                exit(1);
            }
        };

        let parent_switch_copy = self.block_registry.current_switch.clone();
        let parent_block = self.get_current_block("switch statement", switch_loc.clone(), switch_end);

        let exit_block = self.context.append_basic_block(current_func, "switch.exit");
        let target_block = if let Some(block_statement) = default_case.clone() {
            let default_block = self.context.append_basic_block(current_func, "switch.default");
            self.block_registry.current_block_ref = Some(default_block.clone());
            self.builder.position_at_end(default_block);
            self.block_registry.current_switch = Some(SwitchBlockRefs { exit_block });
            self.build_statements(Rc::clone(&scope), block_statement.exprs);
            default_block
        } else {
            self.block_registry.current_switch = Some(SwitchBlockRefs { exit_block });
            exit_block
        };

        let mut case_blocks: HashMap<u32, BasicBlock<'ctx>> = HashMap::new();
        let max_block_id = case_list.iter().last().unwrap().block_id;

        for block_id in 0..(max_block_id + 1) {
            let case_block = self.context.append_basic_block(current_func, "switch.case");
            case_blocks.insert(block_id, case_block);
        }

        for block_id in 0..(max_block_id + 1) {
            let group = self.select_switch_grouped_cases(block_id, case_list.clone());
            let group_body = &group.iter().last().unwrap().body;

            let case_block = case_blocks.get(&block_id).unwrap();
            self.builder.position_at_end(*case_block);
            self.block_registry.current_block_ref = Some(*case_block);
            self.build_statements(Rc::clone(&scope), group_body.exprs.clone());

            // NOTE Jump to the exit block if this is the last case, otherwise jump to the next case.
            let current_block = self.get_current_block("switch statement", switch_loc.clone(), switch_end);
            self.builder.position_at_end(current_block);

            if !self.is_block_terminated(current_block) {
                match case_list.iter().find(|case| case.block_id == block_id + 1) {
                    Some(next_case) => {
                        self.mark_block_terminated(current_block, false);
                        let next_case_block = case_blocks.get(&next_case.block_id).unwrap();
                        self.builder.build_unconditional_branch(*next_case_block).unwrap();
                        self.builder.position_at_end(*next_case_block);
                    }
                    None => {}
                }
            }
        }

        // Check if ending case doesn't have a terminator...
        let current_block = self.get_current_block("switch statement", switch_loc.clone(), switch_end);
        if !self.is_block_terminated(current_block) {
            self.mark_block_terminated(current_block, false);
            self.builder.build_unconditional_branch(target_block).unwrap();
            self.builder.position_at_end(target_block);
        }

        let ir_cases: Vec<(IntValue<'ctx>, BasicBlock<'ctx>)> = case_list
            .iter()
            .map(|case| {
                let int_value = match &case.value {
                    InternalValue::IntValue(int_value, ..) => int_value,
                    _ => unreachable!(),
                };
                let basic_block = case_blocks.get(&case.block_id).cloned().unwrap();
                (int_value.clone(), basic_block)
            })
            .collect();

        self.builder.position_at_end(parent_block);
        self.builder
            .build_switch(
                self.internal_value_to_basic_metadata(value).into_int_value(),
                target_block,
                &ir_cases,
            )
            .unwrap();

        if default_case.is_some() {
            self.builder.position_at_end(target_block);
            if !self.is_block_terminated(target_block) {
                self.builder.position_at_end(target_block);
                self.mark_block_terminated(target_block, false);
                self.builder.build_unconditional_branch(exit_block).unwrap();
            }
        }

        self.builder.position_at_end(exit_block);
        self.block_registry.current_block_ref = Some(exit_block);
        self.block_registry.current_switch = parent_switch_copy;
    }

    fn build_smart_switch(
        &mut self,
        scope: ScopeRef<'ctx>,
        value: InternalValue<'ctx>,
        case_list: SwitchCaseList<'ctx>,
        default_case: Option<BlockStatement>,
        switch_loc: Location,
        switch_end: usize,
    ) {
        let current_func_metadata = self.get_current_func("switch statement", switch_loc.clone(), switch_end);
        let current_func = match self.get_local_func_ir_value(current_func_metadata.local_ir_value_id) {
            Some(func_value) => func_value,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build switch statement outside of a function.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: switch_loc.line,
                        column: switch_loc.column,
                        length: switch_end,
                    }),
                });
                exit(1);
            }
        };

        let parent_switch_copy = self.block_registry.current_switch.clone();
        let parent_block = self.get_current_block("switch statement", switch_loc.clone(), switch_end);

        let exit_block = self.context.append_basic_block(current_func, "smart_switch.exit");
        let target_block = if let Some(block_statement) = default_case.clone() {
            let default_block = self.context.append_basic_block(current_func, "smart_switch.default");
            self.block_registry.current_block_ref = Some(default_block.clone());
            self.builder.position_at_end(default_block);
            self.block_registry.current_switch = Some(SwitchBlockRefs { exit_block });
            self.build_statements(Rc::clone(&scope), block_statement.exprs);
            default_block
        } else {
            self.block_registry.current_switch = Some(SwitchBlockRefs { exit_block });
            exit_block
        };

        let mut case_blocks: HashMap<u32, BasicBlock<'ctx>> = HashMap::new();
        let max_block_id = case_list.iter().last().unwrap().block_id;

        for block_id in 0..(max_block_id + 1) {
            let case_block = self.context.append_basic_block(current_func, "smart_switch.case");
            case_blocks.insert(block_id, case_block);
        }

        for block_id in 0..(max_block_id + 1) {
            let group = self.select_switch_grouped_cases(block_id, case_list.clone());
            let group_body = &group.iter().last().unwrap().body;

            let case_block = case_blocks.get(&block_id).unwrap();
            self.builder.position_at_end(*case_block);
            self.block_registry.current_block_ref = Some(*case_block);
            self.build_statements(Rc::clone(&scope), group_body.exprs.clone());

            let current_block = self.get_current_block("switch statement", switch_loc.clone(), switch_end);
            self.builder.position_at_end(current_block);

            if !self.is_block_terminated(current_block) {
                match case_list.iter().find(|case| case.block_id == block_id + 1) {
                    Some(next_case) => {
                        self.mark_block_terminated(current_block, false);
                        let next_case_block = case_blocks.get(&next_case.block_id).unwrap();
                        self.builder.build_unconditional_branch(*next_case_block).unwrap();
                        self.builder.position_at_end(*next_case_block);
                    }
                    None => {}
                }
            }
        }

        // Check if ending case doesn't have a terminator...
        let current_block = self.get_current_block("switch statement", switch_loc.clone(), switch_end);
        if !self.is_block_terminated(current_block) {
            self.mark_block_terminated(current_block, false);
            self.builder.build_unconditional_branch(target_block).unwrap();
            self.builder.position_at_end(target_block);
        }

        let mut ir_cases: Vec<(InternalValue<'ctx>, BasicBlock<'ctx>)> = case_list
            .iter()
            .map(|case| {
                let basic_block = case_blocks.get(&case.block_id).cloned().unwrap();
                (case.value.clone(), basic_block)
            })
            .collect();

        // Starting point of the switch statement.
        {
            let (internal_value, basic_block) = ir_cases.remove(0);
            self.builder.position_at_end(parent_block);

            let condition_internal_value = match self.bin_op_eq(internal_value, value.clone()) {
                Ok(internal_value) => internal_value,
                Err(..) => InternalValue::BoolValue(
                    self.context.bool_type().const_int(0, false),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ),
            };

            let basic_value: BasicValueEnum<'ctx> = self
                .internal_value_to_basic_metadata(condition_internal_value.clone())
                .try_into()
                .unwrap();

            self.builder
                .build_conditional_branch(basic_value.into_int_value(), basic_block, {
                    match ir_cases.first() {
                        Some((_, basic_block, ..)) => basic_block.clone(),
                        None => target_block,
                    }
                })
                .unwrap();
        }

        if default_case.is_some() {
            self.builder.position_at_end(target_block);
            if !self.is_block_terminated(target_block) {
                self.builder.position_at_end(target_block);
                self.mark_block_terminated(target_block, false);
                self.builder.build_unconditional_branch(exit_block).unwrap();
            }
        }

        self.builder.position_at_end(exit_block);
        self.block_registry.current_block_ref = Some(exit_block);
        self.block_registry.current_switch = parent_switch_copy;
    }

    pub(crate) fn build_continue_statement(&mut self, continue_statement: Continue) {
        let current_block = self.get_current_block(
            "continue statement",
            continue_statement.loc.clone(),
            continue_statement.span.end,
        );

        let loop_end_block = match &self.block_registry.current_loop_ref {
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
        self.mark_block_terminated(current_block, false);

        let target_block = match match &self.block_registry.current_loop_ref {
            Some(loop_block_refs) => Some(loop_block_refs.end_block),
            None => match &self.block_registry.current_switch {
                Some(switch_block_refs) => Some(switch_block_refs.exit_block),
                None => None,
            },
        } {
            Some(basic_block) => basic_block,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Break statement cannot be used here.".to_string()),
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

        self.builder.build_unconditional_branch(target_block).unwrap();
        self.builder.position_at_end(target_block);
    }

    pub(crate) fn build_infinite_for_statement(&mut self, scope: ScopeRef<'ctx>, for_statement: For) {
        let always_true_condition = self.build_index_value(1);

        build_loop_statement!(
            self,
            scope,
            always_true_condition,
            {
                for stmt in for_statement.body.exprs {
                    let current_block =
                        self.get_current_block("for statement", for_statement.loc.clone(), for_statement.span.end);
                    if self.is_block_terminated(current_block) {
                        break;
                    }

                    self.build_statement(Rc::clone(&scope), stmt.clone());
                }
            },
            {},
            for_statement.loc,
            for_statement.span.end
        );
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
        build_loop_statement!(
            self,
            scope,
            { self.build_cond(Rc::clone(&scope), condition, loc.clone(), span_end) },
            {
                for stmt in body.exprs {
                    let current_block = self.get_current_block("for statement", loc.clone(), span_end);
                    if self.is_block_terminated(current_block) {
                        break;
                    }

                    self.build_statement(Rc::clone(&scope), stmt.clone());
                }
            },
            {
                if let Some(increment_expr) = increment {
                    self.build_expr(Rc::clone(&scope), increment_expr);
                }
            },
            loc,
            span_end
        );
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

    pub(crate) fn build_foreach(&mut self, scope: ScopeRef<'ctx>, foreach: Foreach) {
        let lvalue = self.build_expr(Rc::clone(&scope), foreach.expr);
        let rvalue = self.internal_value_as_rvalue(lvalue.clone(), foreach.loc.clone(), foreach.span.end);

        let internal_array_type = match rvalue {
            InternalValue::ArrayValue(_, internal_type) => match internal_type {
                InternalType::ArrayType(internal_array_type) => internal_array_type,
                _ => unreachable!(),
            },
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build foreach statement with a non-array expression.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: foreach.loc.line,
                        column: foreach.loc.column,
                        length: foreach.span.end,
                    }),
                });
                exit(1);
            }
        };

        let array_length = self.build_index_value(internal_array_type.array_type.len().try_into().unwrap());

        let current_block = self.get_current_block("for statement", foreach.loc.clone(), foreach.span.end);
        let current_func = self.get_current_func("for statement", foreach.loc.clone(), foreach.span.end);
        let func_value = match self.get_local_func_ir_value(current_func.local_ir_value_id) {
            Some(func_value) => func_value,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build foreach statement outside of a function.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: foreach.loc.line,
                        column: foreach.loc.column,
                        length: foreach.span.end,
                    }),
                });
                exit(1);
            }
        };

        let index_alloca = self
            .builder
            .build_alloca(array_length.get_type(), "foreach.index")
            .unwrap();

        self.builder
            .build_store(index_alloca, array_length.get_type().const_int(0, false))
            .unwrap();

        let cond_block = self.context.append_basic_block(func_value, "loop.cond");
        let body_block = self.context.append_basic_block(func_value, "loop.body");
        let end_block = self.context.append_basic_block(func_value, "loop.end");

        // track current_loop
        let previous_loop_ref = self.block_registry.current_loop_ref.clone();
        self.block_registry.current_loop_ref = Some(LoopBlockRefs { cond_block, end_block });

        self.builder.position_at_end(current_block);
        self.builder.build_unconditional_branch(cond_block).unwrap();
        self.mark_block_terminated(current_block, false);

        self.builder.position_at_end(cond_block);
        let condition = {
            let index_value = self
                .builder
                .build_load(array_length.get_type(), index_alloca, "load")
                .unwrap();
            self.builder
                .build_int_compare(
                    inkwell::IntPredicate::ULT,
                    index_value.into_int_value(),
                    array_length,
                    "foreach.condition",
                )
                .unwrap()
        };
        self.builder
            .build_conditional_branch(condition, body_block, end_block)
            .unwrap();
        self.mark_block_terminated(cond_block, false);

        self.block_registry.current_block_ref = Some(body_block);
        self.builder.position_at_end(body_block);

        // fetch current item from array
        let element_basic_type = match internal_array_type
            .inner_type
            .to_basic_type(self.context.ptr_type(AddressSpace::default()))
        {
            Ok(basic_type) => basic_type,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(err.to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: foreach.loc.line,
                        column: foreach.loc.column,
                        length: foreach.span.end,
                    }),
                });
                exit(1);
            }
        };

        let index_value = self
            .builder
            .build_load(array_length.get_type(), index_alloca, "load")
            .unwrap()
            .into_int_value();

        let current_item_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(
                    element_basic_type,
                    self.internal_value_to_basic_metadata(lvalue).try_into().unwrap(),
                    &[index_value.clone()],
                    "gep",
                )
                .unwrap()
        };

        scope.borrow_mut().insert(
            foreach.item.name.to_string(),
            ScopeRecord {
                ptr: current_item_ptr,
                ty: *internal_array_type.inner_type,
            },
        );

        if let Some(index_identifier) = foreach.index {
            scope.borrow_mut().insert(
                index_identifier.name.to_string(),
                ScopeRecord {
                    ptr: index_alloca,
                    ty: InternalType::IntType(InternalIntType {
                        type_str: "size_t".to_string(),
                        int_kind: TokenKind::SizeT,
                        int_type: array_length.get_type(),
                    }),
                },
            );
        }

        for stmt in foreach.body.exprs {
            let current_block = self.get_current_block("foreach statement", foreach.loc.clone(), foreach.span.end);
            if self.is_block_terminated(current_block) {
                break;
            }

            self.build_statement(Rc::clone(&scope), stmt.clone());
        }

        let after_body_block = self.get_current_block("foreach statement", foreach.loc.clone(), foreach.span.end);

        if !self.is_block_terminated(after_body_block) {
            self.builder.position_at_end(after_body_block);

            // increment
            let one_value = array_length.get_type().const_int(1, false);

            self.builder
                .build_store(
                    index_alloca,
                    self.builder
                        .build_int_add(index_value, one_value, "foreach.increment")
                        .unwrap(),
                )
                .unwrap();

            self.builder.build_unconditional_branch(cond_block).unwrap();
            self.mark_block_terminated(after_body_block, false);
        }

        self.block_registry.current_block_ref = Some(end_block);
        self.builder.position_at_end(end_block);

        // clear current_loop
        self.block_registry.current_loop_ref = previous_loop_ref;
    }

    pub(crate) fn build_if(&mut self, scope: ScopeRef<'ctx>, if_statement: If) {
        let current_block = self.get_current_block("for statement", if_statement.loc.clone(), if_statement.span.end);
        let current_func = self.get_current_func("for statement", if_statement.loc.clone(), if_statement.span.end);
        let func_value = match self.get_local_func_ir_value(current_func.local_ir_value_id) {
            Some(func_value) => func_value,
            None => {
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
        };

        let then_block = self.context.append_basic_block(func_value, "if.then");
        let else_block = self.context.append_basic_block(func_value, "if.else");
        let end_block = self.context.append_basic_block(func_value, "if.end");

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
        self.block_registry.current_block_ref = Some(then_block);
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
            let new_else_block = self.context.append_basic_block(func_value, "else_if");
            let new_then_block = self.context.append_basic_block(func_value, "else_if.then");

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
            self.block_registry.current_block_ref = Some(new_then_block);
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
        self.block_registry.current_block_ref = Some(current_else_block);
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
        self.block_registry.current_block_ref = Some(end_block);
    }
}
