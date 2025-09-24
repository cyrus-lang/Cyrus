use crate::{diagnostics::AnalyzerDiagKind, type_cache::TypeResolverCaches};
use ast::{AssignmentKind, SelfModifierKind, source_loc::SourceLoc};
use diagcentral::{Diag, DiagLevel, DiagLoc, display_single_diag, reporter::DiagReporter};
use resolver::{
    Resolver,
    declsign::FuncSig,
    scope::{LocalOrGlobalSymbol, LocalSymbol, LocalSymbolKind, ResolvedVariable, SymbolEntryKind},
};
use std::{
    cell::RefCell,
    collections::HashMap,
    mem,
    rc::Rc,
    sync::{Arc, Mutex},
};
use typed_ast::{
    format::format_concrete_type,
    types::{BasicConcreteType, ConcreteType, ResolvedSymbol},
    *,
};

// FIXME Remove later
#[allow(unused)]
#[derive(Debug)]
enum ControlContext {
    For(TypedFor),
    Switch(TypedSwitch),
    While(TypedWhile),
}

#[macro_export]
macro_rules! update_global_symbol_type {
    ($self:expr, $module_id:expr, $symbol_id:expr, $pattern:pat => $var:ident, $body:block) => {{
        let mut global_symbols = $self.resolver.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get_mut(&$module_id).unwrap();
        match &mut symbol_table.entries.get_mut(&$symbol_id).unwrap().kind {
            $pattern => {
                let $var = $var;
                $body
            }
            _ => unreachable!(),
        }
    }};
}

pub struct AnalysisContext<'a> {
    pub ast: Rc<RefCell<TypedProgramTree>>,
    pub resolver: &'a Resolver,
    pub reporter: DiagReporter<AnalyzerDiagKind>,
    pub module_id: ModuleID,
    pub symbol_formatter: Box<dyn Fn(Option<ScopeID>) -> Box<dyn Fn(SymbolID) -> String + 'a> + 'a>,
    pub ty_caches: TypeResolverCaches,
    control_stack: Vec<ControlContext>,
    pub(crate) cur_func_symbol_id: Option<SymbolID>,
    pub disable_warnings: bool,
    pub entry_points: Arc<Mutex<Vec<SourceLoc>>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum FlowState {
    // Execution can continue normally
    Reachable,
    // Execution cannot reach further statements (after return/break/continue)
    Unreachable,
    // This path definitely returned from the function
    Returns,
}

impl<'a> AnalysisContext<'a> {
    pub fn new(
        resolver: &'a Resolver,
        module_id: ModuleID,
        ast: Rc<RefCell<TypedProgramTree>>,
        entry_points: Arc<Mutex<Vec<SourceLoc>>>,
        disable_warnings: bool,
    ) -> Self {
        let symbol_formatter = Box::new(
            move |scope_id_opt: Option<ScopeID>| -> Box<dyn Fn(SymbolID) -> String> {
                Box::new(move |symbol_id: SymbolID| -> String {
                    let local_scope_opt =
                        scope_id_opt.and_then(|scope_id| resolver.get_scope_ref(module_id, scope_id).clone());

                    let local_or_global_symbol = resolver
                        .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
                        .unwrap();

                    match local_or_global_symbol {
                        LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match &local_symbol.kind {
                            LocalSymbolKind::Variable(resolved_variable) => {
                                resolved_variable.typed_variable.name.clone()
                            }
                            LocalSymbolKind::Struct(resolved_struct) => resolved_struct.struct_sig.name.clone(),
                            LocalSymbolKind::Enum(resolved_enum) => resolved_enum.enum_sig.name.clone(),
                            LocalSymbolKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.name.clone(),
                            LocalSymbolKind::Interface(resolved_interface) => {
                                resolved_interface.interface_sig.name.clone()
                            }
                            LocalSymbolKind::Union(resolved_union) => resolved_union.union_sig.name.clone(),
                        },
                        LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig.name.clone(),
                            SymbolEntryKind::Func(resolved_function) => resolved_function.func_sig.name.clone(),
                            SymbolEntryKind::Typedef(resolved_typedef) => resolved_typedef.typedef_sig.name.clone(),
                            SymbolEntryKind::GlobalVar(resolved_global_var) => {
                                resolved_global_var.global_var_sig.name.clone()
                            }
                            SymbolEntryKind::Struct(resolved_struct) => resolved_struct.struct_sig.name.clone(),
                            SymbolEntryKind::Enum(resolved_enum) => resolved_enum.enum_sig.name.clone(),
                            SymbolEntryKind::Interface(resolved_interface) => {
                                resolved_interface.interface_sig.name.clone()
                            }
                            SymbolEntryKind::Union(resolved_union) => resolved_union.union_sig.name.clone(),
                        },
                    }
                })
            },
        );

        Self {
            ast,
            resolver,
            reporter: DiagReporter::new(),
            module_id,
            symbol_formatter,
            control_stack: Vec::new(),
            cur_func_symbol_id: None,
            entry_points,
            ty_caches: TypeResolverCaches::default(),
            disable_warnings,
        }
    }

    // Traverse TypedAST
    pub fn analyze(&mut self) {
        let mut body = {
            let mut ast_borrowed = self.ast.borrow_mut();
            mem::take(&mut ast_borrowed.body)
        };

        for mut typed_stmt in &mut body {
            match &mut typed_stmt {
                TypedStatement::Import(..) => continue,
                TypedStatement::GlobalVariable(typed_global_var) => self.analyze_global_var(typed_global_var),
                TypedStatement::FuncDef(typed_func_def) => self.analyze_func_def(typed_func_def),
                TypedStatement::FuncDecl(typed_func_decl) => self.analyze_func_decl(typed_func_decl),
                TypedStatement::Interface(typed_interface) => self.analyze_interface(typed_interface),
                TypedStatement::Struct(typed_struct) => self.analyze_struct(None, typed_struct, false),
                TypedStatement::Enum(typed_enum) => self.analyze_enum(None, typed_enum, false),
                TypedStatement::Typedef(typed_typedef) => self.analyze_typedef(None, typed_typedef),
                TypedStatement::Union(typed_union) => self.analyze_union(None, typed_union, false),
                // Invalid top-level statements
                TypedStatement::Variable(_)
                | TypedStatement::BlockStatement(_)
                | TypedStatement::If(_)
                | TypedStatement::Return(_)
                | TypedStatement::Break(_)
                | TypedStatement::Continue(_)
                | TypedStatement::For(_)
                | TypedStatement::While(_)
                | TypedStatement::Switch(_)
                | TypedStatement::Expression(_) => {
                    unreachable!()
                }
            }
        }

        self.analyze_unused_symbols();
        self.ast.borrow_mut().body = body;
    }

    // Traverse BlockStatement
    fn analyze_block_statement(&mut self, block_stmt: &mut TypedBlockStatement) -> FlowState {
        let mut state = FlowState::Reachable;

        for typed_stmt in &mut block_stmt.exprs {
            if state == FlowState::Unreachable {
                self.report_unreachable_block_diag(typed_stmt);
                break;
            }

            state = match typed_stmt {
                TypedStatement::Variable(typed_variable) => {
                    self.analyze_variable(Some(block_stmt.scope_id), typed_variable);
                    FlowState::Reachable
                }
                TypedStatement::BlockStatement(typed_block_statement) => {
                    self.analyze_block_statement(typed_block_statement)
                }
                TypedStatement::If(typed_if) => self.analyze_if_stmt(block_stmt.scope_id, typed_if, None),
                TypedStatement::Return(typed_return) => self.analyze_return(block_stmt.scope_id, typed_return),
                TypedStatement::Break(typed_break) => {
                    self.analyze_break(typed_break);
                    FlowState::Unreachable
                }
                TypedStatement::Continue(typed_continue) => {
                    self.analyze_continue(typed_continue);
                    FlowState::Unreachable
                }
                TypedStatement::For(typed_for) => self.analyze_for_loop(Some(block_stmt.scope_id), typed_for),
                TypedStatement::While(typed_while) => self.analyze_while_loop(Some(block_stmt.scope_id), typed_while),
                TypedStatement::Switch(typed_switch) => self.analyze_switch(Some(block_stmt.scope_id), typed_switch),
                TypedStatement::Struct(typed_struct) => {
                    self.analyze_struct(Some(block_stmt.scope_id), typed_struct, true);
                    FlowState::Reachable
                }
                TypedStatement::Enum(typed_enum) => {
                    self.analyze_enum(Some(block_stmt.scope_id), typed_enum, true);
                    FlowState::Reachable
                }
                TypedStatement::Expression(typed_expr) => {
                    self.analyze_typed_expr_type(
                        Some(block_stmt.scope_id),
                        typed_expr,
                        typed_expr.concrete_type.clone(),
                    );
                    FlowState::Reachable
                }
                TypedStatement::Interface(typed_interface) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::InternalInterfaceIsNotValid,
                        location: Some(DiagLoc::new(typed_interface.loc.clone())),
                        hint: None,
                    });
                    FlowState::Reachable
                }
                TypedStatement::Typedef(typed_typedef) => {
                    self.analyze_typedef(Some(block_stmt.scope_id), typed_typedef);
                    FlowState::Reachable
                }
                TypedStatement::Union(typed_union) => {
                    self.analyze_union(Some(block_stmt.scope_id), typed_union, true);
                    FlowState::Reachable
                }
                // Invalid statements
                TypedStatement::FuncDef(_)
                | TypedStatement::FuncDecl(_)
                | TypedStatement::Import(_)
                | TypedStatement::GlobalVariable(_) => unreachable!(),
            }
        }

        self.analyze_local_unused_symbols(block_stmt.scope_id);
        state
    }

    pub fn check_entry_points(entry_points_arc: Arc<Mutex<Vec<SourceLoc>>>) {
        let entry_points = entry_points_arc.lock().unwrap();
        let mut entry_points_clone = entry_points.clone();

        if entry_points.len() == 0 {
            display_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::MissingEntryPoint,
                location: None,
                hint: None,
            });
        } else if entry_points.len() > 1 {
            let loc = entry_points_clone.pop().unwrap();

            display_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::MultipleEntryPoints,
                location: Some(DiagLoc::new(loc)),
                hint: {
                    if let Some(another_decl_loc) = entry_points_clone.pop() {
                        Some(format!("Another declaration is at {}.", another_decl_loc))
                    } else {
                        None
                    }
                },
            });
        }

        drop(entry_points);
    }

    fn merge_flow_state(&self, a: FlowState, b: FlowState) -> FlowState {
        match (a, b) {
            (FlowState::Returns, FlowState::Returns) => FlowState::Returns,
            (FlowState::Unreachable, FlowState::Unreachable) => FlowState::Unreachable,
            _ => FlowState::Reachable,
        }
    }

    fn analyze_switch_on_enum(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_switch: &mut TypedSwitch,
        enum_symbol_id: SymbolID,
    ) -> FlowState {
        let mut branch_states = Vec::new();

        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, enum_symbol_id)
            .unwrap();

        let mut resolved_enum = local_or_global_symbol.as_enum().unwrap().clone();

        // Here instead of a simple for, we need peekable iterator to check next case

        let mut iter = typed_switch.cases.iter_mut().peekable();

        while let Some(case) = iter.next() {
            let identifier = match &case.pattern {
                TypedSwitchCasePattern::Identifier(identifier, _) => identifier,
                TypedSwitchCasePattern::EnumVariant(identifier, valued_fields, loc) => {
                    let mut field_names: Vec<String> = Vec::new();

                    for valued_field in valued_fields {
                        if field_names.contains(&valued_field.name) {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: AnalyzerDiagKind::DuplicateEnumVariantName {
                                    enum_name: resolved_enum.enum_sig.name.clone(),
                                    variant_name: valued_field.as_string(),
                                },
                                location: Some(DiagLoc::new(loc.clone())),
                                hint: None,
                            });
                            continue;
                        }

                        field_names.push(valued_field.as_string());
                    }

                    identifier
                }
                TypedSwitchCasePattern::Expression(..) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::ExpressionPatternInAEnumSwitch,
                        location: Some(DiagLoc::new(typed_switch.loc.clone())),
                        hint: None,
                    });
                    continue;
                }
            };

            let mut variant_opt = resolved_enum
                .enum_sig
                .variants
                .iter_mut()
                .find(|variant| variant.get_identifier().as_string() == *identifier);

            if variant_opt.is_none() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::NoSuchEnumVariant {
                        enum_name: resolved_enum.enum_sig.name.clone(),
                        variant_name: identifier.clone(),
                    },
                    location: Some(DiagLoc::new(typed_switch.loc.clone())),
                    hint: None,
                });
                continue;
            }

            if let TypedSwitchCasePattern::EnumVariant(_, valued_fields, loc) = &case.pattern {
                let actual_enum_fields_len = match &mut variant_opt {
                    Some(typed_enum_variant) => match typed_enum_variant {
                        TypedEnumVariant::Variant(_, enum_valued_fields) => enum_valued_fields.len(),
                        _ => unreachable!(),
                    },
                    None => unreachable!(),
                };

                if valued_fields.len() != actual_enum_fields_len {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::EnumVariantArgCountMismatch {
                            variant_name: variant_opt.unwrap().get_identifier().as_string(),
                            expected: actual_enum_fields_len as u32,
                            provided: valued_fields.len() as u32,
                        },
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: None,
                    });
                    continue;
                }

                match &mut variant_opt {
                    Some(typed_enum_variant) => match typed_enum_variant {
                        TypedEnumVariant::Variant(identifier, enum_valued_fields) => {
                            // normalize and then update valued_field type in local scope
                            let local_scope_rc =
                                self.resolver.get_scope_ref(self.module_id, case.body.scope_id).unwrap();

                            for (enum_valued_field_idx, enum_valued_field) in enum_valued_fields.iter_mut().enumerate()
                            {
                                enum_valued_field.field_type = match self.normalize_type(
                                    scope_id_opt,
                                    enum_valued_field.field_type.clone(),
                                    SourceLoc::from_loc(
                                        identifier.loc.clone(),
                                        resolved_enum.enum_sig.loc.file_path.clone(),
                                    ),
                                ) {
                                    Some(concrete_type) => concrete_type,
                                    None => continue,
                                };

                                let mut local_scope = local_scope_rc.borrow_mut();
                                let valued_field = &valued_fields[enum_valued_field_idx];
                                let local_symbol = local_scope.resolve_mut(&valued_field.name).unwrap();
                                match &mut local_symbol.kind {
                                    LocalSymbolKind::Variable(resolved_variable) => {
                                        resolved_variable.typed_variable.ty =
                                            Some(enum_valued_field.field_type.clone());
                                    }
                                    _ => unreachable!(),
                                }
                                drop(local_scope);
                            }
                        }
                        _ => unreachable!(),
                    },
                    None => unreachable!(),
                };
            }

            let body_flow_state = self.analyze_block_statement(&mut case.body);
            branch_states.push(body_flow_state);

            if body_flow_state == FlowState::Reachable {
                if let Some(next_case) = iter.next() {
                    if let TypedSwitchCasePattern::EnumVariant(..) = &next_case.pattern {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::SwitchFallthroughIntoValuedFieldCase,
                            location: Some(DiagLoc::new(next_case.loc.clone())),
                            hint: None,
                        });
                    }
                }
            }
        }

        if let Some(default_case) = &mut typed_switch.default_case {
            let body_flow_state = self.analyze_block_statement(default_case);
            branch_states.push(body_flow_state);
        } else {
            branch_states.push(FlowState::Reachable);
        }

        self.control_stack.pop();

        // final decision
        if branch_states.iter().all(|s| matches!(s, FlowState::Returns)) {
            FlowState::Returns
        } else {
            // merge states
            branch_states
                .into_iter()
                .reduce(|a, b| self.merge_flow_state(a, b))
                .unwrap_or(FlowState::Unreachable)
        }
    }

    fn analyze_switch(&mut self, scope_id_opt: Option<ScopeID>, typed_switch: &mut TypedSwitch) -> FlowState {
        self.control_stack.push(ControlContext::Switch(typed_switch.clone()));

        if typed_switch.cases.is_empty() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::EmptyCaseSwitchStatement,
                location: Some(DiagLoc::new(typed_switch.loc.clone())),
                hint: None,
            });
            return FlowState::Reachable;
        }

        let operand_concrete_type = match self.analyze_typed_expr_type(scope_id_opt, &mut typed_switch.operand, None) {
            Some(concrete_type) => concrete_type,
            None => return FlowState::Reachable,
        };

        // If operand is an enum → delegate to specialized analyzer
        if matches!(
            operand_concrete_type,
            ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(..))
        ) {
            return self.analyze_switch_on_enum(
                scope_id_opt,
                typed_switch,
                operand_concrete_type.as_enum_symbol_id().unwrap(),
            );
        }

        let mut branch_states = Vec::new();

        for case in &mut typed_switch.cases {
            match &mut case.pattern {
                TypedSwitchCasePattern::Expression(typed_expr, _) => {
                    let pattern_concrete_type = match self.analyze_typed_expr_type(scope_id_opt, typed_expr, None) {
                        Some(concrete_type) => concrete_type,
                        None => continue,
                    };

                    if !self.check_type_mismatch(
                        scope_id_opt,
                        pattern_concrete_type.clone(),
                        operand_concrete_type.clone(),
                        typed_switch.loc.clone(),
                    ) {
                        let operand_type =
                            format_concrete_type(operand_concrete_type.clone(), &(self.symbol_formatter)(scope_id_opt));
                        let pattern_type =
                            format_concrete_type(pattern_concrete_type, &(self.symbol_formatter)(scope_id_opt));

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::TypeMismatchInCasePattern {
                                operand_type,
                                pattern_type,
                            },
                            location: Some(DiagLoc::new(case.loc.clone())),
                            hint: None,
                        });
                        continue;
                    }
                }
                TypedSwitchCasePattern::Identifier(..) | TypedSwitchCasePattern::EnumVariant(..) => {
                    let expr_type =
                        format_concrete_type(operand_concrete_type.clone(), &(self.symbol_formatter)(scope_id_opt));

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::SwitchOperandIsNotEnum { expr_type },
                        location: Some(DiagLoc::new(typed_switch.loc.clone())),
                        hint: None,
                    });
                    continue;
                }
            }

            let body_flow_state = self.analyze_block_statement(&mut case.body);
            branch_states.push(body_flow_state);
        }

        if let Some(default_case) = &mut typed_switch.default_case {
            let body_flow_state = self.analyze_block_statement(default_case);
            branch_states.push(body_flow_state);
        } else {
            branch_states.push(FlowState::Reachable);
        }

        self.control_stack.pop();

        if branch_states.iter().all(|s| matches!(s, FlowState::Returns)) {
            FlowState::Returns
        } else {
            branch_states
                .into_iter()
                .reduce(|a, b| self.merge_flow_state(a, b))
                .unwrap_or(FlowState::Unreachable)
        }
    }

    fn analyze_if_stmt(
        &mut self,
        scope_id: ScopeID,
        typed_if: &mut TypedIf,
        expected_type: Option<ConcreteType>,
    ) -> FlowState {
        let consequent_state = self.analyze_block_statement(&mut typed_if.consequent);

        self.analyze_typed_expr_type(Some(scope_id), &mut typed_if.condition, expected_type.clone());

        let alternate_state = {
            if let Some(block_stmt) = &mut typed_if.alternate {
                self.analyze_block_statement(&mut *block_stmt)
            } else {
                FlowState::Reachable
            }
        };

        typed_if.branches.iter_mut().for_each(|branch| {
            self.analyze_if_stmt(scope_id, branch, expected_type.clone());
        });

        self.merge_flow_state(consequent_state, alternate_state)
    }

    fn analyze_while_loop(&mut self, scope_id_opt: Option<ScopeID>, typed_while: &mut TypedWhile) -> FlowState {
        if let Some(concrete_type) = self.analyze_typed_expr_type(
            scope_id_opt,
            &mut typed_while.condition,
            Some(ConcreteType::BasicType(BasicConcreteType::Bool)),
        ) {
            self.check_expr_type_must_be_condition(concrete_type, typed_while.loc.clone());
        }

        self.control_stack.push(ControlContext::While(typed_while.clone()));
        self.analyze_block_statement(&mut typed_while.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    fn analyze_for_loop(&mut self, scope_id_opt: Option<ScopeID>, typed_for: &mut TypedFor) -> FlowState {
        if let Some(initializer) = &mut typed_for.initializer {
            self.analyze_variable(scope_id_opt, initializer);
        }

        if let Some(typed_expr) = &mut typed_for.condition {
            if let Some(concrete_type) = self.analyze_typed_expr_type(
                scope_id_opt,
                typed_expr,
                Some(ConcreteType::BasicType(BasicConcreteType::Bool)),
            ) {
                self.check_expr_type_must_be_condition(concrete_type, typed_for.loc.clone());
            }
        }

        if let Some(typed_expr) = &mut typed_for.increment {
            self.analyze_typed_expr_type(scope_id_opt, typed_expr, typed_expr.concrete_type.clone());
        }

        self.control_stack.push(ControlContext::For(typed_for.clone()));
        self.analyze_block_statement(&mut typed_for.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    fn analyze_return(&mut self, scope_id: ScopeID, typed_return: &mut TypedReturn) -> FlowState {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(Some(scope_id));

        let func_sig = self.get_cur_func_sig();
        let return_type = self
            .normalize_type(Some(scope_id), func_sig.return_type, typed_return.loc.clone())
            .unwrap();

        if return_type.is_void() && typed_return.argument.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::VoidFunctionReturnsValue,
                location: Some(DiagLoc::new(typed_return.loc.clone())),
                hint: None,
            });
        } else if let Some(typed_expr) = &mut typed_return.argument {
            if let Some(concrete_type) =
                self.analyze_typed_expr_type(Some(scope_id), typed_expr, Some(return_type.clone()))
            {
                let expected = format_concrete_type(return_type.clone(), &formatter_closure);
                let got = format_concrete_type(concrete_type.clone(), &formatter_closure);

                if !self.check_type_mismatch(Some(scope_id), concrete_type, return_type, typed_return.loc.clone()) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::ReturnStatementTypeMismatch { expected, got },
                        location: Some(DiagLoc::new(typed_return.loc.clone())),
                        hint: None,
                    });
                }
            }
        } else if !return_type.is_void() && typed_return.argument.is_none() {
            let argument_type = format_concrete_type(return_type.clone(), &formatter_closure);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ReturnStatementNeedsAnArgument { argument_type },
                location: Some(DiagLoc::new(typed_return.loc.clone())),
                hint: None,
            });
        }

        FlowState::Returns
    }

    fn analyze_break(&mut self, typed_break: &TypedBreak) {
        if self.control_stack.len() == 0 {
            // break cannot be used outside of a for/switch statement
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidBreakStatement,
                location: Some(DiagLoc::new(typed_break.loc.clone())),
                hint: None,
            });
        }
    }

    fn analyze_continue(&mut self, typed_continue: &TypedContinue) {
        let inside_loop = self
            .control_stack
            .iter()
            .rev()
            .any(|ctx| matches!(ctx, ControlContext::For(_)));

        if !inside_loop {
            // continue cannot be used outside of a loop
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidContinueStatement,
                location: Some(DiagLoc::new(typed_continue.loc.clone())),
                hint: None,
            });
        }
    }

    fn report_unreachable_block_diag(&mut self, typed_stmt: &TypedStatement) {
        if !self.disable_warnings {
            self.reporter.report(Diag {
                level: DiagLevel::Warning,
                kind: AnalyzerDiagKind::UnreachableCode,
                location: Some(DiagLoc::new(typed_stmt.get_loc())),
                hint: None,
            });
        }
    }

    fn get_cur_func_sig(&self) -> FuncSig {
        let symbol_id = self.cur_func_symbol_id.unwrap();
        let symbol_entry = self
            .resolver
            .lookup_symbol_entry_with_id(self.module_id, symbol_id)
            .unwrap();
        match symbol_entry.as_func() {
            Some(resolved_func) => resolved_func.func_sig.clone(),
            None => symbol_entry.as_method().unwrap().func_sig.clone(),
        }
    }

    fn check_global_var_assignment_type(
        &mut self,
        global_var_type: ConcreteType,
        expr_type: ConcreteType,
        loc: SourceLoc,
    ) {
        let compatible_type = match (global_var_type.clone(), expr_type.clone()) {
            (ConcreteType::Const(concrete_type1), ConcreteType::Const(concrete_type2)) => {
                concrete_type1 == concrete_type2
            }
            (ConcreteType::Const(concrete_type1), concrete_type2) => *concrete_type1 == concrete_type2,
            (concrete_type1, ConcreteType::Const(concrete_type2)) => concrete_type1 == *concrete_type2,
            (concrete_type1, concrete_type2) => concrete_type1 == concrete_type2,
        };

        if !compatible_type {
            let lhs_type = format_concrete_type(global_var_type, &(self.symbol_formatter)(None));
            let rhs_type = format_concrete_type(expr_type, &(self.symbol_formatter)(None));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type },
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
        }
    }

    pub(crate) fn analyze_global_var(&mut self, typed_global_var: &mut TypedGlobalVariable) {
        if let Some(expr) = &mut typed_global_var.expr {
            if !expr.kind.is_comptime_valid() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::GlobalVariableExprNotComptimeValid,
                    location: Some(DiagLoc::new(typed_global_var.loc.clone())),
                    hint: None,
                });
                return;
            }

            expr.concrete_type = match self.analyze_typed_expr_type(None, expr, typed_global_var.ty.clone()) {
                Some(concrete_type) => Some(concrete_type),
                None => return,
            };
        }

        typed_global_var.ty = match &typed_global_var.ty {
            Some(concrete_type) => self.normalize_type(None, concrete_type.clone(), typed_global_var.loc.clone()),
            None => Some(typed_global_var.expr.clone().unwrap().concrete_type.unwrap()),
        };

        update_global_symbol_type!(self, typed_global_var.module_id, typed_global_var.symbol_id,
            SymbolEntryKind::GlobalVar(resolved_var) => resolved_var, {
                resolved_var.global_var_sig.ty = typed_global_var.ty.clone();
            }
        );

        if let Some(expr) = &mut typed_global_var.expr {
            if let Some(target_type) = &typed_global_var.ty {
                if !self.check_type_mismatch(
                    None,
                    expr.concrete_type.clone().unwrap(),
                    target_type.clone(),
                    typed_global_var.loc.clone(),
                ) {
                    let lhs_type = format_concrete_type(target_type.clone(), &(self.symbol_formatter)(None));
                    let rhs_type =
                        format_concrete_type(expr.concrete_type.clone().unwrap(), &(self.symbol_formatter)(None));

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type },
                        location: Some(DiagLoc::new(typed_global_var.loc.clone())),
                        hint: None,
                    });
                }
            }
        }

        if let Some(typed_expr) = &typed_global_var.expr {
            self.check_global_var_assignment_type(
                typed_global_var.ty.clone().unwrap(),
                typed_expr.concrete_type.clone().unwrap(),
                typed_global_var.loc.clone(),
            );
        }
    }

    fn analyze_local_unused_symbols(&mut self, scope_id: ScopeID) {
        let local_scope_rc = self.resolver.get_scope_ref(self.module_id, scope_id).unwrap();
        let local_scope = local_scope_rc.borrow();
        let symbols_clone = local_scope.symbols.clone();
        let symbols_iter = symbols_clone.values().into_iter();
        drop(local_scope);

        for local_symbol in symbols_iter {
            if !local_symbol.used {
                let (symbol_name, loc) = match &local_symbol.kind {
                    LocalSymbolKind::Variable(resolved_variable) => (
                        resolved_variable.typed_variable.name.clone(),
                        resolved_variable.typed_variable.loc.clone(),
                    ),
                    LocalSymbolKind::Struct(resolved_struct) => (
                        resolved_struct.struct_sig.name.clone(),
                        resolved_struct.struct_sig.loc.clone(),
                    ),
                    LocalSymbolKind::Enum(resolved_enum) => {
                        (resolved_enum.enum_sig.name.clone(), resolved_enum.enum_sig.loc.clone())
                    }
                    LocalSymbolKind::Typedef(resolved_typedef) => (
                        resolved_typedef.typedef_sig.name.clone(),
                        resolved_typedef.typedef_sig.loc.clone(),
                    ),
                    LocalSymbolKind::Interface(resolved_interface) => (
                        resolved_interface.interface_sig.name.clone(),
                        resolved_interface.interface_sig.loc.clone(),
                    ),
                    LocalSymbolKind::Union(resolved_union) => (
                        resolved_union.union_sig.name.clone(),
                        resolved_union.union_sig.loc.clone(),
                    ),
                };

                if !self.disable_warnings {
                    self.reporter.report(Diag {
                        level: DiagLevel::Warning,
                        kind: AnalyzerDiagKind::UnusedSymbol { symbol_name },
                        location: Some(DiagLoc::new(loc)),
                        hint: None,
                    });
                }
            }
        }
    }

    fn analyze_unused_symbols(&mut self) {
        let global_symbols = self.resolver.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get(&self.module_id).unwrap();

        for symbol_entry in symbol_table.entries.values() {
            if !symbol_entry.used {
                let (symbol_name, loc) = match &symbol_entry.kind {
                    SymbolEntryKind::Method(resolved_method) => (
                        resolved_method.func_sig.name.clone(),
                        resolved_method.func_sig.loc.clone(),
                    ),
                    SymbolEntryKind::Func(resolved_func) => {
                        // allow unused for main function
                        if resolved_func.func_sig.name == "main" {
                            continue;
                        }

                        (resolved_func.func_sig.name.clone(), resolved_func.func_sig.loc.clone())
                    }
                    SymbolEntryKind::Typedef(resolved_typedef) => (
                        resolved_typedef.typedef_sig.name.clone(),
                        resolved_typedef.typedef_sig.loc.clone(),
                    ),
                    SymbolEntryKind::GlobalVar(resolved_global_var) => (
                        resolved_global_var.global_var_sig.name.clone(),
                        resolved_global_var.global_var_sig.loc.clone(),
                    ),
                    SymbolEntryKind::Struct(resolved_struct) => (
                        resolved_struct.struct_sig.name.clone(),
                        resolved_struct.struct_sig.loc.clone(),
                    ),
                    SymbolEntryKind::Enum(resolved_enum) => {
                        (resolved_enum.enum_sig.name.clone(), resolved_enum.enum_sig.loc.clone())
                    }
                    SymbolEntryKind::Union(resolved_union) => (
                        resolved_union.union_sig.name.clone(),
                        resolved_union.union_sig.loc.clone(),
                    ),
                    SymbolEntryKind::Interface(resolved_interface) => (
                        resolved_interface.interface_sig.name.clone(),
                        resolved_interface.interface_sig.loc.clone(),
                    ),
                };

                if !self.disable_warnings {
                    self.reporter.report(Diag {
                        level: DiagLevel::Warning,
                        kind: AnalyzerDiagKind::UnusedSymbol { symbol_name },
                        location: Some(DiagLoc::new(loc)),
                        hint: None,
                    });
                }
            }
        }

        drop(global_symbols);
    }

    pub(crate) fn analyze_struct(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_struct: &mut TypedStruct,
        is_local: bool,
    ) {
        self.check_struct_name(typed_struct.name.clone(), typed_struct.loc.clone(), is_local);

        let mut field_names: Vec<String> = Vec::new();

        for field in &mut typed_struct.fields {
            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DuplicateFieldName {
                        object_name: typed_struct.name.clone(),
                        field_name: field.name.clone(),
                    },
                    location: Some(DiagLoc::new(field.loc.clone())),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
                continue;
            }

            field.ty = match self.normalize_type(scope_id_opt, field.ty.clone(), field.loc.clone()) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            field_names.push(field.name.clone());
        }

        self.analyze_methods(self.module_id, &typed_struct.methods);
    }

    pub(crate) fn analyze_union(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_union: &mut TypedUnion,
        is_local: bool,
    ) {
        self.check_union_name(typed_union.name.clone(), typed_union.loc.clone(), is_local);
        self.analyze_methods(self.module_id, &typed_union.methods);

        let mut field_names: Vec<String> = Vec::new();

        for field in &mut typed_union.fields {
            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DuplicateFieldName {
                        field_name: field.name.clone(),
                        object_name: typed_union.name.clone(),
                    },
                    location: Some(DiagLoc::new(field.loc.clone())),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
            }

            match self.normalize_type(scope_id_opt, field.ty.clone(), field.loc.clone()) {
                Some(concrete_type) => {
                    field.ty = concrete_type;
                }
                None => continue,
            }

            field_names.push(field.name.clone());
        }
    }

    pub(crate) fn analyze_enum(&mut self, scope_id_opt: Option<ScopeID>, typed_enum: &mut TypedEnum, is_local: bool) {
        self.check_enum_name(typed_enum.name.clone(), typed_enum.loc.clone(), is_local);
        self.analyze_methods(self.module_id, &typed_enum.methods);

        let mut variant_names: Vec<String> = Vec::new();

        for variant in &mut typed_enum.variants {
            let variant_identifier = match variant {
                TypedEnumVariant::Identifier(identifier) => identifier,
                TypedEnumVariant::Valued(identifier, typed_expr) => {
                    typed_expr.concrete_type = match self.analyze_typed_expr_type(scope_id_opt, typed_expr, None) {
                        Some(concrete_type) => Some(concrete_type),
                        None => continue,
                    };
                    identifier
                }
                TypedEnumVariant::Variant(identifier, typed_enum_valued_fields) => {
                    for field in typed_enum_valued_fields {
                        field.field_type =
                            match self.normalize_type(scope_id_opt, field.field_type.clone(), field.loc.clone()) {
                                Some(concrete_type) => concrete_type,
                                None => continue,
                            };
                    }
                    identifier
                }
            };

            if variant_names.contains(&variant_identifier.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DuplicateEnumVariantName {
                        enum_name: typed_enum.name.clone(),
                        variant_name: variant_identifier.name.clone(),
                    },
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        variant_identifier.loc.clone(),
                        typed_enum.loc.file_path.clone(),
                    ))),
                    hint: Some("Consider to rename the variant to a different name.".to_string()),
                });
                continue;
            }

            variant_names.push(variant_identifier.name.clone());
        }
    }

    pub(crate) fn check_duplicate_param_names(
        &mut self,
        params: &[TypedFuncParamKind],
        variadic: Option<&TypedFuncVariadicParams>,
        location: DiagLoc,
    ) {
        let mut param_names: Vec<String> = Vec::new();

        for (param_idx, param) in params.iter().enumerate() {
            match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => {
                    if param_names.contains(&typed_func_param.name) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::DuplicateFuncParameter {
                                param_name: typed_func_param.name.clone(),
                                param_idx: param_idx.try_into().unwrap(),
                            },
                            location: Some(location.clone()),
                            hint: Some("Consider to rename the parameter to a different name.".to_string()),
                        });
                        continue;
                    }

                    param_names.push(typed_func_param.name.clone());
                }
                TypedFuncParamKind::SelfModifier(_) => {
                    param_names.push("self".to_string());
                }
            }
        }

        if let Some(variadic_param) = variadic {
            match variadic_param {
                TypedFuncVariadicParams::Typed(identifier, _) => {
                    if param_names.contains(identifier) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::DuplicateFuncVariadicParameter {
                                param_name: identifier.clone(),
                            },
                            location: Some(location.clone()),
                            hint: Some("Consider to rename the parameter to a different name.".to_string()),
                        });
                    }
                }
                TypedFuncVariadicParams::UntypedCStyle => {}
            }
        }
    }

    fn analyze_any_func_def(
        &mut self,
        symbol_id: SymbolID,
        return_type: &mut ConcreteType,
        params: &mut TypedFuncParams,
        body: &mut TypedBlockStatement,
        loc: SourceLoc,
    ) {
        self.cur_func_symbol_id = Some(symbol_id);
        let state = self.analyze_block_statement(body);

        if !return_type.is_void() && state != FlowState::Returns {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::MissingReturn,
                location: Some(DiagLoc::new(loc.clone())),
                hint: Some("Not all control paths return a value.".to_string()),
            });
        }

        *return_type = match self.normalize_type(None, return_type.clone(), loc.clone()) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        self.normalize_func_params(params, loc);
    }

    fn analyze_methods(&mut self, module_id: ModuleID, methods: &HashMap<String, SymbolID>) {
        let mut local_methods_list: Vec<(SymbolID, FuncSig, Box<TypedBlockStatement>)> = Vec::new();

        // forward method declaration resolving
        for symbol_id in methods.values() {
            let (mut func_sig, func_body) = {
                let mut global_symbols = self.resolver.global_symbols.lock().unwrap();
                let symbol_table = global_symbols.get_mut(&module_id).unwrap();
                let symbol_entry = symbol_table.entries.get_mut(symbol_id).unwrap();

                match &mut symbol_entry.kind {
                    SymbolEntryKind::Method(m) => (m.func_sig.clone(), m.func_body.take().unwrap()),
                    _ => unreachable!(),
                }
            };

            self.cur_func_symbol_id = Some(*symbol_id);
            self.check_method_name(func_sig.name.clone(), func_sig.loc.clone());

            // public methods are allowed to not be used
            if func_sig.vis.is_public() {
                self.mark_symbol_used_once(module_id, *symbol_id);
            }

            self.cur_func_symbol_id = Some(*symbol_id);
            self.normalize_func_params(&mut func_sig.params, func_sig.loc.clone());

            func_sig.return_type = match self.normalize_type(None, func_sig.return_type.clone(), func_sig.loc.clone()) {
                Some(concrete_type) => concrete_type,
                None => return,
            };

            if let Some(typed_func_param_kind) = func_sig.params.list.first() {
                if let TypedFuncParamKind::SelfModifier(typed_self_modifier) = typed_func_param_kind.clone() {
                    func_sig.params.list[0] = TypedFuncParamKind::SelfModifier(typed_self_modifier.clone());
                }
            }

            local_methods_list.push((*symbol_id, func_sig.clone(), func_body));

            let mut global_symbols = self.resolver.global_symbols.lock().unwrap();
            let symbol_table = global_symbols.get_mut(&module_id).unwrap();
            let symbol_entry = symbol_table.entries.get_mut(&symbol_id).unwrap();
            if let SymbolEntryKind::Method(m) = &mut symbol_entry.kind {
                m.func_sig = func_sig;
            }
        }

        // analyze methods bodies
        for (symbol_id, func_sig, mut func_body) in local_methods_list {
            self.cur_func_symbol_id = Some(symbol_id);
            let state = self.analyze_block_statement(&mut func_body);

            if !func_sig.return_type.is_void() && state != FlowState::Returns {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::MissingReturn,
                    location: Some(DiagLoc::new(func_sig.loc.clone())),
                    hint: Some("Not all control paths return a value.".to_string()),
                });
            }

            let mut global_symbols = self.resolver.global_symbols.lock().unwrap();
            let symbol_table = global_symbols.get_mut(&module_id).unwrap();
            let symbol_entry = symbol_table.entries.get_mut(&symbol_id).unwrap();
            if let SymbolEntryKind::Method(m) = &mut symbol_entry.kind {
                m.func_sig = func_sig;
                m.func_body = Some(func_body);
            }
        }
    }

    fn analyze_func_def(&mut self, typed_func_def: &mut TypedFuncDef) {
        if typed_func_def.name == "main" {
            let mut entry_points = self.entry_points.lock().unwrap();
            entry_points.push(typed_func_def.loc.clone());
        }

        if typed_func_def.vis.is_public() {
            self.mark_symbol_used_once(self.module_id, typed_func_def.symbol_id);
        }

        self.analyze_any_func_def(
            typed_func_def.symbol_id,
            &mut typed_func_def.return_type,
            &mut typed_func_def.params,
            &mut typed_func_def.body,
            typed_func_def.loc.clone(),
        );
    }

    pub(crate) fn normalize_func_params(&mut self, params: &mut TypedFuncParams, loc: SourceLoc) {
        // analyze static arguments
        for param in params.list.iter_mut() {
            match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => {
                    let normalized_type = self
                        .normalize_type(None, typed_func_param.ty.clone(), typed_func_param.loc.clone())
                        .unwrap();

                    typed_func_param.ty = normalized_type.clone();
                }
                TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
                    let normalized_type = self
                        .normalize_type(
                            None,
                            ConcreteType::UnresolvedSymbol(typed_self_modifier.symbol_id.unwrap()),
                            typed_self_modifier.loc.clone(),
                        )
                        .unwrap();

                    match typed_self_modifier.kind {
                        SelfModifierKind::Copied => {
                            typed_self_modifier.ty = Some(normalized_type);
                        }
                        SelfModifierKind::Referenced => {
                            typed_self_modifier.ty = Some(ConcreteType::Pointer(Box::new(normalized_type)));
                        }
                    }
                }
            }
        }

        if let Some(variadic_params) = &mut params.variadic {
            if let TypedFuncVariadicParams::Typed(identifier, concrete_type) = variadic_params {
                let normalized_concrete_type = match self.normalize_type(None, concrete_type.clone(), loc.clone()) {
                    Some(concrete_type) => concrete_type,
                    None => return,
                };

                *variadic_params = TypedFuncVariadicParams::Typed(identifier.clone(), normalized_concrete_type);
            }
        }
    }

    fn analyze_func_decl(&mut self, typed_func_decl: &mut TypedFuncDecl) {
        self.check_duplicate_param_names(
            &typed_func_decl.params.list,
            typed_func_decl.params.variadic.as_ref(),
            DiagLoc::new(typed_func_decl.loc.clone()),
        );

        typed_func_decl.return_type =
            match self.normalize_type(None, typed_func_decl.return_type.clone(), typed_func_decl.loc.clone()) {
                Some(concrete_type) => concrete_type,
                None => return,
            };

        self.normalize_func_params(&mut typed_func_decl.params, typed_func_decl.loc.clone());
    }

    fn analyze_interface(&mut self, typed_interface: &TypedInterface) {
        self.check_interface_name(typed_interface.name.clone(), typed_interface.loc.clone(), false);

        let mut name_list: Vec<String> = Vec::new();

        let resolved_interface = self
            .resolver
            .lookup_symbol_entry_with_id(self.module_id, typed_interface.symbol_id)
            .unwrap();

        let interface_name = resolved_interface.as_interface().unwrap().interface_sig.name.clone();

        for method in &typed_interface.methods {
            if name_list.contains(&method.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InterfaceDuplicateMethod {
                        interface_name: interface_name.clone(),
                        method_name: method.name.clone(),
                    },
                    location: Some(DiagLoc::new(method.loc.clone())),
                    hint: None,
                });
                continue;
            }

            name_list.push(method.name.clone());
        }
    }

    fn analyze_typedef(&mut self, scope_id_opt: Option<ScopeID>, typed_typedef: &mut TypedTypedef) {
        typed_typedef.ty = match self.normalize_type(scope_id_opt, typed_typedef.ty.clone(), typed_typedef.loc.clone())
        {
            Some(concrete_type) => concrete_type,
            None => return,
        };
    }

    fn analyze_variable(&mut self, scope_id_opt: Option<ScopeID>, typed_variable: &mut TypedVariable) {
        if typed_variable.ty.is_none() && typed_variable.rhs.is_none() {
            return;
        }

        let value_type_opt = {
            if let Some(typed_expr) = &mut typed_variable.rhs {
                if let Some(concrete_type) = &typed_variable.ty {
                    typed_variable.ty =
                        self.normalize_type(scope_id_opt, concrete_type.clone(), typed_variable.loc.clone());
                }

                let concrete_type =
                    match self.analyze_typed_expr_type(scope_id_opt, typed_expr, typed_variable.ty.clone()) {
                        Some(concrete_type) => concrete_type,
                        None => return,
                    };

                typed_expr.concrete_type = Some(concrete_type.clone());
                Some(concrete_type)
            } else {
                None
            }
        };

        let local_scope_rc = self
            .resolver
            .get_scope_ref(self.module_id, scope_id_opt.unwrap())
            .unwrap();

        if let Some(concrete_type) = &typed_variable.ty {
            typed_variable.ty = self.normalize_type(scope_id_opt, concrete_type.clone(), typed_variable.loc.clone());

            if let Some(value_type) = value_type_opt {
                let lhs_type = format_concrete_type(
                    typed_variable.ty.clone().unwrap(),
                    &(self.symbol_formatter)(scope_id_opt),
                );
                let rhs_type = format_concrete_type(value_type.clone(), &(self.symbol_formatter)(scope_id_opt));

                if !self.check_type_mismatch(
                    scope_id_opt,
                    value_type.clone(),
                    typed_variable.ty.clone().unwrap(),
                    typed_variable.loc.clone(),
                ) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type },
                        location: Some(DiagLoc::new(typed_variable.loc.clone())),
                        hint: None,
                    });
                }
            }
        }

        if typed_variable.ty.is_some()
            && (typed_variable.ty.clone().unwrap().is_const() && typed_variable.rhs.is_none())
        {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ConstVariableMustBeInitialized,
                location: Some(DiagLoc::new(typed_variable.loc.clone())),
                hint: None,
            });
        }

        let mut local_scope = local_scope_rc.borrow_mut();
        local_scope.insert(
            typed_variable.name.clone(),
            LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                module_id: self.module_id,
                symbol_id: typed_variable.symbol_id,
                typed_variable: typed_variable.clone(),
            })),
        );
        drop(local_scope);
    }

    pub(crate) fn lower_assign_to_infix_expr(&self, assign: &mut TypedAssignment) -> TypedExpressionKind {
        let infix_expr = TypedExpressionKind::Infix(TypedInfixExpression {
            op: assign.kind.to_infix_operator(),
            lhs: assign.lhs.clone(),
            rhs: assign.rhs.clone(),
            loc: assign.loc.clone(),
        });

        TypedExpressionKind::Assignment(TypedAssignment {
            lhs: assign.lhs.clone(),
            rhs: Box::new(TypedExpression {
                kind: infix_expr,
                concrete_type: None,
                loc: assign.loc.clone(),
                value_category: ValueCategory::Rvalue,
            }),
            kind: AssignmentKind::Default,
            loc: assign.loc.clone(),
        })
    }

    pub(crate) fn analyze_assignment(&mut self, scope_id_opt: Option<ScopeID>, assign: &mut TypedAssignment) {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let lhs_type = match self.analyze_typed_expr_type(scope_id_opt, &mut assign.lhs, None) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        let rhs_type = match self.analyze_typed_expr_type(scope_id_opt, &mut assign.rhs, Some(lhs_type.clone())) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        if lhs_type.is_const() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::CannotAssignToConstLValue,
                location: Some(DiagLoc::new(assign.loc.clone())),
                hint: None,
            });
            return;
        }

        if assign.kind == AssignmentKind::Default {
            if !self.check_type_mismatch(scope_id_opt, rhs_type.clone(), lhs_type.clone(), assign.loc.clone()) {
                let lhs_type = format_concrete_type(lhs_type, &formatter_closure);
                let rhs_type = format_concrete_type(rhs_type, &formatter_closure);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type },
                    location: Some(DiagLoc::new(assign.loc.clone())),
                    hint: None,
                });
            }
        } else {
            unreachable!()
        }
    }
}
