use crate::{
    diagnostics::AnalyzerDiagKind,
    flowstate::{ControlContext, FlowState},
    generics::GenericMappingCtx,
    monomorph::MonomorphRegistry,
    type_cache::TypeResolverCaches,
};
use ast::{AccessSpecifier, AssignmentKind, LiteralKind, SelfModifierKind, source_loc::SourceLoc};
use diagcentral::{Diag, DiagLevel, DiagLoc, reporter::DiagReporter};
use resolver::{
    Resolver,
    symbols::{LocalSymbol, LocalSymbolKind, ResolvedVariable, SymbolEntryKind},
    sigs::{EnumSig, FuncSig},
    typed_func_decl_as_func_sig, typed_func_params_as_func_type_params,
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
    types::{BasicType, SemanticType, TypedFuncType},
    *,
};

#[macro_export]
macro_rules! update_global_symbol {
    ($self:expr, $module_id:expr, $symbol_id:expr, $pattern:pat => $var:ident, $body:block) => {{
        let mut global_symbols = $self.resolver.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get_mut(&$module_id).unwrap();
        match &mut symbol_table.entries.get_mut(&$symbol_id).unwrap().kind {
            $pattern => {
                let $var = $var;
                $body
            }
            _ => {
                unreachable!()
            }
        }
    }};
}

#[macro_export]
macro_rules! update_local_symbol {
    ($self:expr, $scope_id:expr, $symbol_id:expr, $pattern:pat => $var:ident, $body:block) => {{
        let local_scope_rc = $self.resolver.get_scope_ref($self.module_id, $scope_id).unwrap();
        let mut local_scope = local_scope_rc.borrow_mut();
        let local_symbol = local_scope.resolve_with_symbol_id_mut($symbol_id).unwrap();
        match &mut local_symbol.kind {
            $pattern => {
                let $var = $var;
                $body
            }
            _ => {
                unreachable!()
            }
        }
        drop(local_scope);
    }};
}

pub struct AnalysisContext<'a> {
    pub ast: Rc<RefCell<TypedProgramTree>>,
    pub resolver: &'a Resolver,
    pub reporter: DiagReporter<AnalyzerDiagKind>,
    pub module_id: ModuleID,
    pub symbol_formatter: Box<dyn Fn(Option<ScopeID>) -> Box<dyn Fn(SymbolID) -> String + 'a> + 'a>,
    pub ty_caches: TypeResolverCaches,
    pub(crate) current_func: Option<TypedFuncType>,
    pub(crate) current_method_symbol_id: Option<SymbolID>,
    pub disable_warnings: bool,
    pub entry_points: Arc<Mutex<Vec<SourceLoc>>>,
    pub(crate) generic_ctx_stack: Vec<GenericMappingCtx>,
    pub monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
    control_stack: Vec<ControlContext>,
}

impl<'a> AnalysisContext<'a> {
    pub fn new(
        resolver: &'a Resolver,
        module_id: ModuleID,
        ast: Rc<RefCell<TypedProgramTree>>,
        entry_points: Arc<Mutex<Vec<SourceLoc>>>,
        monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
        disable_warnings: bool,
    ) -> Self {
        let symbol_formatter = Self::build_symbol_formatter(resolver, module_id);

        Self {
            ast,
            resolver,
            reporter: DiagReporter::new(),
            module_id,
            symbol_formatter,
            control_stack: Vec::new(),
            current_func: None,
            current_method_symbol_id: None,
            entry_points,
            ty_caches: TypeResolverCaches::default(),
            generic_ctx_stack: Vec::new(),
            disable_warnings,
            monomorph_registry,
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
                TypedStmt::GlobalVariable(typed_global_var) => self.analyze_global_var(typed_global_var),
                TypedStmt::FuncDef(typed_func_def) => self.analyze_func_def(typed_func_def),
                TypedStmt::FuncDecl(typed_func_decl) => self.analyze_func_decl(typed_func_decl),
                TypedStmt::Interface(typed_interface) => self.analyze_interface(typed_interface),
                TypedStmt::Struct(typed_struct) => self.analyze_struct(None, typed_struct, false),
                TypedStmt::Enum(typed_enum) => self.analyze_enum(None, typed_enum, false),
                TypedStmt::Typedef(typed_typedef) => self.analyze_typedef(None, typed_typedef),
                TypedStmt::Union(typed_union) => self.analyze_union(None, typed_union, false),
                // Invalid top-level statements
                TypedStmt::Variable(_)
                | TypedStmt::ExportTuple(_)
                | TypedStmt::BlockStatement(_)
                | TypedStmt::Defer(_)
                | TypedStmt::If(_)
                | TypedStmt::Return(_)
                | TypedStmt::Break(_)
                | TypedStmt::Continue(_)
                | TypedStmt::For(_)
                | TypedStmt::While(_)
                | TypedStmt::Switch(_)
                | TypedStmt::Expression(_) => {
                    unreachable!()
                }
            }
        }

        self.analyze_unused_symbols();
        self.ast.borrow_mut().body = body;
    }

    pub(crate) fn analyze_block_statement(&mut self, block_stmt: &mut TypedBlockStmt) -> FlowState {
        let mut state = FlowState::Reachable;
        for typed_stmt in &mut block_stmt.exprs {
            let stmt_state = self.analyze_statement(block_stmt.scope_id, typed_stmt);

            match stmt_state {
                FlowState::Reachable => {
                    state = self.merge_flow_state(state, FlowState::Reachable);
                }
                FlowState::Unreachable => {
                    self.report_unreachable_block_diag(typed_stmt);
                    state = FlowState::Unreachable;
                    break;
                }
                FlowState::Returns => {
                    state = FlowState::Returns;
                    break;
                }
            }
        }

        for defer in &mut block_stmt.defers {
            self.analyze_statement(block_stmt.scope_id, &mut defer.operand);
        }

        self.analyze_local_unused_symbols(block_stmt.scope_id);
        state
    }

    pub(crate) fn analyze_statement(&mut self, scope_id: ScopeID, typed_stmt: &mut TypedStmt) -> FlowState {
        match typed_stmt {
            TypedStmt::ExportTuple(export_tuple_values) => {
                self.analyze_export_tuple_values(Some(scope_id), export_tuple_values);
                FlowState::Reachable
            }
            TypedStmt::Variable(typed_variable) => {
                self.analyze_variable(Some(scope_id), typed_variable);
                FlowState::Reachable
            }
            TypedStmt::BlockStatement(typed_block_statement) => {
                self.analyze_block_statement(typed_block_statement)
            }
            TypedStmt::If(typed_if) => self.analyze_if_stmt(scope_id, typed_if, None),
            TypedStmt::Return(typed_return) => self.analyze_return(scope_id, typed_return),
            TypedStmt::Break(typed_break) => {
                self.analyze_break(typed_break);
                FlowState::Unreachable
            }
            TypedStmt::Continue(typed_continue) => {
                self.analyze_continue(typed_continue);
                FlowState::Unreachable
            }
            TypedStmt::For(typed_for) => self.analyze_for_loop(Some(scope_id), typed_for),
            TypedStmt::While(typed_while) => self.analyze_while_loop(Some(scope_id), typed_while),
            TypedStmt::Switch(typed_switch) => self.analyze_switch(Some(scope_id), typed_switch),
            TypedStmt::Struct(typed_struct) => {
                self.analyze_struct(Some(scope_id), typed_struct, true);
                FlowState::Reachable
            }
            TypedStmt::Enum(typed_enum) => {
                self.analyze_enum(Some(scope_id), typed_enum, true);
                FlowState::Reachable
            }
            TypedStmt::Expression(typed_expr) => {
                self.analyze_typed_expr_type(Some(scope_id), typed_expr, typed_expr.sema_ty.clone());
                FlowState::Reachable
            }
            TypedStmt::Interface(typed_interface) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InternalInterfaceIsNotValid,
                    location: Some(DiagLoc::new(typed_interface.loc.clone())),
                    hint: None,
                });
                FlowState::Reachable
            }
            TypedStmt::Typedef(typed_typedef) => {
                self.analyze_typedef(Some(scope_id), typed_typedef);
                FlowState::Reachable
            }
            TypedStmt::Union(typed_union) => {
                self.analyze_union(Some(scope_id), typed_union, true);
                FlowState::Reachable
            }
            // Invalid statements
            TypedStmt::Defer(..)
            | TypedStmt::FuncDef(_)
            | TypedStmt::FuncDecl(_)
            | TypedStmt::GlobalVariable(_) => {
                unreachable!()
            }
        }
    }

    fn analyze_export_tuple_values(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        export_tuple_values: &mut TypedExportTupleStmt,
    ) {
        if let Some(typed_expr) = &mut export_tuple_values.rhs {
            let sema_ty =
                match self.analyze_typed_expr_type(scope_id_opt, typed_expr, export_tuple_values.ty.clone()) {
                    Some(sema_ty) => sema_ty,
                    None => return,
                };

            if export_tuple_values.ty.is_none() {
                export_tuple_values.ty = Some(sema_ty);
            }
        }

        if let Some(sema_ty) = &mut export_tuple_values.ty {
            *sema_ty =
                match self.normalize_type(scope_id_opt, sema_ty.clone(), export_tuple_values.loc.clone()) {
                    Some(normalized) => normalized,
                    None => return,
                };
        }

        let sema_ty = match self.normalize_type(
            scope_id_opt,
            export_tuple_values.ty.clone().unwrap(),
            export_tuple_values.loc.clone(),
        ) {
            Some(sema_ty) => sema_ty,
            None => return,
        };

        let tuple_type = match sema_ty.as_tuple_type() {
            Some(tuple_type) => tuple_type.clone(),
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand,
                    location: Some(DiagLoc::new(export_tuple_values.loc.clone())),
                    hint: None,
                });
                return;
            }
        };

        if export_tuple_values.exports.len() != tuple_type.type_list.len() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::TupleExportedValuesAndTupleElementsCountMismatch,
                location: Some(DiagLoc::new(export_tuple_values.loc.clone())),
                hint: None,
            });
            return;
        }

        // update type and rhs metadata in local scope

        for (symbol_id, mut sema_ty) in export_tuple_values.exports.iter().zip(tuple_type.type_list) {
            if export_tuple_values.is_const && !matches!(sema_ty, SemanticType::Const(..)) {
                sema_ty = SemanticType::Const(Box::new(sema_ty.clone()));
            }

            scope_id_opt.inspect(|scope_id| {
                update_local_symbol!(self, *scope_id, *symbol_id,
                    LocalSymbolKind::Variable(resolved_variable) => resolved_variable, {
                        resolved_variable.typed_variable.ty = Some(sema_ty);
                    }
                );
            });
        }
    }

    fn analyze_switch_on_enum(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_switch: &mut TypedSwitchStmt,
        enum_sig: &mut EnumSig,
    ) -> FlowState {
        let mut branch_states = Vec::new();

        for i in 0..typed_switch.cases.len() {
            let case = &mut typed_switch.cases[i];

            // check symbol name duplication
            let identifier = match &case.pattern {
                TypedSwitchCasePattern::Identifier(identifier, _) => identifier,
                TypedSwitchCasePattern::EnumVariant(identifier, valued_fields, loc) => {
                    let mut field_names: Vec<String> = Vec::new();

                    for valued_field in valued_fields {
                        if field_names.contains(&valued_field.name) {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: AnalyzerDiagKind::DuplicateEnumVariantName {
                                    enum_name: enum_sig.name.clone(),
                                    variant_name: valued_field.name.clone(),
                                },
                                location: Some(DiagLoc::new(loc.clone())),
                                hint: None,
                            });
                            continue;
                        }

                        field_names.push(valued_field.name.clone());
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

            let mut variant_opt = enum_sig
                .variants
                .iter_mut()
                .find(|variant| variant.get_identifier().as_string() == *identifier);

            if variant_opt.is_none() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::NoSuchEnumVariant {
                        enum_name: enum_sig.name.clone(),
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

                            for (enum_valued_field_idx, enum_valued_field) in enum_valued_fields.iter_mut().enumerate()
                            {
                                enum_valued_field.field_type = match self.normalize_type(
                                    scope_id_opt,
                                    enum_valued_field.field_type.clone(),
                                    SourceLoc::from_loc(identifier.loc.clone(), enum_sig.loc.file_path.clone()),
                                ) {
                                    Some(sema_ty) => sema_ty,
                                    None => continue,
                                };

                                // update local variable concrete type (exported symbol)
                                {
                                    let valued_field = &valued_fields[enum_valued_field_idx];

                                    update_local_symbol!(self, case.body.scope_id, valued_field.symbol_id,
                                        LocalSymbolKind::Variable(resolved_variable) => resolved_variable, {
                                            resolved_variable.typed_variable.ty = Some(enum_valued_field.field_type.clone());
                                        }
                                    );
                                }
                            }
                        }
                        _ => unreachable!(),
                    },
                    None => unreachable!(),
                };
            }

            let body_flow_state = self.analyze_block_statement(&mut case.body);
            branch_states.push(body_flow_state);

            // use the normalized state for fallthrough detection
            if body_flow_state == FlowState::Reachable && i + 1 < typed_switch.cases.len() {
                let next_case = &typed_switch.cases[i + 1];
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

        if let Some(default_case) = &mut typed_switch.default_case {
            let body_flow_state = self.analyze_block_statement(default_case);
            branch_states.push(body_flow_state);
        } else {
            branch_states.push(FlowState::Reachable);
        }

        // default case
        if let Some(default_case) = &mut typed_switch.default_case {
            let body_flow_state = self.analyze_block_statement(default_case);
            branch_states.push(body_flow_state);
        } else {
            branch_states.push(FlowState::Reachable);
        }

        self.control_stack.pop();

        // final merge
        let flow_state = if branch_states.iter().all(|s| matches!(s, FlowState::Returns)) {
            FlowState::Returns
        } else {
            FlowState::Reachable
        };

        flow_state
    }

    fn analyze_switch(&mut self, scope_id_opt: Option<ScopeID>, typed_switch: &mut TypedSwitchStmt) -> FlowState {
        let mut mapping_ctx = GenericMappingCtx::new_root();

        self.control_stack.push(ControlContext::Switch);

        if typed_switch.cases.is_empty() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::EmptyCaseSwitchStatement,
                location: Some(DiagLoc::new(typed_switch.loc.clone())),
                hint: None,
            });
            return FlowState::Reachable;
        }

        let operand_ty = match self.analyze_typed_expr_type(scope_id_opt, &mut typed_switch.operand, None) {
            Some(sema_ty) => sema_ty,
            None => return FlowState::Reachable,
        };

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        match if let Some(enum_symbol_id) = operand_ty.as_enum_symbol_id() {
            Some((enum_symbol_id, None))
        } else if let Some(generic_type) = operand_ty.as_generic_type() {
            match self
                .resolver
                .resolve_local_or_global_symbol(local_scope_opt.clone(), generic_type.base)
                .and_then(|sym| {
                    let resolved_enum = sym.as_enum();
                    resolved_enum.cloned()
                })
                .map(|resolved_enum| resolved_enum.symbol_id)
                .map(|enum_symbol_id| enum_symbol_id)
            {
                Some(enum_symbol_id) => Some((enum_symbol_id, Some(generic_type))),
                None => {
                    let expr_type = (self.symbol_formatter)(scope_id_opt)(generic_type.base);
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::SwitchOperandIsNotEnum { expr_type },
                        location: Some(DiagLoc::new(typed_switch.loc.clone())),
                        hint: None,
                    });
                    return FlowState::Reachable;
                }
            }
        } else {
            None
        } {
            Some((enum_symbol_id, generic_type_opt)) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, enum_symbol_id)
                    .unwrap();

                let resolved_enum = sym.as_enum().unwrap();
                let mut enum_sig = resolved_enum.enum_sig.clone();
                self.substitute_enum_type_args(
                    &mut mapping_ctx,
                    scope_id_opt,
                    &mut enum_sig,
                    generic_type_opt,
                    typed_switch.loc.clone(),
                );

                return self.analyze_switch_on_enum(scope_id_opt, typed_switch, &mut enum_sig);
            }
            None => {}
        }

        let mut branch_states = Vec::new();

        for case in &mut typed_switch.cases {
            match &mut case.pattern {
                TypedSwitchCasePattern::Expression(typed_expr, _) => {
                    let pattern_concrete_type = match self.analyze_typed_expr_type(scope_id_opt, typed_expr, None) {
                        Some(sema_ty) => sema_ty,
                        None => continue,
                    };

                    if !self.check_type_mismatch(
                        scope_id_opt,
                        pattern_concrete_type.clone(),
                        operand_ty.clone(),
                        typed_switch.loc.clone(),
                    ) {
                        let operand_type =
                            format_concrete_type(operand_ty.clone(), &(self.symbol_formatter)(scope_id_opt));
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
                    let expr_type = format_concrete_type(operand_ty.clone(), &(self.symbol_formatter)(scope_id_opt));

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

        let mut branch_states = Vec::new();

        // normalize each case body
        for case in &mut typed_switch.cases {
            let body_flow_state = self.analyze_block_statement(&mut case.body);
            branch_states.push(body_flow_state);
        }

        // default case
        if let Some(default_case) = &mut typed_switch.default_case {
            let body_flow_state = self.analyze_block_statement(default_case);
            branch_states.push(body_flow_state);
        } else {
            branch_states.push(FlowState::Reachable);
        }

        self.control_stack.pop();

        // final merge
        let flow_state = if branch_states.iter().all(|s| matches!(s, FlowState::Returns)) {
            FlowState::Returns
        } else {
            FlowState::Reachable
        };

        flow_state
    }

    fn analyze_if_stmt(
        &mut self,
        scope_id: ScopeID,
        typed_if: &mut TypedIfStmt,
        expected_type: Option<SemanticType>,
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

    fn analyze_while_loop(&mut self, scope_id_opt: Option<ScopeID>, typed_while: &mut TypedWhileStmt) -> FlowState {
        if let Some(sema_ty) = self.analyze_typed_expr_type(
            scope_id_opt,
            &mut typed_while.condition,
            Some(SemanticType::BasicType(BasicType::Bool)),
        ) {
            self.check_expr_type_must_be_condition(sema_ty, typed_while.loc.clone());
        }

        self.control_stack.push(ControlContext::While);
        self.analyze_block_statement(&mut typed_while.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    fn analyze_for_loop(&mut self, scope_id_opt: Option<ScopeID>, typed_for: &mut TypedForStmt) -> FlowState {
        if let Some(initializer) = &mut typed_for.initializer {
            self.analyze_variable(scope_id_opt, initializer);
        }

        if let Some(typed_expr) = &mut typed_for.condition {
            if let Some(sema_ty) = self.analyze_typed_expr_type(
                scope_id_opt,
                typed_expr,
                Some(SemanticType::BasicType(BasicType::Bool)),
            ) {
                self.check_expr_type_must_be_condition(sema_ty, typed_for.loc.clone());
            }
        }

        if let Some(typed_expr) = &mut typed_for.increment {
            self.analyze_typed_expr_type(scope_id_opt, typed_expr, typed_expr.sema_ty.clone());
        }

        self.control_stack.push(ControlContext::Loop);
        self.analyze_block_statement(&mut typed_for.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    fn analyze_return(&mut self, scope_id: ScopeID, typed_return: &mut TypedReturnStmt) -> FlowState {
        let func_type = self.current_func.clone().unwrap();
        let return_type = self
            .normalize_type(Some(scope_id), *func_type.return_type, typed_return.loc.clone())
            .unwrap();

        if return_type.is_void() && typed_return.argument.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::VoidFunctionReturnsValue,
                location: Some(DiagLoc::new(typed_return.loc.clone())),
                hint: None,
            });
        } else if let Some(typed_expr) = &mut typed_return.argument {
            if let Some(sema_ty) =
                self.analyze_typed_expr_type(Some(scope_id), typed_expr, Some(return_type.clone()))
            {
                let expected = format_concrete_type(return_type.clone(), &(self.symbol_formatter)(Some(scope_id)));
                let got = format_concrete_type(sema_ty.clone(), &(self.symbol_formatter)(Some(scope_id)));

                if !self.check_type_mismatch(Some(scope_id), sema_ty, return_type, typed_return.loc.clone()) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::ReturnStatementTypeMismatch { expected, got },
                        location: Some(DiagLoc::new(typed_return.loc.clone())),
                        hint: None,
                    });
                }
            }
        } else if !return_type.is_void() && typed_return.argument.is_none() {
            let argument_type = format_concrete_type(return_type.clone(), &(self.symbol_formatter)(Some(scope_id)));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ReturnStatementNeedsAnArgument { argument_type },
                location: Some(DiagLoc::new(typed_return.loc.clone())),
                hint: None,
            });
        }

        FlowState::Returns
    }

    fn analyze_break(&mut self, typed_break: &TypedBreakStmt) -> FlowState {
        if self.control_stack.is_empty() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidBreakStatement,
                location: Some(DiagLoc::new(typed_break.loc.clone())),
                hint: None,
            });
            FlowState::Reachable
        } else {
            FlowState::Unreachable
        }
    }

    fn analyze_continue(&mut self, typed_continue: &TypedContinueStmt) -> FlowState {
        let inside_loop = self
            .control_stack
            .iter()
            .rev()
            .any(|ctx| matches!(ctx, ControlContext::Loop));

        if !inside_loop {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidContinueStatement,
                location: Some(DiagLoc::new(typed_continue.loc.clone())),
                hint: None,
            });
            FlowState::Reachable
        } else {
            FlowState::Unreachable
        }
    }

    fn report_unreachable_block_diag(&mut self, typed_stmt: &TypedStmt) {
        if !self.disable_warnings {
            self.reporter.report(Diag {
                level: DiagLevel::Warning,
                kind: AnalyzerDiagKind::UnreachableCode,
                location: Some(DiagLoc::new(typed_stmt.get_loc())),
                hint: None,
            });
        }
    }

    fn check_global_var_assignment_type(
        &mut self,
        global_var_type: SemanticType,
        expr_type: SemanticType,
        loc: SourceLoc,
    ) {
        let compatible_type = match (global_var_type.clone(), expr_type.clone()) {
            (SemanticType::Const(concrete_type1), SemanticType::Const(concrete_type2)) => {
                concrete_type1 == concrete_type2
            }
            (SemanticType::Const(concrete_type1), concrete_type2) => *concrete_type1 == concrete_type2,
            (concrete_type1, SemanticType::Const(concrete_type2)) => concrete_type1 == *concrete_type2,
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

    fn check_global_var_for_const_folding(&self, sema_ty: SemanticType) -> bool {
        match sema_ty.get_const_inner().as_basic_type() {
            Some(basic_concrete_type) => basic_concrete_type.is_integer(),
            None => false,
        }
    }

    pub(crate) fn analyze_global_var(&mut self, typed_global_var: &mut TypedGlobalVarStmt) {
        if let Some(mut expr) = typed_global_var.expr.clone() {
            let sema_ty = match self.analyze_typed_expr_type(None, &mut expr, typed_global_var.ty.clone()) {
                Some(sema_ty) => sema_ty,
                None => return,
            };

            if self.check_global_var_for_const_folding(sema_ty) {
                if let Some(integer) = self.const_expr_as_raw_integer(None, &expr) {
                    let integer_concrete_type = Some(SemanticType::BasicType(BasicType::Int));

                    expr = TypedExprStmt {
                        kind: TypedExprKind::Literal(TypedLiteralExpr {
                            ty: integer_concrete_type.clone(),
                            kind: LiteralKind::Integer(integer, None),
                            loc: expr.loc.clone(),
                        }),
                        sema_ty: integer_concrete_type,
                        vcat: ValueCategory::RValue,
                        loc: expr.loc.clone(),
                    };
                } else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::GlobalVariableExprNotComptimeValid,
                        location: Some(DiagLoc::new(typed_global_var.loc.clone())),
                        hint: None,
                    });
                    return;
                }
            }

            expr.sema_ty = match self.analyze_typed_expr_type(None, &mut expr, typed_global_var.ty.clone()) {
                Some(sema_ty) => Some(sema_ty),
                None => return,
            };

            typed_global_var.expr = Some(expr);
        }

        typed_global_var.ty = match &typed_global_var.ty {
            Some(sema_ty) => self.normalize_type(None, sema_ty.clone(), typed_global_var.loc.clone()),
            None => Some(typed_global_var.expr.clone().unwrap().sema_ty.unwrap()),
        };

        if matches!(
            typed_global_var.ty,
            Some(SemanticType::BasicType(BasicType::Void))
        ) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::VoidVariableType,
                location: Some(DiagLoc::new(typed_global_var.loc.clone())),
                hint: None,
            });
        }

        if matches!(typed_global_var.ty, Some(SemanticType::FuncType(..))) && typed_global_var.expr.is_none() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::UninitializedLambda,
                location: Some(DiagLoc::new(typed_global_var.loc.clone())),
                hint: None,
            });
            return;
        }

        if typed_global_var.is_const && !matches!(typed_global_var.ty, Some(SemanticType::Const(..))) {
            typed_global_var.ty = Some(SemanticType::Const(Box::new(typed_global_var.ty.clone().unwrap())));
        }

        if let Some(expr) = &typed_global_var.expr {
            if !expr.kind.is_comptime_valid() && !matches!(typed_global_var.ty, Some(SemanticType::Const(..))) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::GlobalVariableExprNotComptimeValid,
                    location: Some(DiagLoc::new(typed_global_var.loc.clone())),
                    hint: None,
                });
                return;
            }
        }

        update_global_symbol!(self, typed_global_var.module_id, typed_global_var.symbol_id,
            SymbolEntryKind::GlobalVar(resolved_var) => resolved_var, {
                resolved_var.global_var_sig.rhs = typed_global_var.expr.clone();
                resolved_var.global_var_sig.ty = typed_global_var.ty.clone();
            }
        );

        if let Some(expr) = &mut typed_global_var.expr {
            if let Some(target_type) = &typed_global_var.ty {
                if !self.check_type_mismatch(
                    None,
                    expr.sema_ty.clone().unwrap(),
                    target_type.clone(),
                    typed_global_var.loc.clone(),
                ) {
                    let lhs_type = format_concrete_type(target_type.clone(), &(self.symbol_formatter)(None));
                    let rhs_type =
                        format_concrete_type(expr.sema_ty.clone().unwrap(), &(self.symbol_formatter)(None));

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
                typed_expr.sema_ty.clone().unwrap(),
                typed_global_var.loc.clone(),
            );
        }
    }

    fn analyze_local_unused_symbols(&mut self, scope_id: ScopeID) {
        if let Some(local_scope_rc) = self.resolver.get_scope_ref(self.module_id, scope_id) {
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

    fn analyze_object_impls_interface(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        object_name: String,
        impls: &Vec<TypedIdentifier>,
        method_ids: &HashMap<String, SymbolID>,
    ) {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        for identifier in impls {
            let sym = self
                .resolver
                .resolve_local_or_global_symbol(local_scope_opt.clone(), identifier.symbol_id)
                .unwrap();

            let resolved_interface = match sym.as_interface() {
                Some(resolved_interface) => resolved_interface,
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::SymbolMustBeAnInterface {
                            symbol_name: identifier.name.clone(),
                        },
                        location: Some(DiagLoc::new(identifier.loc.clone())),
                        hint: None,
                    });
                    continue;
                }
            };

            if resolved_interface.interface_sig.vis.is_private()
                && resolved_interface.interface_sig.module_id != self.module_id
            {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InternalSymbolAccess {
                        symbol_name: identifier.name.clone(),
                    },
                    location: Some(DiagLoc::new(identifier.loc.clone())),
                    hint: None,
                });
                continue;
            }

            let method_decls = &resolved_interface.interface_sig.methods;

            for func_decl in method_decls.clone() {
                if !method_ids.contains_key(&func_decl.name) {
                    // method missing
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::MissingInterfaceMethodImpl {
                            object_name: object_name.clone(),
                            method_name: func_decl.name.clone(),
                            interface_name: identifier.name.clone(),
                        },
                        location: Some(DiagLoc::new(identifier.loc.clone())),
                        hint: None,
                    });
                    continue;
                }

                let object_method_symbol_id = method_ids.get(&func_decl.name).unwrap();
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt.clone(), *object_method_symbol_id)
                    .unwrap();
                let object_method = sym.as_method().unwrap();

                { // NOTE
                    // we may need to change SelfModifier to something like a standalone SemanticType
                    // because currently we can't have `Self` type in interface which already became a paint in the ass.
                    //
                    // interface Person {
                    //     fn display(&self) Self;
                    // }
                    //
                    // struct Student : Person {
                    //     name: char*;
                    //     age: uint;
                    //
                    //     fn display(&self) Student { ... }
                    // }
                    //
                    // Expected: Method signature must be considered valid here.
                }

                // method signature mismatch
                if object_method.func_sig != typed_func_decl_as_func_sig(&func_decl) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::InterfaceMethodTypeMismatch {
                            object_name: object_name.clone(),
                            interface_name: identifier.name.clone(),
                            method_name: func_decl.name.clone(),
                        },
                        location: Some(DiagLoc::new(object_method.func_sig.loc.clone())),
                        hint: None,
                    });
                }
            }
        }
    }

    pub(crate) fn analyze_struct(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_struct: &mut TypedStructStmt,
        is_local: bool,
    ) {
        fn update_struct_symbol_entry<'b>(this: &mut AnalysisContext<'b>, typed_struct: &TypedStructStmt) {
            if let Some(scope_id) = typed_struct.is_local {
                update_local_symbol!(this, scope_id, typed_struct.symbol_id,
                    LocalSymbolKind::Struct(resolved_struct) => resolved_struct, {
                        resolved_struct.struct_sig.fields = typed_struct.fields.clone();
                    }
                )
            } else {
                update_global_symbol!(this, typed_struct.module_id, typed_struct.symbol_id,
                    SymbolEntryKind::Struct(resolved_struct) => resolved_struct, {
                        resolved_struct.struct_sig.fields = typed_struct.fields.clone();
                    }
                );
            }
        }

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
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            if field
                .ty
                .as_struct_symbol_id()
                .map(|symbol_id| symbol_id == typed_struct.symbol_id)
                == Some(true)
            {
                let type_name = (self.symbol_formatter)(scope_id_opt)(typed_struct.symbol_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InfiniteRecursiveType { type_name },
                    location: Some(DiagLoc::new(field.loc.clone())),
                    hint: None,
                });
                continue;
            }

            field_names.push(field.name.clone());
        }

        self.analyze_methods(self.module_id, &typed_struct.methods);

        self.analyze_object_impls_interface(
            scope_id_opt,
            typed_struct.name.clone(),
            &typed_struct.impls,
            &typed_struct.methods,
        );

        // update symbol entry
        update_struct_symbol_entry(self, &typed_struct);
    }

    pub(crate) fn analyze_union(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_union: &mut TypedUnionStmt,
        is_local: bool,
    ) {
        fn update_union_symbol_entry<'b>(this: &mut AnalysisContext<'b>, typed_union: &TypedUnionStmt) {
            if let Some(scope_id) = typed_union.is_local {
                update_local_symbol!(this, scope_id, typed_union.symbol_id,
                    LocalSymbolKind::Union(resolved_union) => resolved_union, {
                        resolved_union.union_sig.fields = typed_union.fields.clone();
                    }
                )
            } else {
                update_global_symbol!(this, typed_union.module_id, typed_union.symbol_id,
                    SymbolEntryKind::Union(resolved_union) => resolved_union, {
                        resolved_union.union_sig.fields = typed_union.fields.clone();
                    }
                );
            }
        }

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
                Some(sema_ty) => {
                    field.ty = sema_ty;
                }
                None => continue,
            }

            if field
                .ty
                .as_union_symbol_id()
                .map(|symbol_id| symbol_id == typed_union.symbol_id)
                == Some(true)
            {
                let type_name = (self.symbol_formatter)(scope_id_opt)(typed_union.symbol_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InfiniteRecursiveType { type_name },
                    location: Some(DiagLoc::new(field.loc.clone())),
                    hint: None,
                });
                continue;
            }

            field_names.push(field.name.clone());
        }

        update_union_symbol_entry(self, &typed_union);
    }

    pub(crate) fn analyze_enum(&mut self, scope_id_opt: Option<ScopeID>, typed_enum: &mut TypedEnumStmt, is_local: bool) {
        fn update_enum_symbol_entry<'b>(this: &mut AnalysisContext<'b>, typed_enum: &TypedEnumStmt) {
            if let Some(scope_id) = typed_enum.is_local {
                update_local_symbol!(this, scope_id, typed_enum.symbol_id,
                    LocalSymbolKind::Enum(resolved_enum) => resolved_enum, {
                        resolved_enum.enum_sig.variants = typed_enum.variants.clone();
                    }
                )
            } else {
                update_global_symbol!(this, typed_enum.module_id, typed_enum.symbol_id,
                    SymbolEntryKind::Enum(resolved_enum) => resolved_enum, {
                        resolved_enum.enum_sig.variants = typed_enum.variants.clone();
                    }
                );
            }
        }

        self.check_enum_name(typed_enum.name.clone(), typed_enum.loc.clone(), is_local);
        self.analyze_methods(self.module_id, &typed_enum.methods);

        let mut variant_names: Vec<String> = Vec::new();

        for variant in &mut typed_enum.variants {
            let variant_identifier = match variant {
                TypedEnumVariant::Identifier(identifier) => identifier,
                TypedEnumVariant::Valued(identifier, typed_expr) => {
                    typed_expr.sema_ty = match self.analyze_typed_expr_type(scope_id_opt, typed_expr, None) {
                        Some(sema_ty) => Some(sema_ty),
                        None => continue,
                    };
                    identifier
                }
                TypedEnumVariant::Variant(identifier, typed_enum_valued_fields) => {
                    for field in typed_enum_valued_fields {
                        field.field_type =
                            match self.normalize_type(scope_id_opt, field.field_type.clone(), field.loc.clone()) {
                                Some(sema_ty) => sema_ty,
                                None => continue,
                            };

                        if field
                            .field_type
                            .as_enum_symbol_id()
                            .map(|symbol_id| symbol_id == typed_enum.symbol_id)
                            == Some(true)
                        {
                            let type_name = (self.symbol_formatter)(scope_id_opt)(typed_enum.symbol_id);

                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: AnalyzerDiagKind::InfiniteRecursiveType { type_name },
                                location: Some(DiagLoc::new(field.loc.clone())),
                                hint: None,
                            });
                            continue;
                        }
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

        update_enum_symbol_entry(self, &typed_enum);
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
        return_type: &mut SemanticType,
        params: &mut TypedFuncParams,
        body: &mut TypedBlockStmt,
        vis_opt: Option<AccessSpecifier>,
        loc: SourceLoc,
    ) {
        self.current_func = Some(TypedFuncType {
            def_module_id: Some(self.module_id),
            params: typed_func_params_as_func_type_params(params),
            return_type: Box::new(return_type.clone()),
            vis_opt,
            loc: loc.clone(),
        });
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
            Some(sema_ty) => sema_ty,
            None => return,
        };

        self.normalize_func_params(params, loc);
    }

    fn analyze_methods(&mut self, module_id: ModuleID, methods: &HashMap<String, SymbolID>) {
        let mut local_methods_list: Vec<(SymbolID, FuncSig, Box<TypedBlockStmt>)> = Vec::new();

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

            self.current_method_symbol_id = Some(*symbol_id);
            self.current_func = Some(TypedFuncType {
                def_module_id: Some(self.module_id),
                params: typed_func_params_as_func_type_params(&func_sig.params),
                return_type: Box::new(func_sig.return_type.clone()),
                vis_opt: Some(func_sig.vis.clone()),
                loc: func_sig.loc.clone(),
            });
            self.check_method_name(func_sig.name.clone(), func_sig.loc.clone());

            // public methods are allowed to not be used
            if func_sig.vis.is_public() {
                self.mark_symbol_used_once(module_id, *symbol_id);
            }

            self.normalize_func_params(&mut func_sig.params, func_sig.loc.clone());

            func_sig.return_type = match self.normalize_type(None, func_sig.return_type.clone(), func_sig.loc.clone()) {
                Some(sema_ty) => sema_ty,
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
            self.current_method_symbol_id = Some(symbol_id);
            self.current_func = Some(TypedFuncType {
                def_module_id: Some(self.module_id),
                params: typed_func_params_as_func_type_params(&func_sig.params),
                return_type: Box::new(func_sig.return_type.clone()),
                vis_opt: Some(func_sig.vis.clone()),
                loc: func_sig.loc.clone(),
            });
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

        self.current_method_symbol_id = None;
    }

    fn analyze_func_def(&mut self, typed_func_def: &mut TypedFuncDefStmt) {
        if typed_func_def.name == "main" {
            let mut entry_points = self.entry_points.lock().unwrap();
            entry_points.push(typed_func_def.loc.clone());
        }

        if typed_func_def.vis.is_public() {
            self.mark_symbol_used_once(self.module_id, typed_func_def.symbol_id);
        }

        self.analyze_any_func_def(
            &mut typed_func_def.return_type,
            &mut typed_func_def.params,
            &mut typed_func_def.body,
            Some(typed_func_def.vis.clone()),
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

                    if matches!(normalized_type, SemanticType::BasicType(BasicType::Void)) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::VoidVariableType,
                            location: Some(DiagLoc::new(typed_func_param.loc.clone())),
                            hint: None,
                        });
                        continue;
                    }

                    typed_func_param.ty = normalized_type.clone();
                }
                TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
                    let normalized_type = self
                        .normalize_type(
                            None,
                            SemanticType::UnresolvedSymbol(typed_self_modifier.symbol_id.unwrap()),
                            typed_self_modifier.loc.clone(),
                        )
                        .unwrap();

                    match typed_self_modifier.kind {
                        SelfModifierKind::Copied => {
                            typed_self_modifier.ty = Some(normalized_type);
                        }
                        SelfModifierKind::Referenced => {
                            typed_self_modifier.ty = Some(SemanticType::Pointer(Box::new(normalized_type)));
                        }
                    }
                }
            }
        }

        if let Some(variadic_params) = &mut params.variadic {
            if let TypedFuncVariadicParams::Typed(identifier, sema_ty) = variadic_params {
                let normalized_concrete_type = match self.normalize_type(None, sema_ty.clone(), loc.clone()) {
                    Some(sema_ty) => sema_ty,
                    None => return,
                };

                if matches!(
                    normalized_concrete_type,
                    SemanticType::BasicType(BasicType::Void)
                ) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::VoidVariableType,
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: None,
                    });
                }

                *variadic_params = TypedFuncVariadicParams::Typed(identifier.clone(), normalized_concrete_type);
            }
        }
    }

    pub(crate) fn normalize_func_type_params(&mut self, params: &mut TypedFuncTypeParams, loc: SourceLoc) {
        // analyze static arguments
        for param in params.list.iter_mut() {
            let normalized_type = self.normalize_type(None, param.clone(), loc.clone()).unwrap();

            if matches!(normalized_type, SemanticType::BasicType(BasicType::Void)) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::VoidVariableType,
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
                continue;
            }

            *param = normalized_type.clone();
        }

        if let Some(variadic_params) = &mut params.variadic {
            match *variadic_params.clone() {
                TypedFuncTypeVariadicParams::UntypedCStyle => {}
                TypedFuncTypeVariadicParams::Typed(sema_ty) => {
                    let normalized_concrete_type = match self.normalize_type(None, sema_ty.clone(), loc.clone()) {
                        Some(sema_ty) => sema_ty,
                        None => return,
                    };

                    if matches!(
                        normalized_concrete_type,
                        SemanticType::BasicType(BasicType::Void)
                    ) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::VoidVariableType,
                            location: Some(DiagLoc::new(loc.clone())),
                            hint: None,
                        });
                    }

                    *variadic_params = Box::new(TypedFuncTypeVariadicParams::Typed(normalized_concrete_type));
                }
            }
        }
    }

    fn analyze_func_decl(&mut self, typed_func_decl: &mut TypedFuncDeclStmt) {
        self.check_duplicate_param_names(
            &typed_func_decl.params.list,
            typed_func_decl.params.variadic.as_ref(),
            DiagLoc::new(typed_func_decl.loc.clone()),
        );

        typed_func_decl.return_type =
            match self.normalize_type(None, typed_func_decl.return_type.clone(), typed_func_decl.loc.clone()) {
                Some(sema_ty) => sema_ty,
                None => return,
            };

        self.normalize_func_params(&mut typed_func_decl.params, typed_func_decl.loc.clone());
    }

    fn analyze_interface(&mut self, typed_interface: &TypedInterfaceStmt) {
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

    fn analyze_typedef(&mut self, scope_id_opt: Option<ScopeID>, typed_typedef: &mut TypedTypedefStmt) {
        typed_typedef.ty = match self.normalize_type(scope_id_opt, typed_typedef.ty.clone(), typed_typedef.loc.clone())
        {
            Some(sema_ty) => sema_ty,
            None => return,
        };
    }

    fn analyze_variable(&mut self, scope_id_opt: Option<ScopeID>, typed_variable: &mut TypedVarStmt) {
        if typed_variable.ty.is_none() && typed_variable.rhs.is_none() {
            return;
        }

        let value_type_opt = {
            if let Some(typed_expr) = &mut typed_variable.rhs {
                if let Some(sema_ty) = &typed_variable.ty {
                    typed_variable.ty =
                        self.normalize_type(scope_id_opt, sema_ty.clone(), typed_variable.loc.clone());
                }

                let sema_ty =
                    match self.analyze_typed_expr_type(scope_id_opt, typed_expr, typed_variable.ty.clone()) {
                        Some(sema_ty) => sema_ty,
                        None => return,
                    };

                typed_expr.sema_ty = Some(sema_ty.clone());
                Some(sema_ty)
            } else {
                None
            }
        };

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        if let Some(sema_ty) = &typed_variable.ty {
            typed_variable.ty = self.normalize_type(scope_id_opt, sema_ty.clone(), typed_variable.loc.clone());

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

            if typed_variable.is_const && !matches!(typed_variable.ty, Some(SemanticType::Const(..))) {
                typed_variable.ty = Some(SemanticType::Const(Box::new(typed_variable.ty.clone().unwrap())));
            }
        }

        if matches!(
            typed_variable.ty,
            Some(SemanticType::BasicType(BasicType::Void))
        ) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::VoidVariableType,
                location: Some(DiagLoc::new(typed_variable.loc.clone())),
                hint: None,
            });
        }

        if matches!(typed_variable.ty, Some(SemanticType::FuncType(..))) && typed_variable.rhs.is_none() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::UninitializedLambda,
                location: Some(DiagLoc::new(typed_variable.loc.clone())),
                hint: None,
            });
            return;
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

        local_scope_opt.inspect(|local_scope| {
            let mut local_scope_ref = local_scope.borrow_mut();
            local_scope_ref.insert(
                typed_variable.name.clone(),
                LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                    module_id: self.module_id,
                    symbol_id: typed_variable.symbol_id,
                    typed_variable: typed_variable.clone(),
                })),
            );
            drop(local_scope_ref);
        });
    }

    pub(crate) fn analyze_assignment(&mut self, scope_id_opt: Option<ScopeID>, assign: &mut TypedAssignExpr) {
        let lhs_type = match self.analyze_typed_expr_type(scope_id_opt, &mut assign.lhs, None) {
            Some(sema_ty) => sema_ty,
            None => return,
        };

        let rhs_type = match self.analyze_typed_expr_type(scope_id_opt, &mut assign.rhs, Some(lhs_type.clone())) {
            Some(sema_ty) => sema_ty,
            None => return,
        }
        .as_rvalue(true);

        if lhs_type.as_rvalue(true).is_const() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::CannotAssignToConstLValue,
                location: Some(DiagLoc::new(assign.loc.clone())),
                hint: None,
            });
            return;
        }

        if assign.kind == AssignmentKind::Default {
            if !self.check_type_mismatch(scope_id_opt, rhs_type.clone(), lhs_type.as_rvalue(true).clone(), assign.loc.clone()) {
                let lhs_type = format_concrete_type(lhs_type, &(self.symbol_formatter)(scope_id_opt));
                let rhs_type = format_concrete_type(rhs_type, &(self.symbol_formatter)(scope_id_opt));

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
