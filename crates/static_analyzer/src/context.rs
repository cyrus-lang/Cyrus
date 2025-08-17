use crate::diagnostics::AnalyzerDiagKind;
use diagcentral::{Diag, DiagLevel, DiagLoc, reporter::DiagReporter};
use resolver::{
    Resolver,
    scope::{LocalOrGlobalSymbol, LocalSymbol, LocalSymbolKind, ResolvedFunction, SymbolEntry, SymbolEntryKind},
};
use std::mem;
use typed_ast::{format::format_concrete_type, *};

#[derive(Debug)]
enum ControlContext {
    For(TypedFor),
    Foreach(TypedForeach),
    Switch(TypedSwitch),
}

pub struct AnalysisContext<'a> {
    pub ast: &'a mut TypedProgramTree,
    pub resolver: &'a Resolver,
    pub reporter: DiagReporter<AnalyzerDiagKind>,
    pub module_id: ModuleID,
    pub symbol_formatter: Box<dyn Fn(Option<ScopeID>) -> Box<dyn Fn(SymbolID) -> String + 'a> + 'a>,
    control_stack: Vec<ControlContext>,
    cur_func_symbol_id: Option<SymbolID>,
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
    pub fn new(resolver: &'a Resolver, module_id: ModuleID, ast: &'a mut TypedProgramTree) -> Self {
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
        }
    }

    // Traverse TypedAST
    pub fn analyze(&mut self) {
        let mut body = mem::take(&mut self.ast.body);

        for mut typed_stmt in &mut body {
            match &mut typed_stmt {
                TypedStatement::GlobalVariable(typed_global_var) => self.analyze_global_var(typed_global_var),
                TypedStatement::FuncDef(typed_func_def) => {
                    self.cur_func_symbol_id = Some(typed_func_def.symbol_id);
                    let state = self.analyze_block_statement(&mut typed_func_def.body);

                    if !typed_func_def.return_type.is_void() && state != FlowState::Returns {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::MissingReturn,
                            location: Some(DiagLoc::new(
                                self.resolver.get_current_module_file_path(),
                                typed_func_def.loc.clone(),
                                0,
                            )),
                            hint: Some("not all control paths return a value.".to_string()),
                        });
                    }
                }
                TypedStatement::FuncDecl(typed_func_decl) => self.analyze_func_decl(typed_func_decl),
                TypedStatement::Interface(typed_interface) => self.analyze_interface(typed_interface),
                TypedStatement::Struct(typed_struct) => self.analyze_struct(typed_struct, false),
                TypedStatement::Enum(typed_enum) => self.analyze_enum(typed_enum, false),
                TypedStatement::Typedef(typed_typedef) => self.analyze_typedef(typed_typedef, false),
                // Not analyzed
                TypedStatement::Import(_) => continue,
                // Invalid top-level statements
                TypedStatement::Variable(_)
                | TypedStatement::BlockStatement(_)
                | TypedStatement::If(_)
                | TypedStatement::Return(_)
                | TypedStatement::Break(_)
                | TypedStatement::Continue(_)
                | TypedStatement::For(_)
                | TypedStatement::Foreach(_)
                | TypedStatement::Switch(_)
                | TypedStatement::Expression(_) => {
                    unreachable!()
                }
            }
        }

        self.analyze_unused_symbols();
        self.ast.body = body;
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
                TypedStatement::If(typed_if) => self.analyze_if_stmt(block_stmt.scope_id, typed_if),
                TypedStatement::Return(typed_return) => self.analyze_return(block_stmt.scope_id, typed_return),
                TypedStatement::Break(typed_break) => {
                    self.analyze_break(typed_break);
                    FlowState::Unreachable
                }
                TypedStatement::Continue(typed_continue) => {
                    self.analyze_continue(typed_continue);
                    FlowState::Unreachable
                }
                TypedStatement::For(typed_for) => self.analyze_for_loop(Some(typed_for.body.scope_id), typed_for),
                TypedStatement::Foreach(typed_foreach) => todo!(),
                TypedStatement::Switch(typed_switch) => todo!(),
                TypedStatement::Struct(typed_struct) => {
                    self.analyze_struct(typed_struct, true);
                    FlowState::Reachable
                }
                TypedStatement::Enum(typed_enum) => {
                    self.analyze_enum(typed_enum, true);
                    FlowState::Reachable
                }
                TypedStatement::Expression(typed_expression) => {
                    self.get_typed_expr_type(Some(block_stmt.scope_id), typed_expression);
                    FlowState::Reachable
                }
                TypedStatement::Interface(typed_interface) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Warning,
                        kind: AnalyzerDiagKind::InternalInterfaceIsNotValid,
                        location: Some(DiagLoc::new(
                            self.resolver.get_current_module_file_path(),
                            typed_interface.loc.clone(),
                            0,
                        )),
                        hint: None,
                    });
                    FlowState::Reachable
                }
                TypedStatement::Typedef(typed_typedef) => {
                    self.analyze_typedef(typed_typedef, false);
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

    fn merge_flow_state(&self, a: FlowState, b: FlowState) -> FlowState {
        match (a, b) {
            (FlowState::Returns, FlowState::Returns) => FlowState::Returns,
            (FlowState::Unreachable, FlowState::Unreachable) => FlowState::Unreachable,
            _ => FlowState::Reachable,
        }
    }
    fn analyze_if_stmt(&mut self, scope_id: ScopeID, typed_if: &mut TypedIf) -> FlowState {
        let consequent_state = self.analyze_block_statement(&mut typed_if.consequent);

        let alternate_state = {
            if let Some(block_stmt) = &mut typed_if.alternate {
                self.analyze_block_statement(&mut *block_stmt)
            } else {
                FlowState::Reachable
            }
        };

        self.merge_flow_state(consequent_state, alternate_state)
    }

    fn analyze_for_loop(&mut self, scope_id_opt: Option<ScopeID>, typed_for: &mut TypedFor) -> FlowState {
        if let Some(initializer) = &mut typed_for.initializer {
            self.analyze_variable(scope_id_opt, initializer);
        }

        if let Some(typed_expr) = &mut typed_for.condition {
            if let Some(concrete_type) = self.get_typed_expr_type(scope_id_opt, typed_expr) {
                self.check_expr_type_must_be_condition(concrete_type, typed_for.loc.clone());
            }
        }

        if let Some(typed_expr) = &mut typed_for.increment {
            self.get_typed_expr_type(scope_id_opt, typed_expr);
        }

        self.control_stack.push(ControlContext::For(typed_for.clone()));
        self.analyze_block_statement(&mut typed_for.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    fn analyze_return(&mut self, scope_id: ScopeID, typed_return: &mut TypedReturn) -> FlowState {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(Some(scope_id));

        let resolved_func = self.get_cur_func_symbol_id();
        let return_type = resolved_func.func_sig.return_type;

        if return_type.is_void() && typed_return.argument.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::VoidFunctionReturnsValue,
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    typed_return.loc.clone(),
                    0,
                )),
                hint: None,
            });
        } else if let Some(typed_expr) = &mut typed_return.argument {
            if let Some(concrete_type) = self.get_typed_expr_type(Some(scope_id), typed_expr) {
                let expected = format_concrete_type(return_type.clone(), &formatter_closure);
                let got = format_concrete_type(concrete_type.clone(), &formatter_closure);

                if !self.check_type_mismatch(concrete_type, return_type) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::ReturnStatementTypeMismatch { expected, got },
                        location: Some(DiagLoc::new(
                            self.resolver.get_current_module_file_path(),
                            typed_return.loc.clone(),
                            0,
                        )),
                        hint: None,
                    });
                }
            }
        } else if !return_type.is_void() && typed_return.argument.is_none() {
            let argument_type = format_concrete_type(return_type.clone(), &formatter_closure);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ReturnStatementNeedsAnArgument { argument_type },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    typed_return.loc.clone(),
                    0,
                )),
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
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    typed_break.loc.clone(),
                    0,
                )),
                hint: None,
            });
        }
    }

    fn analyze_continue(&mut self, typed_continue: &TypedContinue) {
        let inside_loop = self
            .control_stack
            .iter()
            .rev()
            .any(|ctx| matches!(ctx, ControlContext::For(_) | ControlContext::Foreach(_)));

        if !inside_loop {
            // continue cannot be used outside of a loop
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidContinueStatement,
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    typed_continue.loc.clone(),
                    0,
                )),
                hint: None,
            });
        }
    }

    fn report_unreachable_block_diag(&mut self, typed_stmt: &TypedStatement) {
        self.reporter.report(Diag {
            level: DiagLevel::Warning,
            kind: AnalyzerDiagKind::UnreachableCode,
            location: Some(DiagLoc::new(
                self.resolver.get_current_module_file_path(),
                typed_stmt.get_loc(),
                0,
            )),
            hint: None,
        });
    }

    fn get_cur_func_symbol_id(&self) -> ResolvedFunction {
        let symbol_id = self.cur_func_symbol_id.unwrap();
        let symbol_entry = self
            .resolver
            .lookup_symbol_entry_with_id(self.module_id, symbol_id)
            .unwrap();
        symbol_entry.as_func().unwrap().clone()
    }

    pub(crate) fn analyze_global_var(&mut self, typed_global_var: &mut TypedGlobalVariable) {
        if let Some(expr) = &mut typed_global_var.expr {
            if !expr.kind.is_comptime_valid() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::GlobalVariableExprNotComptimeValid,
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        typed_global_var.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return;
            }

            self.get_typed_expr_type(None, expr);
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
                };

                self.reporter.report(Diag {
                    level: DiagLevel::Warning,
                    kind: AnalyzerDiagKind::UnusedSymbol { symbol_name },
                    location: Some(DiagLoc::new(self.resolver.get_current_module_file_path(), loc, 0)),
                    hint: None,
                });
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
                    SymbolEntryKind::Interface(resolved_interface) => (
                        resolved_interface.interface_sig.name.clone(),
                        resolved_interface.interface_sig.loc.clone(),
                    ),
                };

                self.reporter.report(Diag {
                    level: DiagLevel::Warning,
                    kind: AnalyzerDiagKind::UnusedSymbol { symbol_name },
                    location: Some(DiagLoc::new(self.resolver.get_current_module_file_path(), loc, 0)),
                    hint: None,
                });
            }
        }

        drop(global_symbols);
    }

    pub(crate) fn analyze_struct(&mut self, typed_struct: &TypedStruct, is_local: bool) {
        self.check_struct_name(typed_struct.name.clone(), typed_struct.loc.clone(), is_local);

        for symbol_id in typed_struct.methods.values() {
            let symbol_entry = self
                .resolver
                .lookup_symbol_entry_with_id(self.module_id, *symbol_id)
                .unwrap();
            let resolved_method = symbol_entry.as_method().unwrap();

            self.check_method_name(
                resolved_method.func_sig.name.clone(),
                resolved_method.func_sig.loc.clone(),
            );
        }

        let mut field_names: Vec<String> = Vec::new();

        for field in &typed_struct.fields {
            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DuplicateFieldName {
                        struct_name: typed_struct.name.clone(),
                        field_name: field.name.clone(),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        field.loc.clone(),
                        0,
                    )),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
                continue;
            }

            field_names.push(field.name.clone());
        }
    }

    pub(crate) fn analyze_enum(&mut self, typed_enum: &TypedEnum, is_local: bool) {
        self.check_enum_name(typed_enum.name.clone(), typed_enum.loc.clone(), is_local);

        for symbol_id in typed_enum.methods.values() {
            let symbol_entry = self
                .resolver
                .lookup_symbol_entry_with_id(self.module_id, *symbol_id)
                .unwrap();
            let resolved_method = symbol_entry.as_method().unwrap();

            self.check_method_name(
                resolved_method.func_sig.name.clone(),
                resolved_method.func_sig.loc.clone(),
            );
        }

        let mut variant_names: Vec<String> = Vec::new();

        for variant in &typed_enum.variants {
            let variant_identifier = match variant {
                TypedEnumVariant::Identifier(identifier) => identifier,
                TypedEnumVariant::Valued(identifier, _) => identifier,
                TypedEnumVariant::Variant(identifier, typed_enum_valued_fields) => {
                    let mut field_names: Vec<String> = Vec::new();

                    for field in typed_enum_valued_fields {
                        if field_names.contains(&field.name) {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: AnalyzerDiagKind::DuplicateEnumFieldName {
                                    enum_name: typed_enum.name.clone(),
                                    field_name: field.name.clone(),
                                    variant_name: identifier.name.clone(),
                                },
                                location: Some(DiagLoc::new(
                                    self.resolver.get_current_module_file_path(),
                                    field.loc.clone(),
                                    0,
                                )),
                                hint: Some("Consider to rename the field to a different name.".to_string()),
                            });
                            continue;
                        }

                        field_names.push(field.name.clone());
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
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        variant_identifier.loc.clone(),
                        0,
                    )),
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

    fn analyze_func_decl(&mut self, typed_func_decl: &TypedFuncDecl) {
        self.check_duplicate_param_names(
            &typed_func_decl.params.list,
            typed_func_decl.params.variadic.as_ref(),
            DiagLoc::new(
                self.resolver.get_current_module_file_path(),
                typed_func_decl.loc.clone(),
                0,
            ),
        );
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
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        method.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                continue;
            }

            name_list.push(method.name.clone());
        }
    }

    fn analyze_typedef(&mut self, typed_typedef: &TypedTypedef, is_local: bool) {
        self.check_typedef_name(typed_typedef.name.clone(), typed_typedef.loc.clone(), is_local);
    }

    fn analyze_variable(&mut self, scope_id_opt: Option<ScopeID>, typed_variable: &mut TypedVariable) {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        if typed_variable.ty.is_none() && typed_variable.rhs.is_none() {
            return;
        }

        let value_type_opt = {
            if let Some(typed_expr) = &mut typed_variable.rhs {
                self.get_typed_expr_type(scope_id_opt, typed_expr)
            } else {
                None
            }
        };

        if let Some(var_type) = &typed_variable.ty {
            if let Some(value_type) = value_type_opt {
                let lhs_type = format_concrete_type(var_type.clone(), &formatter_closure);
                let rhs_type = format_concrete_type(value_type.clone(), &formatter_closure);

                if !self.check_type_mismatch(value_type.clone(), var_type.clone()) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type },
                        location: Some(DiagLoc::new(
                            self.resolver.get_current_module_file_path(),
                            typed_variable.loc.clone(),
                            0,
                        )),
                        hint: None,
                    });
                }
            }
        }
    }

    pub(crate) fn analyze_assignment(&mut self, scope_id_opt: Option<ScopeID>, typed_assignment: &mut TypedAssignment) {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let lhs_type = match self.get_typed_expr_type(scope_id_opt, &mut typed_assignment.lhs) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        let rhs_type = match self.get_typed_expr_type(scope_id_opt, &mut typed_assignment.rhs) {
            Some(concrete_type) => concrete_type,
            None => return,
        };

        if lhs_type.is_const() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::CannotAssignToConstLValue,
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    typed_assignment.loc.clone(),
                    0,
                )),
                hint: None,
            });
            return;
        }

        if !self.check_type_mismatch(rhs_type.clone(), lhs_type.clone()) {
            let lhs_type = format_concrete_type(lhs_type, &formatter_closure);
            let rhs_type = format_concrete_type(rhs_type, &formatter_closure);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    typed_assignment.loc.clone(),
                    0,
                )),
                hint: None,
            });
        }
    }
}
