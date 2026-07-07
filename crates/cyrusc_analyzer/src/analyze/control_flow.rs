// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::Ident;
use cyrusc_const_eval::fold::ConstFolder;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::flow_state::{ControlRegion, FlowState};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::EnumDecl,
    format::{format_enum_decl, format_sema_type},
    stmts::{
        TypedBreakStmt, TypedContinueStmt, TypedEnumVariant, TypedForStmt, TypedIfStmt, TypedReturnStmt,
        TypedSwitchCasePattern, TypedSwitchCasePatternKind, TypedSwitchStmt, TypedWhileStmt,
    },
    substitute::instantiate_enum_decl_with_type_args,
    types::{PlainType, SemaType},
};
use fx_hash::{FxHashSet, FxHashSetExt};

// Switch.
impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_switch(&mut self, switch_stmt: &mut TypedSwitchStmt) -> FlowState {
        if self.analyze_expr(&mut switch_stmt.operand, None).is_none() {
            return FlowState::Reachable;
        }

        // expand operand type
        switch_stmt.operand.ty = {
            if let Some(ty) = &switch_stmt.operand.ty {
                Some(self.expand_sema_type(ty.clone(), switch_stmt.loc))
            } else {
                return FlowState::Reachable;
            }
        };

        let operand_type = switch_stmt.operand.ty.clone().unwrap();

        self.with_control_region(ControlRegion::Switch, |this| {
            if switch_stmt.cases.is_empty() {
                this.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::EmptyCaseSwitchStatement),
                    loc: Some(switch_stmt.loc),
                    hint: None,
                });
                return FlowState::Reachable;
            }

            if operand_type.is_enum() {
                this.analyze_switch_on_enum(switch_stmt, &operand_type)
            } else if operand_type.is_plain_type() || operand_type.is_uint8_pointer() {
                this.analyze_switch_on_value(switch_stmt, &operand_type)
            } else {
                let expr_type = format_sema_type(operand_type.clone(), this.formatter);

                this.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::SwitchOperandIsNotEnum { expr_type }),
                    loc: Some(switch_stmt.loc),
                    hint: None,
                });
                FlowState::Reachable
            }
        })
    }

    fn analyze_switch_on_value(&mut self, switch_stmt: &mut TypedSwitchStmt, operand_type: &SemaType) -> FlowState {
        let mut flow_states = Vec::new();

        for case in &mut switch_stmt.cases {
            self.analyze_switch_on_value_case_patterns(&mut case.patterns, operand_type, case.loc);

            flow_states.push(self.analyze_block_stmt(&mut case.body));
        }

        switch_stmt.all_cases_covered = Some(false);

        if let Some(default) = &mut switch_stmt.default_case {
            flow_states.push(self.analyze_block_stmt(default));
        } else {
            flow_states.push(FlowState::Reachable);
        }

        if self.all_flow_states_return(&flow_states) {
            FlowState::Returns
        } else if self.all_flow_states_are_unreachable(&flow_states) {
            FlowState::Unreachable
        } else {
            FlowState::Reachable
        }
    }

    fn analyze_switch_on_value_case_patterns(
        &mut self,
        patterns: &mut Vec<TypedSwitchCasePattern>,
        operand_type: &SemaType,
        case_loc: Loc,
    ) {
        let mut range_table: Vec<(usize, usize)> = Vec::new();

        fn is_valid_range(existing: &[(usize, usize)], new_ranges: &[(usize, usize)]) -> bool {
            for (new_start, new_end) in new_ranges {
                if new_start > new_end {
                    return false;
                }
                for (old_start, old_end) in existing {
                    let overlaps = new_start <= old_end && old_start <= new_end;
                    if overlaps {
                        return false;
                    }
                }
            }
            true
        }

        fn analyze_pattern(
            this: &mut AnalysisContext,
            pattern: &mut TypedSwitchCasePattern,
            operand_type: &SemaType,
        ) -> Option<Vec<(usize, usize)>> {
            let mut pattern_range_table: Vec<(usize, usize)> = Vec::new();

            match &mut pattern.kind {
                TypedSwitchCasePatternKind::EnumUnit(_)
                | TypedSwitchCasePatternKind::EnumStructVariant { .. }
                | TypedSwitchCasePatternKind::EnumTupleVariant { .. }
                | TypedSwitchCasePatternKind::Binding { .. }
                | TypedSwitchCasePatternKind::Wildcard => {
                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InvalidSwitchCasePattern),
                        loc: Some(pattern.loc),
                        hint: None,
                    });
                }

                TypedSwitchCasePatternKind::Range(range) => {
                    this.analyze_expr(&mut range.lower, Some(operand_type.clone()));
                    this.analyze_expr(&mut range.upper, Some(operand_type.clone()));

                    let mut const_folder =
                        ConstFolder::new(this, &this.decl_tables, this.target, this.tctx.clone(), this);

                    let lower = match const_folder.expr_as_const_int(&range.lower, this) {
                        Some(value) => value,
                        None => return None,
                    };

                    let upper = match const_folder.expr_as_const_int(&range.upper, this) {
                        Some(value) => value,
                        None => return None,
                    };

                    if lower >= upper {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::InvalidRange),
                            loc: Some(range.loc),
                            hint: None,
                        });
                        return None;
                    }

                    pattern_range_table.push((lower.try_into().unwrap(), upper.try_into().unwrap()));
                }
                TypedSwitchCasePatternKind::Expr(expr) => {
                    let Some(expr_type) = this.analyze_expr(expr, Some(operand_type.clone())) else {
                        return None;
                    };

                    if !this.is_assignable_to(expr_type.clone(), operand_type.clone(), expr.loc) {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::IncompatibleSwitchPatternType {
                                operand_type: format_sema_type(operand_type.clone(), this.formatter),
                                pattern_type: format_sema_type(expr_type, this.formatter),
                            }),
                            loc: Some(expr.loc),
                            hint: None,
                        });
                    }
                }
            }

            Some(pattern_range_table)
        }

        for pattern in patterns {
            let Some(pattern_range_table) = analyze_pattern(self, pattern, operand_type) else {
                continue;
            };

            if is_valid_range(&range_table, &pattern_range_table) {
                range_table.extend(pattern_range_table);
            } else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::OverlappingSwitchCaseRange),
                    loc: Some(case_loc),
                    hint: None,
                });
                return;
            }
        }
    }

    fn analyze_switch_on_enum(&mut self, switch_stmt: &mut TypedSwitchStmt, operand_type: &SemaType) -> FlowState {
        let named_type = operand_type.as_named_type().unwrap();
        let enum_decl_id = named_type.type_decl_id.as_enum().unwrap();
        let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

        let mut inst_enum_decl = instantiate_enum_decl_with_type_args(&enum_decl, &named_type.type_args);

        let all_variants_count = inst_enum_decl.variants.len();
        let mut all_covered_variants = FxHashSet::new();

        let mut flow_states = Vec::new();

        for case in &mut switch_stmt.cases {
            let covered_variants = self.analyze_switch_on_enum_case_patterns(&case.patterns, &mut inst_enum_decl);

            flow_states.push(self.analyze_block_stmt(&mut case.body));
            all_covered_variants.extend(covered_variants);
        }

        switch_stmt.all_cases_covered = Some(false);

        if let Some(default) = &mut switch_stmt.default_case {
            let flow_state = self.analyze_block_stmt(default);

            flow_states.push(flow_state);
        } else {
            // check that all enum variants are covered inside switch patterns:
            if all_covered_variants.len() == all_variants_count {
                // if yes, don't push anything.
                switch_stmt.all_cases_covered = Some(true);
            } else {
                // if no, we push an extra reachable flow state intentionally.
                flow_states.push(FlowState::Reachable);
            }
        }

        if self.all_flow_states_terminate(&flow_states) {
            if self.all_flow_states_are_unreachable(&flow_states) {
                FlowState::Unreachable
            } else {
                FlowState::Returns
            }
        } else {
            FlowState::Reachable
        }
    }

    fn analyze_switch_on_enum_case_patterns(
        &mut self,
        patterns: &Vec<TypedSwitchCasePattern>,
        inst_enum_decl: &mut EnumDecl,
    ) -> FxHashSet<String> {
        let enum_name = format_enum_decl(&inst_enum_decl, self.formatter);

        let mut exporting_pattern_count = 0;
        let mut covered_variants = FxHashSet::new();

        fn find_variant<'a>(enum_decl: &'a mut EnumDecl, ident: &Ident) -> Option<&'a mut TypedEnumVariant> {
            enum_decl.variants.iter_mut().find(|variant| match variant {
                TypedEnumVariant::Unit(_ident)
                | TypedEnumVariant::Valued { ident: _ident, .. }
                | TypedEnumVariant::Tuple { ident: _ident, .. }
                | TypedEnumVariant::Struct { ident: _ident, .. } => _ident == ident,
            })
        }

        fn expose_pattern_bindings<'a>(
            this: &mut AnalysisContext<'a>,
            pattern: &TypedSwitchCasePattern,
            ty: &SemaType,
        ) {
            match &pattern.kind {
                TypedSwitchCasePatternKind::Binding { var_decl_id, .. } => {
                    // assign inferred type to the variable
                    this.decl_tables.with_var_decl_mut(*var_decl_id, |_var_decl| {
                        _var_decl.ty = Some(ty.clone());
                    });
                }
                TypedSwitchCasePatternKind::Wildcard => { /* ignore exposing */ }

                _ => {}
            }
        }

        fn analyze_pattern<'a>(
            this: &mut AnalysisContext<'a>,
            pattern: &TypedSwitchCasePattern,
            inst_enum_decl: &mut EnumDecl,
            enum_name: &String,
            exporting_pattern_count: &mut usize,
            covered_variants: &mut FxHashSet<String>,
        ) {
            match &pattern.kind {
                TypedSwitchCasePatternKind::Wildcard => {
                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::OnlyVariantPatternIsAllowedInSwitch),
                        loc: Some(pattern.loc),
                        hint: None,
                    });
                }

                TypedSwitchCasePatternKind::Binding { .. } => {
                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::OnlyVariantPatternIsAllowedInSwitch),
                        loc: Some(pattern.loc),
                        hint: None,
                    });
                }

                // .EnumUnit
                TypedSwitchCasePatternKind::EnumUnit(ident) => {
                    let exists = find_variant(inst_enum_decl, ident).is_some();

                    if !exists {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                                enum_name: enum_name.clone(),
                                variant_name: ident.as_string(),
                            }),
                            loc: Some(pattern.loc),
                            hint: None,
                        });
                    }

                    if !covered_variants.insert(ident.as_string()) {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::DuplicateEnumVariantInSwitchPatterns {
                                variant_name: ident.as_string(),
                            }),
                            loc: Some(pattern.loc),
                            hint: None,
                        });
                    }
                }

                // Tuple Variant
                TypedSwitchCasePatternKind::EnumTupleVariant { ident, items } => {
                    let Some(enum_variant) = find_variant(inst_enum_decl, ident) else {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                                enum_name: enum_name.clone(),
                                variant_name: ident.as_string(),
                            }),
                            loc: Some(pattern.loc),
                            hint: None,
                        });
                        return;
                    };

                    if !covered_variants.insert(ident.as_string()) {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::DuplicateEnumVariantInSwitchPatterns {
                                variant_name: ident.as_string(),
                            }),
                            loc: Some(pattern.loc),
                            hint: None,
                        });
                    }

                    match enum_variant {
                        TypedEnumVariant::Tuple { fields, .. } => {
                            if fields.len() != items.len() {}

                            *exporting_pattern_count += 1;

                            for (pattern, field) in items.iter().zip(fields) {
                                expose_pattern_bindings(this, pattern, &field.ty);
                            }
                        }
                        TypedEnumVariant::Valued { value, .. } => {
                            *exporting_pattern_count += 1;

                            if items.len() != 1 {
                                this.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::ValuedEnumVariantCanOnlyExportOneField {
                                        variant_name: ident.as_string(),
                                    }),
                                    loc: Some(pattern.loc),
                                    hint: None,
                                });
                                return;
                            }

                            let pattern = items.first().unwrap();

                            if let Some(expr_type) = this.analyze_expr(value, None) {
                                expose_pattern_bindings(this, pattern, &expr_type);
                            }
                        }
                        _ => {
                            this.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::EnumVariantKindMismatchInSwitchPattern),
                                loc: Some(pattern.loc),
                                hint: None,
                            });
                        }
                    }
                }

                // .StructVariant { x, y, .. }
                TypedSwitchCasePatternKind::EnumStructVariant {
                    ident,
                    items,
                    has_rest: _,
                } => {
                    let Some(enum_variant) = find_variant(inst_enum_decl, ident) else {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                                enum_name: enum_name.clone(),
                                variant_name: ident.as_string(),
                            }),
                            loc: Some(pattern.loc),
                            hint: None,
                        });
                        return;
                    };

                    *exporting_pattern_count += 1;

                    if !covered_variants.insert(ident.as_string()) {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::DuplicateEnumVariantInSwitchPatterns {
                                variant_name: ident.as_string(),
                            }),
                            loc: Some(pattern.loc),
                            hint: None,
                        });
                    }

                    match enum_variant {
                        TypedEnumVariant::Struct { fields, .. } => {
                            let mut has_error = false;

                            for item in items {
                                let exists = fields.iter().any(|field| field.name == item.name);

                                if !exists {
                                    this.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: Box::new(AnalyzerDiagKind::UnknownFieldInEnumStructPattern {
                                            field_name: item.name.as_string(),
                                        }),
                                        loc: Some(pattern.loc),
                                        hint: None,
                                    });
                                    has_error = true;
                                }
                            }

                            if !has_error {
                                for struct_pattern_field in items {
                                    let field_actual_name = &struct_pattern_field.name;

                                    let Some(field) = fields.iter().find(|field| field.name == *field_actual_name)
                                    else {
                                        this.reporter.report(Diag {
                                            level: DiagLevel::Error,
                                            kind: Box::new(AnalyzerDiagKind::UnknownFieldInEnumStructPattern {
                                                field_name: field_actual_name.as_string(),
                                            }),
                                            loc: Some(struct_pattern_field.name.loc),
                                            hint: None,
                                        });
                                        continue;
                                    };

                                    expose_pattern_bindings(this, &struct_pattern_field.pattern, &field.ty);
                                }
                            }
                        }

                        _ => {
                            this.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::EnumVariantKindMismatchInSwitchPattern),
                                loc: Some(pattern.loc),
                                hint: None,
                            });
                        }
                    }
                }

                _ => {
                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::OnlyVariantPatternIsAllowedInSwitch),
                        loc: Some(pattern.loc),
                        hint: None,
                    });
                }
            }
        }

        for pattern in patterns {
            analyze_pattern(
                self,
                pattern,
                inst_enum_decl,
                &enum_name,
                &mut exporting_pattern_count,
                &mut covered_variants,
            );
        }

        if exporting_pattern_count > 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MultipleExportingPatternsInSwitchCase),
                loc: None,
                hint: None,
            });
        }

        covered_variants
    }
}

// If / While / For / Return / Break
impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_if_stmt(&mut self, if_stmt: &mut TypedIfStmt) -> FlowState {
        let then_state = self.analyze_block_stmt(&mut if_stmt.then_block);

        self.analyze_cond_expr(&mut if_stmt.cond);

        let else_state = {
            if let Some(block_stmt) = &mut if_stmt.else_block {
                self.analyze_block_stmt(&mut *block_stmt)
            } else {
                FlowState::Reachable
            }
        };

        if_stmt.branches.iter_mut().for_each(|branch| {
            self.analyze_if_stmt(branch);
        });

        then_state.merge(else_state)
    }

    pub(crate) fn analyze_while_loop(&mut self, typed_while: &mut TypedWhileStmt) -> FlowState {
        self.analyze_cond_expr(&mut typed_while.cond);

        self.with_control_region(ControlRegion::Loop, |this| {
            this.analyze_block_stmt(&mut typed_while.body);
        });

        FlowState::Reachable
    }

    pub(crate) fn analyze_for_loop(&mut self, typed_for: &mut TypedForStmt) -> FlowState {
        if let Some(initializer) = &mut typed_for.initializer {
            self.analyze_var(initializer);
        }

        if let Some(cond) = &mut typed_for.cond {
            self.analyze_cond_expr(cond);
        }

        if let Some(typed_expr) = &mut typed_for.increment {
            self.analyze_expr(typed_expr, None);
        }

        self.with_control_region(ControlRegion::Loop, |this| {
            this.analyze_block_stmt(&mut typed_for.body);
        });

        FlowState::Reachable
    }

    pub(crate) fn analyze_return(&mut self, ret: &mut TypedReturnStmt) -> FlowState {
        let func_type = self.func_env.current_func.clone().unwrap();

        let mut ret_type = match {
            if let Some(arg) = &mut ret.arg {
                self.analyze_expr(arg, Some(*func_type.ret_type.clone()))
            } else {
                // void as fallback return type
                Some(SemaType::Plain(PlainType::Void))
            }
        } {
            Some(ty) => ty,
            None => return FlowState::Reachable,
        };

        // expand return type
        ret_type = self.expand_sema_type(ret_type.clone(), ret.loc);

        if ret_type.is_void() && ret.arg.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidFunctionReturnsValue),
                loc: Some(ret.loc),
                hint: None,
            });
        } else if let Some(expr) = &mut ret.arg {
            if let Some(expr_type) = self.analyze_expr(expr, Some(ret_type.clone())) {
                if !self.is_assignable_to(expr_type.clone(), ret_type.clone(), expr.loc) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ReturnStatementTypeMismatch {
                            expected: format_sema_type(ret_type.const_inner().clone(), self.formatter),
                            got: format_sema_type(expr_type.const_inner().clone(), self.formatter),
                        }),
                        loc: Some(ret.loc),
                        hint: None,
                    });
                }
            }
        } else if !ret_type.is_void() && ret.arg.is_none() {
            let argument_type = format_sema_type(ret_type.clone(), self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ReturnStatementNeedsAnArgument { argument_type }),
                loc: Some(ret.loc),
                hint: None,
            });
        }

        FlowState::Returns
    }

    pub(crate) fn analyze_break(&mut self, break_stmt: &TypedBreakStmt) -> FlowState {
        let valid = self.control_region_stack.iter().rev().any(|c| c.allows_break());

        if !valid {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidBreakStatement),
                loc: Some(break_stmt.loc),
                hint: None,
            });
            FlowState::Reachable
        } else {
            FlowState::Unreachable
        }
    }

    pub(crate) fn analyze_continue(&mut self, continue_stmt: &TypedContinueStmt) -> FlowState {
        let valid = self.control_region_stack.iter().rev().any(|c| c.allows_continue());

        if !valid {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidContinueStatement),
                loc: Some(continue_stmt.loc),
                hint: None,
            });
            FlowState::Reachable
        } else {
            FlowState::Unreachable
        }
    }
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn with_control_region<F, R>(&mut self, control_region: ControlRegion, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.control_region_stack.push(control_region);
        let result = f(self);
        self.control_region_stack.pop();
        result
    }

    #[inline]
    fn all_flow_states_are_unreachable(&self, flow_states: &[FlowState]) -> bool {
        flow_states.iter().all(|fs| matches!(fs, FlowState::Unreachable))
    }

    #[inline]
    fn all_flow_states_return(&self, flow_states: &[FlowState]) -> bool {
        flow_states.iter().all(|fs| matches!(fs, FlowState::Returns))
    }

    #[inline]
    fn all_flow_states_terminate(&self, flow_states: &[FlowState]) -> bool {
        flow_states
            .iter()
            .all(|fs| matches!(fs, FlowState::Returns | FlowState::Unreachable))
    }
}
