/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::{
    AnalysisContext, AnalyzerConfig, EntryPoints, diagnostics::AnalyzerDiagKind, normalizer::TypeCache,
    typecheck::typecheck::FuncEnv,
};
use cyrusc_ast::{
    AssignKind,
    abi::{ReprAttr, ReprKind},
};
use cyrusc_const_eval::{fold::ConstFolder, resolver::ConstResolver, value::is_comptime_valid};
use cyrusc_diagcentral::{Diag, DiagLevel, reporter::DiagReporter};
use cyrusc_internal::{
    flow_state::{ControlRegion, FlowState},
    symbols::table::{Query, SymbolEntryMut},
    vtable::VTableRegistry,
};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    exprs::{MemoryLocation, TypedAssignExpr, TypedExprKind, TypedExprStmt, TypedTupleAccessExpr},
    format::{SymbolFormatterFn, format_sema_type, format_unnamed_enum_type},
    generics::{
        mapping_ctx_arena::GenericMappingCtxArena,
        monomorph::MonomorphRegistry,
        substitute::{substitute_enum_sig, substitute_func_sig},
    },
    sigs::{FuncSig, typed_func_decl_as_func_sig, typed_func_params_as_func_type_params},
    stmts::*,
    types::{
        PlainType, SemanticType, TypedFuncType, TypedUnnamedEnumType, TypedUnnamedEnumVariant,
        enum_sig_as_unnamed_enum_type,
    },
    *,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    mem,
    rc::Rc,
    sync::{Arc, Mutex},
};

// Analysis Context Public API
impl<'a, M: SymbolEntryMut> AnalysisContext<'a, M> {
    pub fn new(
        config: AnalyzerConfig,
        reporter: Arc<DiagReporter>,
        query: &'a dyn Query,
        symbol_mut: &'a M,
        program_tree: Rc<RefCell<TypedProgramTree>>,
        entry_points: Arc<EntryPoints>,
        monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        vtable_registry: Arc<Mutex<VTableRegistry>>,
    ) -> Self {
        Self {
            config,
            type_cache: TypeCache::default(),
            fenv: FuncEnv::new(),
            reporter,
            control_stack: Vec::new(),
            program_tree,
            query,
            symbol_mut,
            entry_points,
            vtable_registry,
            monomorph_registry,
            mapping_ctx_arena,
        }
    }

    // Traverse TypedAST
    pub fn analyze(&mut self) {
        let mut body = {
            let mut tree_borrowed = self.program_tree.borrow_mut();
            mem::take(&mut tree_borrowed.body)
        };

        for mut typed_stmt in &mut body {
            match &mut typed_stmt {
                TypedStmt::GlobalVar(typed_global_var) => self.analyze_global_var(typed_global_var),
                TypedStmt::FuncDef(typed_func_def) => self.analyze_func_def(typed_func_def),
                TypedStmt::FuncDecl(typed_func_decl) => self.analyze_func_decl(typed_func_decl),
                TypedStmt::Interface(typed_interface) => self.analyze_interface(typed_interface),
                TypedStmt::Struct(struct_stmt) => self.analyze_struct(struct_stmt),
                TypedStmt::Enum(typed_enum) => self.analyze_enum(typed_enum),
                TypedStmt::Typedef(typed_typedef) => self.analyze_typedef(typed_typedef),
                TypedStmt::Union(union_stmt) => self.analyze_union(union_stmt),
                TypedStmt::Variable(_)
                | TypedStmt::ExportTuple(_)
                | TypedStmt::BlockStmt(_)
                | TypedStmt::Defer(_)
                | TypedStmt::If(_)
                | TypedStmt::Return(_)
                | TypedStmt::Break(_)
                | TypedStmt::Continue(_)
                | TypedStmt::For(_)
                | TypedStmt::While(_)
                | TypedStmt::Switch(_)
                | TypedStmt::Label(_)
                | TypedStmt::Goto(_)
                | TypedStmt::Expr(_) => {
                    unreachable!()
                }
                TypedStmt::Builtin(_typed_builtin) => todo!(),
            }
        }

        self.program_tree.borrow_mut().body = body;
    }
}

// Analysis Entry Points
//
// These functions are the primary entry points for type checking different
// expression categories. They handle top-level analysis and dispatch to
// specialized helpers for detailed checking.
impl<'a, M: SymbolEntryMut> AnalysisContext<'a, M> {
    pub(crate) fn analyze_block_stmt(&mut self, block_stmt: &mut TypedBlockStmt) -> FlowState {
        let mut flow_state = FlowState::Reachable;
        let mut terminated = false;

        let stmts = std::mem::take(&mut block_stmt.stmts);
        let mut final_stmts = Vec::with_capacity(block_stmt.stmts.len());

        for mut stmt in stmts {
            let stmt_state = self.analyze_stmt(&mut stmt);

            if terminated {
                if self.config.warnings.enabled {
                    self.reporter.report(Diag {
                        level: DiagLevel::Warning,
                        kind: Box::new(AnalyzerDiagKind::UnreachableCode),
                        loc: Some(stmt.loc()),
                        hint: None,
                    });
                }
                continue;
            }

            match stmt_state {
                FlowState::Reachable => {
                    final_stmts.push(stmt);
                }
                FlowState::Unreachable => {
                    final_stmts.push(stmt);
                    flow_state = FlowState::Unreachable;
                    terminated = true;
                }
                FlowState::Returns => {
                    final_stmts.push(stmt);
                    flow_state = FlowState::Returns;
                    terminated = true;
                }
            }
        }

        block_stmt.stmts = final_stmts;

        for defer in &mut block_stmt.defers {
            self.analyze_stmt(&mut defer.operand);
        }

        flow_state
    }

    pub(crate) fn analyze_stmt(&mut self, typed_stmt: &mut TypedStmt) -> FlowState {
        match typed_stmt {
            TypedStmt::Expr(typed_expr) => {
                self.analyze_expr(typed_expr, typed_expr.sema_type.clone());
                FlowState::Reachable
            }
            TypedStmt::Variable(typed_variable) => {
                self.analyze_variable(typed_variable);
                FlowState::Reachable
            }
            TypedStmt::BlockStmt(typed_block_statement) => self.analyze_block_stmt(typed_block_statement),
            TypedStmt::ExportTuple(typed_export_tuple_values) => {
                self.analyze_export_tuple_values(typed_export_tuple_values);
                FlowState::Reachable
            }
            TypedStmt::If(typed_if) => self.analyze_if_stmt(typed_if, None),
            TypedStmt::Return(typed_return) => self.analyze_return(typed_return),
            TypedStmt::Break(typed_break) => {
                self.analyze_break(typed_break);
                FlowState::Unreachable
            }
            TypedStmt::Continue(typed_continue) => {
                self.analyze_continue(typed_continue);
                FlowState::Unreachable
            }
            TypedStmt::Goto(typed_goto) => {
                self.analyze_goto(typed_goto);
                FlowState::Reachable
            }
            TypedStmt::For(typed_for) => self.analyze_for_loop(typed_for),
            TypedStmt::While(typed_while) => self.analyze_while_loop(typed_while),
            TypedStmt::Switch(typed_switch) => self.analyze_switch(typed_switch),
            // Skipped
            TypedStmt::Label(..) => FlowState::Reachable,
            // Invalid statements
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidStatement),
                    loc: Some(typed_stmt.loc()),
                    hint: None,
                });
                return FlowState::Reachable;
            }
        }
    }

    pub(crate) fn analyze_export_tuple_values(&mut self, export_tuple: &mut TypedExportTupleStmt) -> Option<()> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let mut explicit_sema_ty: Option<SemanticType> = None;

        if let Some(sema_type) = &export_tuple.ty {
            match self.normalize_and_check_sema_ty(sema_type.clone(), export_tuple.loc) {
                Some(sema_type) => {
                    explicit_sema_ty = Some(sema_type);
                }
                None => return None,
            }
        }

        match &mut export_tuple.rhs {
            Some(expr) => expr.clone(),
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DestructureTupleWithNoRhs),
                    loc: Some(export_tuple.loc),
                    hint: None,
                });
                return None;
            }
        };

        let expr_sema_ty = self.analyze_expr(&mut export_tuple.rhs.as_mut().unwrap(), explicit_sema_ty.clone())?;

        let Some(tuple_type) = expr_sema_ty.as_tuple_type() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                loc: Some(export_tuple.loc),
                hint: None,
            });
            return None;
        };

        if explicit_sema_ty.is_none() {
            export_tuple.ty = Some(expr_sema_ty.clone());
        }

        if let Some(target_type) = explicit_sema_ty {
            if !self.is_assignable_to(expr_sema_ty.clone(), target_type.clone(), export_tuple.loc) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                        lhs_type: format_sema_type(target_type, fmt_symbol),
                        rhs_type: format_sema_type(expr_sema_ty, fmt_symbol),
                    }),
                    loc: Some(export_tuple.loc),
                    hint: None,
                });
                return None;
            }
        }

        let tuple_patterns = export_tuple.pattern.into_tuple();
        for (i, (pattern, sema_type)) in tuple_patterns.iter().zip(tuple_type.elements.iter()).enumerate() {
            self.analyze_tuple_pattern(
                pattern,
                sema_type,
                &export_tuple.rhs.as_mut().unwrap(),
                export_tuple.is_const,
                export_tuple.loc,
                vec![i],
            );
        }

        self.normalize_and_check_sema_ty(export_tuple.ty.clone()?, export_tuple.loc)?;
        Some(())
    }

    /// Assigns the resolved type and RHS expression to a variable introduced
    /// by a tuple destructuring pattern.
    ///
    /// Applies `const` qualification to the type when required and updates
    /// the corresponding variable symbol entry.
    fn analyze_tuple_ident_pattern(
        &mut self,
        symbol_id: SymbolID,
        sema_type: &SemanticType,
        rhs: &TypedExprStmt,
        is_const: bool,
    ) {
        let mut ty = sema_type.clone();

        if is_const && !matches!(ty, SemanticType::Const(..)) {
            ty = ty.as_const();
        }

        self.symbol_mut.with_var_mut(symbol_id, |var_mut| {
            var_mut.variable.ty = Some(ty);
            var_mut.variable.rhs = Some(rhs.clone());
        });
    }

    /// Analyzes a tuple destructuring pattern.
    ///
    /// Recursively walks tuple patterns, constructs the appropriate tuple
    /// access expressions for each binding, and assigns the extracted values
    /// to the corresponding variables. Reports errors for tuple size mismatches
    /// or when tuple access is attempted on a non‑tuple type.
    fn analyze_tuple_pattern(
        &mut self,
        pattern: &TypedExportPattern,
        sema_type: &SemanticType,
        expr: &TypedExprStmt,
        is_const: bool,
        loc: Loc,
        access_path: Vec<usize>,
    ) {
        match pattern {
            TypedExportPattern::Ident(symbol_id) => {
                let mut rhs = expr.clone();
                let rhs_ty = rhs.sema_type.clone().unwrap();
                let tuple_type = rhs_ty.as_tuple_type().unwrap();

                for &i in &access_path {
                    rhs = TypedExprStmt {
                        kind: TypedExprKind::TupleAccess(TypedTupleAccessExpr {
                            operand: Box::new(rhs),
                            index: i,
                            loc: loc,
                        }),
                        sema_type: Some(tuple_type.elements.get(i).unwrap().clone()),
                        mloc: MemoryLocation::LValue,
                        loc: loc,
                    };
                }

                self.analyze_tuple_ident_pattern(*symbol_id, sema_type, &rhs, is_const);
            }
            TypedExportPattern::Tuple(patterns) => {
                if let Some(tuple_type) = sema_type.as_tuple_type() {
                    if patterns.len() != tuple_type.elements.len() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::TupleExportedValuesAndTupleElementsCountMismatch),
                            loc: Some(loc),
                            hint: None,
                        });
                        return;
                    }
                    for (i, (sub_pattern, sub_ty)) in patterns.iter().zip(&tuple_type.elements).enumerate() {
                        let mut new_access_path = access_path.clone();
                        new_access_path.push(i);

                        self.analyze_tuple_pattern(sub_pattern, sub_ty, expr, is_const, loc, new_access_path);
                    }
                } else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                        loc: Some(loc),
                        hint: None,
                    });
                }
            }
        }
    }

    fn analyze_switch_on_enum(
        &mut self,
        switch_stmt: &mut TypedSwitchStmt,
        unnamed_enum_type: &mut TypedUnnamedEnumType,
        enum_name: &String,
    ) -> FlowState {
        let mut branch_states = Vec::new();
        let mut used_enum_variants: Vec<String> = Vec::new();

        'cases: for i in 0..switch_stmt.cases.len() {
            let case = &mut switch_stmt.cases[i];

            'patterns: for pattern in &case.patterns {
                let ident = match &pattern {
                    TypedSwitchCasePattern::Ident(ident) => ident,
                    TypedSwitchCasePattern::EnumVariant(ident, valued_fields, _) => {
                        let mut field_names: Vec<String> = Vec::new();

                        for valued_field in valued_fields {
                            if field_names.contains(&valued_field.name) {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::DuplicateEnumVariantName {
                                        enum_name: enum_name.clone(),
                                        variant_name: valued_field.name.clone(),
                                    }),
                                    loc: Some(case.loc),
                                    hint: None,
                                });
                                continue 'patterns;
                            }

                            field_names.push(valued_field.name.clone());
                        }

                        ident
                    }
                    TypedSwitchCasePattern::Expr(..) | TypedSwitchCasePattern::Range(..) => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ExpressionPatternInAEnumSwitch),
                            loc: Some(case.loc),
                            hint: None,
                        });
                        continue 'patterns;
                    }
                };

                if used_enum_variants.contains(&ident.value) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::DuplicateEnumVariantInSwitchPatterns {
                            variant_name: ident.as_string(),
                        }),
                        loc: Some(case.loc),
                        hint: Some("Remove the duplicate to avoid redundancy.".to_string()),
                    });
                }

                let variant_opt = unnamed_enum_type
                    .variants
                    .iter_mut()
                    .find(|variant| variant.ident().value == ident.value);

                if variant_opt.is_none() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                            enum_name: enum_name.clone(),
                            variant_name: ident.as_string(),
                        }),
                        loc: Some(switch_stmt.loc),
                        hint: None,
                    });
                    continue 'patterns;
                }

                if let TypedSwitchCasePattern::EnumVariant(_, valued_fields, _) = &pattern {
                    if let Some(variant) = variant_opt {
                        match variant {
                            TypedUnnamedEnumVariant::Variant(ident, enum_valued_fields) => {
                                let actual_enum_fields_len = enum_valued_fields.len();

                                if valued_fields.len() != actual_enum_fields_len {
                                    self.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: Box::new(AnalyzerDiagKind::EnumVariantArgCountMismatch {
                                            variant_name: variant.ident().as_string(),
                                            expected: actual_enum_fields_len as u32,
                                            provided: valued_fields.len() as u32,
                                        }),
                                        loc: Some(case.loc),
                                        hint: None,
                                    });
                                    continue 'cases;
                                }

                                // normalize and then update valued_field type in local scope

                                for (enum_valued_field_idx, enum_valued_field) in
                                    enum_valued_fields.iter_mut().enumerate()
                                {
                                    enum_valued_field.ty =
                                        match self.normalize_sema_type(enum_valued_field.ty.clone(), ident.loc) {
                                            Some(sema_type) => sema_type,
                                            None => continue 'patterns,
                                        };

                                    let valued_field = &valued_fields[enum_valued_field_idx];

                                    self.symbol_mut.with_var_mut(valued_field.symbol_id, |resolved_var| {
                                        resolved_var.variable.ty = Some(enum_valued_field.ty.clone());
                                    });
                                }
                            }
                            TypedUnnamedEnumVariant::Valued(ident, valued) => {
                                if valued_fields.len() > 1 {
                                    self.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: Box::new(AnalyzerDiagKind::ValuedEnumVariantCanOnlyExportOneField {
                                            variant_name: ident.as_string(),
                                        }),
                                        loc: Some(case.loc),
                                        hint: None,
                                    });
                                    return FlowState::Reachable;
                                }

                                let valued_field = valued_fields.first().unwrap();

                                valued.sema_type = match self.analyze_expr(valued, None) {
                                    Some(sema_type) => Some(sema_type),
                                    None => continue 'patterns,
                                };

                                self.symbol_mut.with_var_mut(valued_field.symbol_id, |resolved_var| {
                                    resolved_var.variable.ty = Some(valued.sema_type.clone().unwrap());
                                });
                            }
                            TypedUnnamedEnumVariant::Ident(ident) => {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::VariantDoesNotExportAnyField {
                                        variant_name: ident.as_string(),
                                    }),
                                    loc: Some(case.loc),
                                    hint: None,
                                });
                                return FlowState::Reachable;
                            }
                        }
                    }
                }

                used_enum_variants.push(ident.as_string());
            }

            let body_flow_state = self.analyze_block_stmt(&mut case.body);
            branch_states.push(body_flow_state);
        }

        if let Some(default_case) = &mut switch_stmt.default_case {
            let body_flow_state = self.analyze_block_stmt(default_case);
            branch_states.push(body_flow_state);
        } else {
            branch_states.push(FlowState::Reachable);
        }

        self.control_stack.pop();

        // final merge
        let flow_state = if branch_states.iter().all(|s| matches!(s, FlowState::Returns)) {
            FlowState::Returns
        } else if used_enum_variants.len() == unnamed_enum_type.variants.len() {
            FlowState::Returns
        } else {
            FlowState::Reachable
        };

        flow_state
    }

    fn analyze_switch(&mut self, switch_stmt: &mut TypedSwitchStmt) -> FlowState {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        self.control_stack.push(ControlRegion::Switch);

        if switch_stmt.cases.is_empty() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::EmptyCaseSwitchStatement),
                loc: Some(switch_stmt.loc),
                hint: None,
            });
            return FlowState::Reachable;
        }

        let operand_type = match self.analyze_expr(&mut switch_stmt.operand, None) {
            Some(sema_type) => sema_type.const_inner().clone(),
            None => return FlowState::Reachable,
        };

        match if let Some(enum_id) = operand_type.const_inner().as_enum_symbol_id() {
            Some((enum_id, None))
        } else if let Some(generic_type) = operand_type.const_inner().as_generic_type() {
            let Some(symbol_entry) = self.query.lookup_symbol_entry(generic_type.base) else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::SwitchOperandIsNotEnum {
                        expr_type: fmt_symbol(generic_type.base),
                    }),
                    loc: Some(switch_stmt.loc),
                    hint: None,
                });
                return FlowState::Reachable;
            };

            let Some(resolved_enum) = symbol_entry.as_enum() else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::SwitchOperandIsNotEnum {
                        expr_type: fmt_symbol(generic_type.base),
                    }),
                    loc: Some(switch_stmt.loc),
                    hint: None,
                });
                return FlowState::Reachable;
            };

            Some((resolved_enum.symbol_id, Some(generic_type)))
        } else if let Some(mut unnamed_enum_type) = operand_type.const_inner().as_unnamed_enum() {
            let enum_name = format_unnamed_enum_type(&unnamed_enum_type, fmt_symbol);
            return self.analyze_switch_on_enum(switch_stmt, &mut unnamed_enum_type, &enum_name);
        } else {
            None
        } {
            Some((enum_id, generic_type_opt)) => {
                let mut enum_sig = self.query.get_enum(enum_id).unwrap().enum_sig.clone();

                if let Some(generic_type) = generic_type_opt {
                    enum_sig = substitute_enum_sig(
                        self.mapping_ctx_arena.clone(),
                        &enum_sig,
                        generic_type.mapping_ctx.clone(),
                    )
                    .unwrap()
                }

                let mut unnamed_enum_type = enum_sig_as_unnamed_enum_type(&enum_sig, switch_stmt.loc);
                return self.analyze_switch_on_enum(switch_stmt, &mut unnamed_enum_type, &enum_sig.name);
            }
            None => {}
        }

        let mut branch_states = Vec::new();

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

        for case in &mut switch_stmt.cases {
            let mut local_range_table: Vec<(usize, usize)> = Vec::new();

            for pattern in &mut case.patterns {
                match pattern {
                    TypedSwitchCasePattern::Expr(typed_expr) => {
                        let Some(pattern_type) = self.analyze_expr(typed_expr, Some(operand_type.clone())) else {
                            continue;
                        };

                        if !self.is_assignable_to(pattern_type.clone(), operand_type.clone(), switch_stmt.loc) {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::TypeMismatchInCasePattern {
                                    operand_type: format_sema_type(operand_type.clone(), fmt_symbol),
                                    pattern_type: format_sema_type(pattern_type, fmt_symbol),
                                }),
                                loc: Some(case.loc),
                                hint: None,
                            });
                            continue;
                        }
                    }
                    TypedSwitchCasePattern::Ident(..) | TypedSwitchCasePattern::EnumVariant(..) => {
                        let expr_type = format_sema_type(operand_type.clone(), fmt_symbol);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::SwitchOperandIsNotEnum { expr_type }),
                            loc: Some(switch_stmt.loc),
                            hint: None,
                        });
                        continue;
                    }
                    TypedSwitchCasePattern::Range(range) => {
                        self.analyze_expr(&mut range.lower, Some(operand_type.clone()));
                        self.analyze_expr(&mut range.upper, Some(operand_type.clone()));

                        if !is_comptime_valid(&range.lower.kind) {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::ExprNotComptimeValid),
                                loc: Some(range.lower.loc),
                                hint: None,
                            });
                            continue;
                        }

                        if !is_comptime_valid(&range.upper.kind) {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::ExprNotComptimeValid),
                                loc: Some(range.upper.loc),
                                hint: None,
                            });
                            continue;
                        }

                        let mut folder = ConstFolder::new(self);
                        let lower_int = folder.expr_as_const_int(&range.lower).unwrap();
                        let upper_int = folder.expr_as_const_int(&range.upper).unwrap();

                        if lower_int >= upper_int {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::InvalidRange),
                                loc: Some(case.loc),
                                hint: None,
                            });
                            continue;
                        }

                        local_range_table.push((lower_int.try_into().unwrap(), upper_int.try_into().unwrap()));
                    }
                }
            }

            if is_valid_range(&range_table, &local_range_table) {
                range_table.extend(local_range_table);
            } else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::OverlappingSwitchCaseRange),
                    loc: Some(case.loc),
                    hint: None,
                });
                continue;
            }

            let body_flow_state = self.analyze_block_stmt(&mut case.body);
            branch_states.push(body_flow_state);
        }

        if let Some(default_case) = &mut switch_stmt.default_case {
            let body_flow_state = self.analyze_block_stmt(default_case);
            branch_states.push(body_flow_state);
        } else {
            branch_states.push(FlowState::Reachable);
        }

        let mut branch_states = Vec::new();

        // normalize each case body
        for case in &mut switch_stmt.cases {
            let body_flow_state = self.analyze_block_stmt(&mut case.body);
            branch_states.push(body_flow_state);
        }

        // default case
        if let Some(default_case) = &mut switch_stmt.default_case {
            let body_flow_state = self.analyze_block_stmt(default_case);
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

    fn analyze_if_stmt(&mut self, if_stmt: &mut TypedIfStmt, expected_type: Option<SemanticType>) -> FlowState {
        let then_state = self.analyze_block_stmt(&mut if_stmt.then_block);

        if let Some(sema_type) = self.analyze_expr(&mut if_stmt.cond, expected_type.clone()) {
            self.report_if_not_cond_expr(sema_type, if_stmt.loc);
        }

        let else_state = {
            if let Some(block_stmt) = &mut if_stmt.else_block {
                self.analyze_block_stmt(&mut *block_stmt)
            } else {
                FlowState::Reachable
            }
        };

        if_stmt.branches.iter_mut().for_each(|branch| {
            self.analyze_if_stmt(branch, expected_type.clone());
        });

        then_state.merge(else_state)
    }

    fn analyze_while_loop(&mut self, typed_while: &mut TypedWhileStmt) -> FlowState {
        if let Some(sema_type) =
            self.analyze_expr(&mut typed_while.cond, Some(SemanticType::PlainType(PlainType::Bool)))
        {
            self.report_if_not_cond_expr(sema_type, typed_while.loc);
        }

        self.control_stack.push(ControlRegion::Loop);
        self.analyze_block_stmt(&mut typed_while.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    fn analyze_for_loop(&mut self, typed_for: &mut TypedForStmt) -> FlowState {
        if let Some(initializer) = &mut typed_for.initializer {
            self.analyze_variable(initializer);
        }

        if let Some(typed_expr) = &mut typed_for.cond {
            if let Some(sema_type) = self.analyze_expr(typed_expr, Some(SemanticType::PlainType(PlainType::Bool))) {
                self.report_if_not_cond_expr(sema_type, typed_for.loc);
            }
        }

        if let Some(typed_expr) = &mut typed_for.increment {
            self.analyze_expr(typed_expr, None);
        }

        self.control_stack.push(ControlRegion::Loop);
        self.analyze_block_stmt(&mut typed_for.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    fn analyze_return(&mut self, ret: &mut TypedReturnStmt) -> FlowState {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let func_type = self.fenv.current_func_type.clone().unwrap();
        let ret_type = self.normalize_sema_type(*func_type.ret_type, ret.loc).unwrap();

        if ret_type.is_void() && ret.arg.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidFunctionReturnsValue),
                loc: Some(ret.loc),
                hint: None,
            });
        } else if let Some(typed_expr) = &mut ret.arg {
            if let Some(sema_type) = self.analyze_expr(typed_expr, Some(ret_type.clone())) {
                if !self.is_assignable_to(sema_type.clone(), ret_type.clone(), ret.loc) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ReturnStatementTypeMismatch {
                            expected: format_sema_type(ret_type.const_inner().clone(), fmt_symbol),
                            got: format_sema_type(sema_type.const_inner().clone(), fmt_symbol),
                        }),
                        loc: Some(ret.loc),
                        hint: None,
                    });
                }
            }
        } else if !ret_type.is_void() && ret.arg.is_none() {
            let argument_type = format_sema_type(ret_type.clone(), fmt_symbol);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ReturnStatementNeedsAnArgument { argument_type }),
                loc: Some(ret.loc),
                hint: None,
            });
        }

        FlowState::Returns
    }

    fn analyze_break(&mut self, break_stmt: &TypedBreakStmt) -> FlowState {
        let valid = self.control_stack.iter().rev().any(|c| c.allows_break());

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

    fn analyze_continue(&mut self, continue_stmt: &TypedContinueStmt) -> FlowState {
        let valid = self.control_stack.iter().rev().any(|c| c.allows_continue());

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

    // FIXME
    fn analyze_goto(&mut self, goto: &mut TypedGotoStmt) {
        // if let Some(label_id) = scope_ref.resolve_label(&goto.name) {
        //     goto.label_id = Some(label_id);
        // } else {
        //     self.reporter.report(Diag {
        //         level: DiagLevel::Error,
        //         kind: Box::new(AnalyzerDiagKind::UndefinedGotoLabel {
        //             label_name: goto.name.clone(),
        //         }),
        //         loc: Some(goto.loc),
        //         hint: None,
        //     });
        // }
    }

    fn analyze_struct(&mut self, struct_stmt: &mut TypedStructStmt) {
        self.validate_struct_repr_attr(
            &struct_stmt.modifiers.repr_attr,
            struct_stmt.fields.len(),
            struct_stmt.loc,
        );

        self.validate_align(&struct_stmt.align, struct_stmt.loc);

        if let Some(generic_params) = &struct_stmt.generic_params {
            self.analyze_generic_params(generic_params);
        }

        self.fenv.current_self_type = Some(SemanticType::ResolvedSymbol(types::ResolvedSymbol::Struct(
            struct_stmt.symbol_id,
        )));

        self.nameconv_check_struct_name(struct_stmt.name.clone(), struct_stmt.loc);

        self.analyze_struct_fields(struct_stmt);

        if struct_stmt.generic_params.is_none() {
            self.analyze_non_generic_methods(&struct_stmt.methods);
        }

        self.analyze_method_generic_params(&struct_stmt.name, &struct_stmt.methods, &struct_stmt.generic_params);

        self.analyze_object_implements_interface_list(
            struct_stmt.name.clone(),
            &struct_stmt.impls,
            &struct_stmt.methods,
        );

        self.symbol_mut
            .with_struct_mut(struct_stmt.symbol_id, |resolved_struct| {
                resolved_struct.struct_sig.fields = struct_stmt.fields.clone();
            });
    }

    fn analyze_union(&mut self, union_stmt: &mut TypedUnionStmt) {
        self.validate_union_repr_attr(&union_stmt.modifiers.repr_attr, union_stmt.fields.len(), union_stmt.loc);

        self.validate_align(&union_stmt.align, union_stmt.loc);

        if let Some(generic_params) = &union_stmt.generic_params {
            self.analyze_generic_params(generic_params);
        }

        self.fenv.current_self_type = Some(SemanticType::ResolvedSymbol(types::ResolvedSymbol::Union(
            union_stmt.symbol_id,
        )));

        self.nameconv_check_union_name(union_stmt.name.clone(), union_stmt.loc);

        self.analyze_union_fields(union_stmt);

        if union_stmt.generic_params.is_none() {
            self.analyze_non_generic_methods(&union_stmt.methods);
        }

        self.analyze_method_generic_params(&union_stmt.name, &union_stmt.methods, &union_stmt.generic_params);

        self.analyze_object_implements_interface_list(union_stmt.name.clone(), &union_stmt.impls, &union_stmt.methods);

        self.symbol_mut.with_union_mut(union_stmt.symbol_id, |resolved_union| {
            resolved_union.union_sig.fields = union_stmt.fields.clone();
        });
    }

    fn analyze_enum(&mut self, enum_stmt: &mut TypedEnumStmt) {
        if let Some(generic_params) = &enum_stmt.generic_params {
            self.analyze_generic_params(generic_params);
        }

        self.fenv.current_self_type = Some(SemanticType::ResolvedSymbol(types::ResolvedSymbol::Enum(
            enum_stmt.symbol_id,
        )));

        self.nameconv_check_enum_name(enum_stmt.name.clone(), enum_stmt.loc);

        self.analyze_enum_variants(enum_stmt);

        if enum_stmt.generic_params.is_none() {
            self.analyze_non_generic_methods(&enum_stmt.methods);
        }

        self.analyze_method_generic_params(&enum_stmt.name, &enum_stmt.methods, &enum_stmt.generic_params);

        self.analyze_object_implements_interface_list(enum_stmt.name.clone(), &enum_stmt.impls, &enum_stmt.methods);

        self.validate_enum_repr_attr(&enum_stmt.modifiers.repr_attr, enum_stmt.align.is_some(), enum_stmt.loc);

        self.validate_align(&enum_stmt.align, enum_stmt.loc);

        self.validate_enum_tag_type(&enum_stmt.tag_type, enum_stmt.loc);

        self.symbol_mut.with_enum_mut(enum_stmt.symbol_id, |resolved_enum| {
            resolved_enum.enum_sig.variants = enum_stmt.variants.clone();
        });
    }

    fn analyze_func_def(&mut self, func_def: &mut TypedFuncDefStmt) {
        self.analyze_entry_func(func_def);

        let is_public = func_def.modifiers.vis.is_public();
        let is_generic_func = func_def.is_generic();

        if let Some(generic_params) = &func_def.generic_params {
            self.analyze_generic_params(generic_params);
        }

        self.fenv.current_func_type = Some(TypedFuncType {
            symbol_id: Some(func_def.symbol_id),
            params: typed_func_params_as_func_type_params(&func_def.params),
            ret_type: Box::new(func_def.ret_type.clone()),
            is_public,
            loc: func_def.loc,
        });

        self.normalize_func_params(&mut func_def.params, func_def.loc);

        let Some(ret_type) = self.normalize_sema_type(func_def.ret_type.clone(), func_def.loc) else {
            return;
        };

        func_def.ret_type = ret_type;

        if !is_generic_func {
            self.analyze_func_body(&mut func_def.body, &func_def.ret_type);
        }

        self.symbol_mut.with_func_mut(func_def.symbol_id, |resolved_func| {
            resolved_func.func_sig.params = func_def.params.clone();
            resolved_func.func_sig.ret_type = func_def.ret_type.clone();
        });
    }

    fn analyze_func_decl(&mut self, func_decl: &mut TypedFuncDeclStmt) {
        if func_decl.generic_params.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::GenericFunctionDeclaration),
                loc: Some(func_decl.loc),
                hint: None,
            });
        }

        self.report_if_duplicate_param_names(
            &func_decl.params.list,
            func_decl.params.variadic.as_ref(),
            func_decl.loc,
        );

        func_decl.ret_type = match self.normalize_sema_type(func_decl.ret_type.clone(), func_decl.loc) {
            Some(sema_type) => sema_type,
            None => return,
        };

        self.normalize_func_params(&mut func_decl.params, func_decl.loc);

        self.symbol_mut.with_func_mut(func_decl.symbol_id, |resolved_func| {
            resolved_func.func_sig.params = func_decl.params.clone();
            resolved_func.func_sig.ret_type = func_decl.ret_type.clone();
        });
    }

    fn analyze_interface(&mut self, interface: &TypedInterfaceStmt) {
        let interface_name = &interface.name;

        if let Some(generic_params) = &interface.generic_params {
            self.analyze_generic_params(generic_params);
        }

        self.nameconv_check_interface_name(interface_name.clone(), interface.loc);

        let mut methods: Vec<String> = Vec::new();

        for method in &interface.methods {
            if !method.params.is_instance_method() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceMethodsMustHaveSelfModifier),
                    loc: Some(method.loc),
                    hint: None,
                });
            }

            if methods.contains(&method.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceDuplicateMethod {
                        interface_name: interface_name.clone(),
                        method_name: method.name.clone(),
                    }),
                    loc: Some(method.loc),
                    hint: None,
                });
                continue;
            }

            methods.push(method.name.clone());
        }
    }

    fn analyze_typedef(&mut self, typed_typedef: &mut TypedTypedefStmt) {
        typed_typedef.ty = match self.normalize_sema_type(typed_typedef.ty.clone(), typed_typedef.loc) {
            Some(sema_type) => sema_type,
            None => return,
        };

        self.symbol_mut
            .with_typedef_mut(typed_typedef.symbol_id, |resolved_typedef| {
                resolved_typedef.typedef_sig.ty = typed_typedef.ty.clone();
            });
    }

    fn analyze_global_var(&mut self, global_var: &mut TypedGlobalVarStmt) {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        if let Some(mut expr) = global_var.expr.clone() {
            match self.analyze_expr(&mut expr, global_var.ty.clone()) {
                Some(sema_type) => Some(sema_type),
                None => return,
            };

            if !is_comptime_valid(&expr.kind) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::GlobalVariableExprNotComptimeValid),
                    loc: Some(global_var.loc),
                    hint: None,
                });
                return;
            }

            global_var.expr = Some(expr);
        }

        global_var.ty = match &global_var.ty {
            Some(sema_type) => self.normalize_and_check_sema_ty(sema_type.clone(), global_var.loc),
            None => match global_var.expr.as_ref().and_then(|expr| expr.sema_type.clone()) {
                Some(sema_type) => Some(sema_type),
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::GlobalVarRequiresTypeAnnotation),
                        loc: Some(global_var.loc),
                        hint: None,
                    });
                    return;
                }
            },
        };

        if let Some(sema_type) = &global_var.ty {
            self.validate_variable_type(sema_type, global_var.expr.is_some(), global_var.loc);
        }

        if let Some(expr) = &global_var.expr {
            if !is_comptime_valid(&expr.kind) && !matches!(global_var.ty, Some(SemanticType::Const(..))) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::GlobalVariableExprNotComptimeValid),
                    loc: Some(global_var.loc),
                    hint: None,
                });
                return;
            }
        }

        if self.is_const_qualified_type_assigned_to_non_const_variable(
            &global_var.ty.as_ref().unwrap(),
            global_var.is_const,
        ) {
            self.report_const_qualified_type_assigned_to_non_const_variable(global_var.loc);
        }

        if let Some(expr) = &global_var.expr {
            if let Some(target_type) = &global_var.ty {
                let expr_type = expr.sema_type.clone().unwrap();

                if *expr_type.const_inner() != *target_type.const_inner() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                            lhs_type: format_sema_type(target_type.clone(), fmt_symbol),
                            rhs_type: format_sema_type(expr_type.clone(), fmt_symbol),
                        }),
                        loc: Some(global_var.loc),
                        hint: Some("Global variable initializers must exactly match the declared type.".into()),
                    });
                }
            }
        }

        self.symbol_mut
            .with_global_var_mut(global_var.symbol_id, |resolved_var| {
                resolved_var.global_var_sig.rhs = global_var.expr.clone();
                resolved_var.global_var_sig.ty = global_var.ty.clone();
            });
    }

    fn analyze_variable(&mut self, var: &mut TypedVarStmt) {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        if let Some(ty) = &var.ty {
            var.ty = self.normalize_sema_type(ty.clone(), var.loc);
        }

        if let Some(rhs) = &mut var.rhs {
            let Some(inferred_type) = self.analyze_expr(rhs, var.ty.clone()) else {
                return;
            };

            if var.ty.is_none() {
                var.ty = Some(inferred_type);
            }
        }

        if self.is_const_qualified_type_assigned_to_non_const_variable(&var.ty.as_ref().unwrap(), var.is_const) {
            self.report_const_qualified_type_assigned_to_non_const_variable(var.loc);
        }

        if let Some(sema_type) = &var.ty {
            self.validate_variable_type(sema_type, var.rhs.is_some(), var.loc);
        }

        if let Some(expr) = &mut var.rhs {
            if let Some(target_type) = &var.ty {
                if !self.is_assignable_to(expr.sema_type.clone().unwrap(), target_type.clone(), var.loc) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                            lhs_type: format_sema_type(target_type.clone(), fmt_symbol),
                            rhs_type: format_sema_type(expr.sema_type.clone().unwrap(), fmt_symbol),
                        }),
                        loc: Some(var.loc),
                        hint: None,
                    });
                }
            }
        }

        self.symbol_mut.with_var_mut(var.symbol_id, |resolved_var| {
            resolved_var.variable.ty = var.ty.clone();
        });
    }

    pub(crate) fn analyze_assign(&mut self, assign: &mut TypedAssignExpr) {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let lhs_type = match self.analyze_expr(&mut assign.lhs, None) {
            Some(sema_type) => sema_type,
            None => return,
        };

        let rhs_type = match self.analyze_expr(&mut assign.rhs, Some(lhs_type.clone())) {
            Some(sema_type) => sema_type,
            None => return,
        };

        let is_lhs_const = self.is_const_qualified_lvalue(&assign.lhs);

        if is_lhs_const {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotAssignToConstLValue),
                loc: Some(assign.loc),
                hint: None,
            });
        }

        assert!(assign.kind == AssignKind::Default);

        if !self.is_assignable_to(rhs_type.clone(), lhs_type.clone(), assign.loc) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                    lhs_type: format_sema_type(lhs_type, fmt_symbol),
                    rhs_type: format_sema_type(rhs_type, fmt_symbol),
                }),
                loc: Some(assign.loc),
                hint: None,
            });
        }
    }
}

// Helper Functions
impl<'a, M: SymbolEntryMut> AnalysisContext<'a, M> {
    /// Returns `true` when a const‑qualified type is assigned to a mutable variable.
    ///
    /// Example:
    ///     var x: const int = 10;
    fn is_const_qualified_type_assigned_to_non_const_variable(
        &mut self,
        ty: &SemanticType,
        is_const_qualified: bool,
    ) -> bool {
        is_const_qualified || !ty.is_const()
    }

    fn is_const_qualified_lvalue(&self, expr: &TypedExprStmt) -> bool {
        expr.kind
            .as_symbol_id()
            .and_then(|symbol_id| {
                self.query
                    .lookup_symbol_entry(symbol_id)
                    .map(|symbol_entry| symbol_entry.is_const_qualified())
            })
            .unwrap_or(false)
    }

    /// Validates that an object correctly implements its declared interfaces.
    ///
    /// Ensures the referenced symbols are valid interfaces, checks visibility
    /// rules, resolves generic interface instantiations, and verifies that all
    /// required interface methods are implemented with matching signatures.
    fn analyze_object_implements_interface_list(
        &mut self,
        object_name: String,
        impls: &Vec<TypedImplementInterface>,
        method_ids: &HashMap<String, SymbolID>,
    ) {
        for implement_interface in impls {
            let symbol_entry = self.query.lookup_symbol_entry(implement_interface.symbol_id).unwrap();
            let name = symbol_entry.decl_name();

            let resolved_interface = match symbol_entry.as_interface() {
                Some(resolved_interface) => resolved_interface,
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::SymbolIsNotInterface { symbol_name: name }),
                        loc: Some(implement_interface.loc),
                        hint: None,
                    });
                    continue;
                }
            };

            let interface_method_decls = &resolved_interface.interface_sig.methods;
            let mut interface_method_sigs: Vec<FuncSig> = Vec::new();

            if let Some(generic_params) = &resolved_interface.interface_sig.generic_params {
                let Some(type_args) = &implement_interface.type_args else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::MissingTypeArgs {
                            type_name: name.clone(),
                        }),
                        loc: Some(implement_interface.loc),
                        hint: None,
                    });
                    continue;
                };

                let generic_type_opt = match self.init_generic_type_with_symbol_id(
                    implement_interface.symbol_id,
                    &mut Some(type_args.clone()),
                    None,
                    Some(generic_params),
                    implement_interface.loc,
                ) {
                    Ok(result) => match result {
                        Some((_, generic_type)) => generic_type,
                        None => unreachable!(),
                    },
                    Err(diag) => {
                        self.reporter.report(diag);
                        continue;
                    }
                };

                if let Some(generic_type) = generic_type_opt {
                    for func_decl in interface_method_decls {
                        let mut func_sig = typed_func_decl_as_func_sig(func_decl);
                        func_sig = substitute_func_sig(
                            self.mapping_ctx_arena.clone(),
                            &func_sig,
                            generic_type.mapping_ctx.clone(),
                        )
                        .unwrap();

                        interface_method_sigs.push(func_sig);
                    }
                }
            }

            for interface_method_sig in interface_method_sigs.clone() {
                if !method_ids.contains_key(&interface_method_sig.name) {
                    // method missing
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::MissingInterfaceMethodImpl {
                            object_name: object_name.clone(),
                            method_name: interface_method_sig.name.clone(),
                            interface_name: name.clone(),
                        }),
                        loc: Some(implement_interface.loc),
                        hint: None,
                    });
                    continue;
                }

                let method_id = method_ids.get(&interface_method_sig.name).unwrap();
                let resolved_method = self.query.get_method(*method_id).unwrap();

                // check method signature mismatch
                if resolved_method.func_sig != interface_method_sig {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InterfaceMethodTypeMismatch {
                            object_name: object_name.clone(),
                            interface_name: name.clone(),
                            method_name: interface_method_sig.name.clone(),
                        }),
                        loc: Some(resolved_method.func_sig.loc),
                        hint: None,
                    });
                }
            }
        }
    }

    /// Checks method-level generic parameters against the object's generic parameters.
    ///
    /// Ensures that no method generic parameter reuses a name already defined
    /// at the object level. If a conflict is found, an error diagnostic is emitted
    /// because the method generic would shadow the object's generic parameter.
    fn analyze_method_generic_params(
        &mut self,
        object_name: &String,
        methods: &HashMap<String, SymbolID>,
        generic_params_opt: &Option<TypedGenericParamsList>,
    ) {
        let Some(generic_params) = generic_params_opt else {
            return;
        };

        for method_id in methods.values().cloned() {
            let symbol_entry = self.query.lookup_symbol_entry(method_id).unwrap();

            if let Some(method_generic_params) = symbol_entry.method_generic_params() {
                for method_generic_param in &method_generic_params.list {
                    if generic_params.lookup_named(&method_generic_param.name.value).is_some() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ShadowsObjectGenericParam {
                                param_name: method_generic_param.name.as_string(),
                                object_name: object_name.clone(),
                            }),
                            loc: Some(method_generic_param.name.loc),
                            hint: Some("Consider to rename the generic param to a different name.".to_string()),
                        });
                    }
                }
            }
        }
    }

    /// Validates a list of generic parameters for duplicate names.
    fn analyze_generic_params(&mut self, generic_params: &TypedGenericParamsList) {
        let mut collected_names: Vec<String> = Vec::new();

        for generic_param in &generic_params.list {
            if collected_names.contains(&generic_param.name.value) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateGenericParam {
                        param_name: generic_param.name.as_string(),
                    }),
                    loc: Some(generic_param.name.loc),
                    hint: Some("Consider to rename the generic param to a different name.".to_string()),
                });
            }

            collected_names.push(generic_param.name.as_string());
        }
    }

    fn analyze_enum_variants(&mut self, typed_enum: &mut TypedEnumStmt) {
        let is_repr_c = typed_enum.is_repr_c();
        let mut variant_names: Vec<String> = Vec::new();

        for variant in &mut typed_enum.variants {
            let variant_ident = match variant {
                TypedEnumVariant::Ident(ident) => ident,
                TypedEnumVariant::Valued(ident, typed_expr) => {
                    typed_expr.sema_type = match self.analyze_expr(typed_expr, None) {
                        Some(sema_type) => Some(sema_type),
                        None => continue,
                    };

                    if is_repr_c && !typed_expr.sema_type.as_ref().unwrap().is_integer() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ReprCEnumWithNonIntegerVariant),
                            loc: Some(ident.loc),
                            hint: None,
                        });
                        continue;
                    }

                    ident
                }
                TypedEnumVariant::Variant(ident, typed_enum_valued_fields) => {
                    if is_repr_c {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ReprCEnumWithNonIntegerVariant),
                            loc: Some(ident.loc),
                            hint: None,
                        });
                        continue;
                    }

                    for field in typed_enum_valued_fields {
                        field.ty = match self.normalize_sema_type(field.ty.clone(), field.loc) {
                            Some(sema_type) => sema_type,
                            None => continue,
                        };

                        self.validate_field_type(Some(typed_enum.symbol_id), &field.ty, field.loc);
                    }
                    ident
                }
            };

            if variant_names.contains(&variant_ident.value) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateEnumVariantName {
                        enum_name: typed_enum.name.clone(),
                        variant_name: variant_ident.value.clone(),
                    }),
                    loc: Some(variant_ident.loc),
                    hint: Some("Consider to rename the variant to a different name.".to_string()),
                });
                continue;
            }

            variant_names.push(variant_ident.value.clone());
        }
    }

    /// Analyzes struct fields for semantic correctness.
    ///
    /// Checks for duplicate field names within the struct and validates each field's type.
    /// Ensures type normalization and semantic validation are applied to all fields.
    fn analyze_struct_fields(&mut self, struct_stmt: &mut TypedStructStmt) {
        let mut field_names: Vec<String> = Vec::new();

        for field in &mut struct_stmt.fields {
            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        object_name: struct_stmt.name.clone(),
                        field_name: field.name.clone(),
                    }),
                    loc: Some(field.loc),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
                continue;
            }

            field.ty = match self.normalize_sema_type(field.ty.clone(), field.loc) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            self.validate_field_type(Some(struct_stmt.symbol_id), &field.ty, field.loc);
            field_names.push(field.name.clone());
        }
    }

    /// Analyzes union fields for semantic correctness.
    ///
    /// Checks for duplicate field names within the union and validates each field's type.
    /// Ensures type normalization and semantic validation are applied to all fields.
    fn analyze_union_fields(&mut self, union_stmt: &mut TypedUnionStmt) {
        let mut field_names: Vec<String> = Vec::new();

        for field in &mut union_stmt.fields {
            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        field_name: field.name.clone(),
                        object_name: union_stmt.name.clone(),
                    }),
                    loc: Some(field.loc),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
            }

            match self.normalize_sema_type(field.ty.clone(), field.loc) {
                Some(sema_type) => {
                    field.ty = sema_type;
                }
                None => continue,
            }

            self.validate_field_type(Some(union_stmt.symbol_id), &field.ty, field.loc);

            field_names.push(field.name.clone());
        }
    }

    pub(crate) fn analyze_func_body(&mut self, body: &mut TypedBlockStmt, ret_type: &SemanticType) {
        let state = self.analyze_block_stmt(body);

        if !ret_type.is_void() && state != FlowState::Returns {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MissingReturn),
                loc: Some(body.loc),
                hint: Some("Not all control paths return a value.".to_string()),
            });
        }
    }

    // FIXME
    fn analyze_non_generic_methods(&mut self, _methods: &HashMap<String, SymbolID>) {
        todo!();

        // let mut local_methods_list: Vec<(SymbolID, FuncSig, Box<TypedBlockStmt>)> = Vec::new();

        // // forward method declaration resolving
        // for symbol_id in methods.values() {
        //     let (mut func_sig, func_body_opt) = {
        //         let mut global_symbols = self.resolver.global_symbols_registry.lock().unwrap();
        //         let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        //         let symbol_entry = symbol_table.entries.get_mut(symbol_id).unwrap();

        //         match &mut symbol_entry.kind {
        //             SymbolEntryKind::Method(m) => (m.func_sig.clone(), m.func_body.take()),
        //             _ => unreachable!(),
        //         }
        //     };

        //     self.tctx.current_method_symbol_id = Some(*symbol_id);
        //     self.tctx.current_func = Some(TypedFuncType {
        //         symbol_id: Some(*symbol_id),
        //         def_module_id: Some(self.module_id),
        //         params: typed_func_params_as_func_type_params(&func_sig.params),
        //         ret_type: Box::new(func_sig.ret_type.clone()),
        //         is_public: func_sig.modifiers.vis.is_public(),
        //         loc: func_sig.loc,
        //     });
        //     self.check_method_name(func_sig.name.clone(), func_sig.loc);

        //     self.normalize_func_params(&mut func_sig.params, func_sig.loc);

        //     func_sig.ret_type = match self.normalize_sema_type( func_sig.ret_type.clone(), func_sig.loc) {
        //         Some(sema_type) => sema_type,
        //         None => return,
        //     };

        //     if let Some(typed_func_param_kind) = func_sig.params.list.first() {
        //         if let TypedFuncParamKind::SelfModifier(typed_self_modifier) = typed_func_param_kind.clone() {
        //             func_sig.params.list[0] = TypedFuncParamKind::SelfModifier(typed_self_modifier.clone());
        //         }
        //     }

        //     if let Some(func_body) = func_body_opt {
        //         local_methods_list.push((*symbol_id, func_sig.clone(), func_body));
        //     }

        //     let mut global_symbols = self.resolver.global_symbols_registry.lock().unwrap();
        //     let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        //     let symbol_entry = symbol_table.entries.get_mut(&symbol_id).unwrap();
        //     if let SymbolEntryKind::Method(m) = &mut symbol_entry.kind {
        //         m.func_sig = func_sig;
        //     }
        // }

        // // analyze methods bodies
        // for (symbol_id, func_sig, mut func_body) in local_methods_list {
        //     self.tctx.current_method_symbol_id = Some(symbol_id);
        //     self.tctx.current_func = Some(TypedFuncType {
        //         symbol_id: Some(symbol_id),
        //         params: typed_func_params_as_func_type_params(&func_sig.params),
        //         ret_type: Box::new(func_sig.ret_type.clone()),
        //         is_public: func_sig.modifiers.vis.is_public(),
        //         loc: func_sig.loc,
        //     });
        //     let state = self.analyze_block_stmt(&mut func_body);

        //     if !func_sig.ret_type.is_void() && state != FlowState::Returns {
        //         self.reporter.report(Diag {
        //             level: DiagLevel::Error,
        //             kind: Box::new(AnalyzerDiagKind::MissingReturn),
        //             loc: Some(DiagLoc::new(func_sig.loc)),
        //             hint: Some("Not all control paths return a value.".to_string()),
        //         });
        //     }

        //     let mut global_symbols = self.resolver.global_symbols_registry.lock().unwrap();
        //     let symbol_table = global_symbols.get_mut(&module_id).unwrap();
        //     let symbol_entry = symbol_table.entries.get_mut(&symbol_id).unwrap();
        //     if let SymbolEntryKind::Method(m) = &mut symbol_entry.kind {
        //         m.func_sig = func_sig;
        //         m.func_body = Some(func_body);
        //     }
        // }

        // self.tctx.current_method_symbol_id = None;
    }

    fn analyze_entry_func(&mut self, func_def: &mut TypedFuncDefStmt) {
        let is_public = func_def.modifiers.vis.is_public();

        if func_def.name == "main" {
            self.entry_points.add(func_def.loc);

            if !is_public {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::PrivateEntryPoint),
                    loc: Some(func_def.loc),
                    hint: Some("Declare it as 'pub' so the runtime and linker can reliably discover it.".to_string()),
                });
            }
        }
    }

    pub(crate) fn validate_variable_type(&mut self, sema_type: &SemanticType, is_init: bool, loc: Loc) {
        if sema_type.const_inner().is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidVariableType),
                loc: Some(loc),
                hint: None,
            });
        }

        if sema_type.const_inner().is_const() && !is_init {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ConstVariableMustBeInitialized),
                loc: Some(loc),
                hint: Some("Declare the variable with an initializer or remove the 'const' qualifier.".to_string()),
            });
        }

        if sema_type.const_inner().is_func_type() && !is_init {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UninitializedLambda),
                loc: Some(loc),
                hint: Some("Assign a function or lambda expression to this variable at declaration.".to_string()),
            });
        }

        self.check_sema_ty(sema_type.clone(), loc);
    }

    pub(crate) fn validate_field_type(&mut self, object_id_opt: Option<SymbolID>, sema_type: &SemanticType, loc: Loc) {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let sema_type = sema_type.const_inner();

        if sema_type.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidFieldType),
                loc: Some(loc),
                hint: None,
            });
        }

        if sema_type.count_const_layers() >= 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::RedundantConstQualifier),
                loc: Some(loc),
                hint: None,
            });
        }

        let sema_ty_as_symbol_id_opt = sema_type.maybe_generic_base_symbol_id();

        if let Some(object_id) = object_id_opt {
            if sema_ty_as_symbol_id_opt.map(|symbol_id| symbol_id == object_id) == Some(true) {
                let type_name = format_sema_type(sema_type.clone(), fmt_symbol);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InfiniteSizeRecursiveType { type_name }),
                    loc: Some(loc),
                    hint: None,
                });
                return;
            }
        }

        self.check_sema_ty(sema_type.clone(), loc);
    }

    pub(crate) fn validate_param_type(&mut self, sema_type: &SemanticType, loc: Loc) {
        let sema_type = sema_type.const_inner();

        if sema_type.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidParameterType),
                loc: Some(loc),
                hint: None,
            });
        }

        if sema_type.count_const_layers() >= 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::RedundantConstQualifier),
                loc: Some(loc),
                hint: None,
            });
        }

        self.check_sema_ty(sema_type.clone(), loc);
    }

    pub(crate) fn validate_struct_repr_attr(&mut self, repr_attr: &Option<ReprAttr>, fields_count: usize, loc: Loc) {
        let Some(repr_attr) = repr_attr else {
            return;
        };

        if let Some(kind) = repr_attr.kind() {
            match kind {
                ReprKind::C | ReprKind::Cyrus => { /* valid */ }
                ReprKind::Transparent => {
                    // transparent is allowed on structs and requires exactly one field
                    if fields_count != 1 {
                        self.reporter.report(Diag {
                            kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                                err: "Repr 'transparent' structs must have exactly one field.".to_string(),
                            }),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some(
                                "Add or remove fields to have exactly one field, or remove the 'transparent' attribute."
                                    .to_string(),
                            ),
                        });
                    }

                    if repr_attr.is_packed() {
                        self.reporter.report(Diag {
                            kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                                err: "Cannot combine 'packed' with repr 'transparent' on structs.".to_string(),
                            }),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some("Remove either 'packed' or 'transparent'.".to_string()),
                        });
                    }
                }
            }
        }
    }

    pub(crate) fn validate_union_repr_attr(&mut self, repr_attr: &Option<ReprAttr>, fields_count: usize, loc: Loc) {
        let Some(repr_attr) = repr_attr else {
            return;
        };

        // packed is not allowed on unions
        if repr_attr.is_packed() {
            self.reporter.report(Diag {
                kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                    err: "Packed layout is not supported for unions.".to_string(),
                }),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: Some("If you need explicit control over union layout, consider using 'repr(C)' with manual padding or a packed struct wrapper.".to_string()),
            });
            return;
        }

        if let Some(kind) = repr_attr.kind() {
            match kind {
                ReprKind::C | ReprKind::Cyrus => { /* valid */ }
                ReprKind::Transparent => {
                    // transparent unions require exactly one field
                    if fields_count != 1 {
                        self.reporter.report(Diag {
                            kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                                err: "Repr 'transparent' unions must have exactly one field.".to_string(),
                            }),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some("Add or remove fields to have exactly one field, or remove the 'transparent' attribute.".to_string()),
                        });
                        return;
                    }
                }
            }
        }
    }

    pub(crate) fn validate_enum_repr_attr(&mut self, repr_attr: &Option<ReprAttr>, has_align: bool, loc: Loc) {
        let Some(repr_attr) = repr_attr else {
            return;
        };

        // packed is not allowed on enums
        if repr_attr.is_packed() {
            self.reporter.report(Diag {
                kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                    err: "Packed layout is not supported for enums.".to_string(),
                }),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: Some(
                    "If you need packed enum-like behavior, consider using a manually packed struct with a tag field."
                        .to_string(),
                ),
            });
            return;
        }

        if let Some(kind) = repr_attr.kind() {
            match kind {
                ReprKind::C | ReprKind::Cyrus => {
                    if has_align {
                        self.reporter.report(Diag {
                            kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                                err: "Cannot specify alignment with 'c' or 'cyrus' enum layout. Alignment is determined by the target ABI.".to_string(),
                            }),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some("Remove the alignment specifier.".to_string()),
                        });
                        return;
                    }
                }
                ReprKind::Transparent => {
                    self.reporter.report(Diag {
                        kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                            err: "Repr 'transparent' cannot be applied to enums. Enums only support 'c' and 'cyrus' layouts.".to_string(),
                        }),
                        level: DiagLevel::Error,
                        loc: Some(loc),
                        hint: None,
                    });
                    return;
                }
            }
        }
    }

    pub(crate) fn validate_align(&mut self, align: &Option<usize>, loc: Loc) {
        if let Some(align) = align {
            if !align.is_power_of_two() {
                self.reporter.report(Diag {
                    kind: Box::new(AnalyzerDiagKind::InvalidAlign { value: *align }),
                    level: DiagLevel::Error,
                    loc: Some(loc),
                    hint: Some("Valid alignments are 1, 2, 4, 8, 16, etc.".to_string()),
                });
            }
        }
    }

    pub(crate) fn validate_enum_tag_type(&mut self, tag_type: &Option<SemanticType>, loc: Loc) {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        if let Some(tag_type) = tag_type {
            let tag_type = tag_type.const_inner();
            let valid = tag_type.is_integer() || tag_type.is_char() || tag_type.is_bool();

            if !valid {
                let got = format_sema_type(tag_type.clone(), fmt_symbol);

                self.reporter.report(Diag {
                    kind: Box::new(AnalyzerDiagKind::InvalidEnumTagType { got }),
                    level: DiagLevel::Error,
                    loc: Some(loc),
                    hint: None,
                });
            }
        }
    }

    pub(crate) fn fold_const_expr(&mut self, expr: &mut TypedExprStmt) {
        let mut folder = ConstFolder::new(self);
        folder.fold_expr(expr);
    }

    fn resolve_variable_rhs_expr(&mut self, symbol_id: SymbolID) -> Option<TypedExprStmt> {
        let symbol_entry = self.query.lookup_symbol_entry(symbol_id)?;

        if let Some(resolved_var) = symbol_entry.as_var() {
            resolved_var.variable.rhs.clone()
        } else if let Some(resolved_global_var) = symbol_entry.as_global_var() {
            resolved_global_var.global_var_sig.rhs.clone()
        } else {
            None
        }
    }
}

impl<'ctx, M: SymbolEntryMut> ConstResolver for AnalysisContext<'ctx, M> {
    fn resolve_symbol_expr(&mut self, symbol_id: SymbolID) -> Option<TypedExprStmt> {
        let resolved = self.resolve_variable_rhs_expr(symbol_id)?;
        Some(resolved)
    }

    fn is_symbol_const(&mut self, symbol_id: SymbolID) -> bool {
        let resolved = self.resolve_variable_rhs_expr(symbol_id);

        match resolved {
            Some(expr) => expr.sema_type.as_ref().map(|t| t.is_const()).unwrap_or(false),

            None => false,
        }
    }
}
