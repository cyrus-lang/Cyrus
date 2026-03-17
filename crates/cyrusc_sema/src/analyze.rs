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
    diagnostics::AnalyzerDiagKind,
    flowstate::{ControlContext, FlowState},
    normalizer::TypeResolutionCache,
    type_checking::context::TypeCheckContext,
};
use cyrusc_ast::{
    AssignKind,
    abi::{ReprAttr, ReprKind},
};
use cyrusc_const_eval::{fold::ConstFolder, resolver::ConstResolver, value::is_comptime_valid};
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc, reporter::DiagReporter, source_loc::SourceLoc};
use cyrusc_resolver::{
    Resolver,
    symbols::{LocalOrGlobalSymbol, LocalScopeRef, LocalSymbol, LocalSymbolKind, ResolvedVariable, SymbolEntryKind},
    update_global_symbol, update_local_symbol,
};
use cyrusc_tast::{
    exprs::{MemoryLocation, TypedAssignExpr, TypedExprKind, TypedExprStmt, TypedTupleAccessExpr},
    format::{format_sema_ty, format_unnamed_enum_ty},
    generics::{
        mapping_ctx_arena::GenericMappingCtxArena,
        monomorph::MonomorphRegistry,
        substitute::{substitute_enum_sig, substitute_func_sig},
    },
    sigs::{FuncSig, typed_func_decl_as_func_sig, typed_func_params_as_func_type_params},
    stmts::*,
    types::{
        PlainType, SemanticType, TypedFuncType, TypedUnnamedEnumType, TypedUnnamedEnumVariant,
        enum_sig_as_unnamed_enum_ty,
    },
    *,
};
use cyrusc_vtable_registry::VTableRegistry;
use std::{
    cell::RefCell,
    collections::HashMap,
    mem,
    rc::Rc,
    sync::{Arc, Mutex},
};

pub struct AnalysisContext<'a> {
    pub monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
    pub entry_points: Arc<Mutex<Vec<SourceLoc>>>,
    pub program_tree: Rc<RefCell<TypedProgramTree>>,
    pub vtable_registry: Arc<Mutex<VTableRegistry>>,
    pub reporter: DiagReporter,

    pub(crate) module_id: ModuleID,
    pub(crate) resolver: &'a Resolver,
    pub(crate) ty_caches: TypeResolutionCache,
    pub(crate) mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
    pub(crate) symbol_formatter: Box<dyn Fn(Option<ScopeID>) -> Box<dyn Fn(SymbolID) -> String + 'a> + 'a>,
    pub(crate) ty_ctx: TypeCheckContext,

    control_stack: Vec<ControlContext>,

    // TODO: Refactor by converting to a stronger struct to configure analysis context.
    pub(crate) disable_warnings: bool,
}

// ============================================================
// Analysis Context Public API
// ============================================================
impl<'a> AnalysisContext<'a> {
    pub fn new(
        resolver: &'a Resolver,
        module_id: ModuleID,
        program_tree: Rc<RefCell<TypedProgramTree>>,
        entry_points: Arc<Mutex<Vec<SourceLoc>>>,
        monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
        mapping_ctx_arena: Arc<Mutex<dyn GenericMappingCtxArena>>,
        vtable_registry: Arc<Mutex<VTableRegistry>>,

        disable_warnings: bool,
    ) -> Self {
        let symbol_formatter = Self::build_symbol_formatter(resolver, module_id);

        Self {
            ty_caches: TypeResolutionCache::default(),
            ty_ctx: TypeCheckContext::new(),
            reporter: DiagReporter::new(),
            control_stack: Vec::new(),
            program_tree,
            resolver,
            module_id,
            symbol_formatter,
            entry_points,
            disable_warnings,
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
                TypedStmt::Struct(typed_struct) => self.analyze_struct(None, typed_struct, false),
                TypedStmt::Enum(typed_enum) => self.analyze_enum(None, typed_enum, false),
                TypedStmt::Typedef(typed_typedef) => self.analyze_typedef(None, typed_typedef),
                TypedStmt::Union(typed_union) => self.analyze_union(None, typed_union, false),
                // Invalid top-level statements
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
            }
        }

        self.program_tree.borrow_mut().body = body;
    }
}

// ============================================================
// Analysis Entry Points
// ============================================================
// These functions are the primary entry points for type checking different
// expression categories. They handle top-level analysis and dispatch to
// specialized helpers for detailed checking.
impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_block_stmt(&mut self, block_stmt: &mut TypedBlockStmt) -> FlowState {
        let mut flow_state = FlowState::Reachable;
        let mut terminated = false;

        let stmts = std::mem::take(&mut block_stmt.stmts);
        let mut final_stmts = Vec::with_capacity(block_stmt.stmts.len());

        for mut stmt in stmts {
            let stmt_state = self.analyze_stmt(block_stmt.scope_id, &mut stmt);

            if terminated {
                if !self.disable_warnings {
                    self.reporter.report(Diag {
                        level: DiagLevel::Warning,
                        kind: Box::new(AnalyzerDiagKind::UnreachableCode),
                        location: Some(DiagLoc::new(stmt.loc())),
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
            self.analyze_stmt(block_stmt.scope_id, &mut defer.operand);
        }

        flow_state
    }

    pub(crate) fn analyze_stmt(&mut self, scope_id: ScopeID, typed_stmt: &mut TypedStmt) -> FlowState {
        match typed_stmt {
            TypedStmt::Expr(typed_expr) => {
                self.analyze_expr(Some(scope_id), typed_expr, typed_expr.sema_ty.clone());
                FlowState::Reachable
            }
            TypedStmt::Variable(typed_variable) => {
                self.analyze_variable(Some(scope_id), typed_variable);
                FlowState::Reachable
            }
            TypedStmt::BlockStmt(typed_block_statement) => self.analyze_block_stmt(typed_block_statement),
            TypedStmt::ExportTuple(typed_export_tuple_values) => {
                self.analyze_export_tuple_values(Some(scope_id), typed_export_tuple_values);
                FlowState::Reachable
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
            TypedStmt::Goto(typed_goto) => {
                self.analyze_goto(scope_id, typed_goto);
                FlowState::Reachable
            }
            TypedStmt::For(typed_for) => self.analyze_for_loop(Some(scope_id), typed_for),
            TypedStmt::While(typed_while) => self.analyze_while_loop(Some(scope_id), typed_while),
            TypedStmt::Switch(typed_switch) => self.analyze_switch(Some(scope_id), typed_switch),
            // Skipped
            TypedStmt::Label(..) => FlowState::Reachable,
            // Invalid statements
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidStatement),
                    location: Some(DiagLoc::new(typed_stmt.loc())),
                    hint: None,
                });
                return FlowState::Reachable;
            }
        }
    }

    pub(crate) fn analyze_export_tuple_values(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        export_tuple: &mut TypedExportTupleStmt,
    ) -> Option<()> {
        let mut explicit_sema_ty: Option<SemanticType> = None;
        if let Some(sema_ty) = &export_tuple.ty {
            match self.normalize_and_check_sema_ty(scope_id_opt, sema_ty.clone(), export_tuple.loc.clone()) {
                Some(sema_ty) => {
                    explicit_sema_ty = Some(sema_ty);
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
                    location: Some(DiagLoc::new(export_tuple.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let expr_sema_ty = self.analyze_expr(
            scope_id_opt,
            &mut export_tuple.rhs.as_mut().unwrap(),
            explicit_sema_ty.clone(),
        )?;

        let Some(tuple_type) = expr_sema_ty.as_tuple_type() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                location: Some(DiagLoc::new(export_tuple.loc.clone())),
                hint: None,
            });
            return None;
        };

        if explicit_sema_ty.is_none() {
            export_tuple.ty = Some(expr_sema_ty.clone());
        }

        if let Some(target_type) = explicit_sema_ty {
            if !self.check_type_mismatch(
                scope_id_opt,
                expr_sema_ty.clone(),
                target_type.clone(),
                export_tuple.loc.clone(),
            ) {
                let lhs_type = format_sema_ty(target_type, &(self.symbol_formatter)(scope_id_opt));
                let rhs_type = format_sema_ty(expr_sema_ty, &(self.symbol_formatter)(scope_id_opt));
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type }),
                    location: Some(DiagLoc::new(export_tuple.loc.clone())),
                    hint: None,
                });
                return None;
            }
        }

        let tuple_patterns = export_tuple.pattern.into_tuple();
        for (i, (pattern, sema_ty)) in tuple_patterns.iter().zip(tuple_type.type_list.iter()).enumerate() {
            self.analyze_tuple_pattern(
                scope_id_opt,
                export_tuple.is_const,
                pattern,
                sema_ty,
                &export_tuple.rhs.as_mut().unwrap(),
                export_tuple.loc.clone(),
                vec![i],
            );
        }

        self.normalize_and_check_sema_ty(scope_id_opt, export_tuple.ty.clone()?, export_tuple.loc.clone())?;
        Some(())
    }

    fn analyze_tuple_pattern(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        is_const: bool,
        pattern: &TypedExportPattern,
        sema_ty: &SemanticType,
        expr: &TypedExprStmt,
        loc: SourceLoc,
        access_path: Vec<usize>,
    ) {
        match pattern {
            TypedExportPattern::Ident(symbol_id) => {
                let mut rhs = expr.clone();
                let rhs_ty = rhs.sema_ty.clone().unwrap();
                let tuple_type = rhs_ty.as_tuple_type().unwrap();

                for &i in &access_path {
                    rhs = TypedExprStmt {
                        kind: TypedExprKind::TupleAccess(TypedTupleAccessExpr {
                            operand: Box::new(rhs),
                            index: i,
                            loc: loc.clone(),
                        }),
                        sema_ty: Some(tuple_type.type_list.get(i).unwrap().clone()),
                        mloc: MemoryLocation::LValue,
                        loc: loc.clone(),
                    };
                }

                self.analyze_tuple_identifier_pattern(scope_id_opt, is_const, *symbol_id, sema_ty, &rhs);
            }
            TypedExportPattern::Tuple(patterns) => {
                if let Some(tuple_type) = sema_ty.as_tuple_type() {
                    if patterns.len() != tuple_type.type_list.len() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::TupleExportedValuesAndTupleElementsCountMismatch),
                            location: Some(DiagLoc::new(loc.clone())),
                            hint: None,
                        });
                        return;
                    }
                    for (i, (sub_pattern, sub_ty)) in patterns.iter().zip(&tuple_type.type_list).enumerate() {
                        let mut new_path = access_path.clone();
                        new_path.push(i);
                        self.analyze_tuple_pattern(
                            scope_id_opt,
                            is_const,
                            sub_pattern,
                            sub_ty,
                            expr,
                            loc.clone(),
                            new_path,
                        );
                    }
                } else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: None,
                    });
                }
            }
        }
    }

    fn analyze_switch_on_enum(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_switch: &mut TypedSwitchStmt,
        unnamed_enum_type: &mut TypedUnnamedEnumType,
        enum_name: &String,
    ) -> FlowState {
        let mut branch_states = Vec::new();
        let mut used_enum_variants: Vec<String> = Vec::new();

        'cases: for i in 0..typed_switch.cases.len() {
            let case = &mut typed_switch.cases[i];

            'patterns: for pattern in &case.patterns {
                let ident = match &pattern {
                    TypedSwitchCasePattern::Ident(ident, _) => ident,
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
                                    location: Some(DiagLoc::new(case.loc.clone())),
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
                            location: Some(DiagLoc::new(case.loc.clone())),
                            hint: None,
                        });
                        continue 'patterns;
                    }
                };

                if used_enum_variants.contains(&ident) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::DuplicateEnumVariantInSwitchPatterns {
                            variant_name: ident.clone(),
                        }),
                        location: Some(DiagLoc::new(case.loc.clone())),
                        hint: Some("Remove the duplicate to avoid redundancy.".to_string()),
                    });
                }

                let variant_opt = unnamed_enum_type
                    .variants
                    .iter_mut()
                    .find(|variant| variant.ident().as_string() == *ident);

                if variant_opt.is_none() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                            enum_name: enum_name.clone(),
                            variant_name: ident.clone(),
                        }),
                        location: Some(DiagLoc::new(typed_switch.loc.clone())),
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
                                        location: Some(DiagLoc::new(case.loc.clone())),
                                        hint: None,
                                    });
                                    continue 'cases;
                                }

                                // normalize and then update valued_field type in local scope

                                for (enum_valued_field_idx, enum_valued_field) in
                                    enum_valued_fields.iter_mut().enumerate()
                                {
                                    enum_valued_field.ty = match self.normalize_sema_type(
                                        scope_id_opt,
                                        enum_valued_field.ty.clone(),
                                        SourceLoc::from_loc(ident.loc.clone(), unnamed_enum_type.loc.file_path.clone()),
                                    ) {
                                        Some(sema_ty) => sema_ty,
                                        None => continue 'patterns,
                                    };

                                    // update local variable concrete type (exported symbol)
                                    {
                                        let valued_field = &valued_fields[enum_valued_field_idx];

                                        update_local_symbol!(self, case.body.scope_id, valued_field.symbol_id,
                                            LocalSymbolKind::Variable(resolved_variable) => resolved_variable, {
                                                resolved_variable.typed_variable.ty = Some(enum_valued_field.ty.clone());
                                            }
                                        );
                                    }
                                }
                            }
                            TypedUnnamedEnumVariant::Valued(ident, valued) => {
                                if valued_fields.len() > 1 {
                                    self.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: Box::new(AnalyzerDiagKind::ValuedEnumVariantCanOnlyExportOneField {
                                            variant_name: ident.as_string(),
                                        }),
                                        location: Some(DiagLoc::new(case.loc.clone())),
                                        hint: None,
                                    });
                                    return FlowState::Reachable;
                                }

                                let valued_field = valued_fields.first().unwrap();

                                valued.sema_ty = match self.analyze_expr(scope_id_opt, valued, None) {
                                    Some(sema_ty) => Some(sema_ty),
                                    None => continue 'patterns,
                                };

                                // update local variable concrete type (exported symbol)
                                {
                                    update_local_symbol!(self, case.body.scope_id, valued_field.symbol_id,
                                        LocalSymbolKind::Variable(resolved_variable) => resolved_variable, {
                                            resolved_variable.typed_variable.ty = Some(valued.sema_ty.clone().unwrap());
                                        }
                                    );
                                }
                            }
                            TypedUnnamedEnumVariant::Ident(ident) => {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::VariantDoesNotExportAnyField {
                                        variant_name: ident.as_string(),
                                    }),
                                    location: Some(DiagLoc::new(case.loc.clone())),
                                    hint: None,
                                });
                                return FlowState::Reachable;
                            }
                        }
                    }
                }

                used_enum_variants.push(ident.clone());
            }

            let body_flow_state = self.analyze_block_stmt(&mut case.body);
            branch_states.push(body_flow_state);
        }

        if let Some(default_case) = &mut typed_switch.default_case {
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

    fn analyze_switch(&mut self, scope_id_opt: Option<ScopeID>, typed_switch: &mut TypedSwitchStmt) -> FlowState {
        self.control_stack.push(ControlContext::Switch);

        if typed_switch.cases.is_empty() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::EmptyCaseSwitchStatement),
                location: Some(DiagLoc::new(typed_switch.loc.clone())),
                hint: None,
            });
            return FlowState::Reachable;
        }

        let operand_ty = match self.analyze_expr(scope_id_opt, &mut typed_switch.operand, None) {
            Some(sema_ty) => sema_ty.const_inner().clone(),
            None => return FlowState::Reachable,
        };

        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        match if let Some(enum_symbol_id) = operand_ty.const_inner().as_enum_symbol_id() {
            Some((enum_symbol_id, None))
        } else if let Some(generic_type) = operand_ty.const_inner().as_generic_type() {
            match self
                .resolver
                .resolve_local_or_global_symbol(scope_opt.clone(), generic_type.base)
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
                        kind: Box::new(AnalyzerDiagKind::SwitchOperandIsNotEnum { expr_type }),
                        location: Some(DiagLoc::new(typed_switch.loc.clone())),
                        hint: None,
                    });
                    return FlowState::Reachable;
                }
            }
        } else if let Some(mut unnamed_enum_type) = operand_ty.const_inner().as_unnamed_enum() {
            let enum_name = format_unnamed_enum_ty(&unnamed_enum_type, &(self.symbol_formatter)(scope_id_opt));
            return self.analyze_switch_on_enum(scope_id_opt, typed_switch, &mut unnamed_enum_type, &enum_name);
        } else {
            None
        } {
            Some((enum_symbol_id, generic_type_opt)) => {
                let sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt, enum_symbol_id)
                    .unwrap();

                let resolved_enum = sym.as_enum().unwrap();
                let mut enum_sig = resolved_enum.enum_sig.clone();

                if let Some(generic_type) = generic_type_opt {
                    enum_sig = substitute_enum_sig(
                        self.mapping_ctx_arena.clone(),
                        &enum_sig,
                        generic_type.mapping_ctx.clone(),
                    )
                    .unwrap()
                }

                let mut unnamed_enum_type = enum_sig_as_unnamed_enum_ty(&enum_sig, typed_switch.loc.clone());

                return self.analyze_switch_on_enum(scope_id_opt, typed_switch, &mut unnamed_enum_type, &enum_sig.name);
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

        for case in &mut typed_switch.cases {
            let mut local_range_table: Vec<(usize, usize)> = Vec::new();

            for pattern in &mut case.patterns {
                match pattern {
                    TypedSwitchCasePattern::Expr(typed_expr, _) => {
                        let pattern_concrete_type =
                            match self.analyze_expr(scope_id_opt, typed_expr, Some(operand_ty.clone())) {
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
                                format_sema_ty(operand_ty.clone(), &(self.symbol_formatter)(scope_id_opt));
                            let pattern_type =
                                format_sema_ty(pattern_concrete_type, &(self.symbol_formatter)(scope_id_opt));

                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::TypeMismatchInCasePattern {
                                    operand_type,
                                    pattern_type,
                                }),
                                location: Some(DiagLoc::new(case.loc.clone())),
                                hint: None,
                            });
                            continue;
                        }
                    }
                    TypedSwitchCasePattern::Ident(..) | TypedSwitchCasePattern::EnumVariant(..) => {
                        let expr_type = format_sema_ty(operand_ty.clone(), &(self.symbol_formatter)(scope_id_opt));

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::SwitchOperandIsNotEnum { expr_type }),
                            location: Some(DiagLoc::new(typed_switch.loc.clone())),
                            hint: None,
                        });
                        continue;
                    }
                    TypedSwitchCasePattern::Range(range) => {
                        self.analyze_expr(scope_id_opt, &mut range.lower, Some(operand_ty.clone()));
                        self.analyze_expr(scope_id_opt, &mut range.upper, Some(operand_ty.clone()));

                        if !is_comptime_valid(&range.lower.kind) {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::ExprNotComptimeValid),
                                location: Some(DiagLoc::new(range.lower.loc.clone())),
                                hint: None,
                            });
                            continue;
                        }

                        if !is_comptime_valid(&range.upper.kind) {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::ExprNotComptimeValid),
                                location: Some(DiagLoc::new(range.upper.loc.clone())),
                                hint: None,
                            });
                            continue;
                        }

                        let mut folder = ConstFolder::new(self);
                        let lower_int = folder.expr_as_const_int(scope_id_opt, &range.lower).unwrap();
                        let upper_int = folder.expr_as_const_int(scope_id_opt, &range.upper).unwrap();

                        if lower_int >= upper_int {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::InvalidRange),
                                location: Some(DiagLoc::new(case.loc.clone())),
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
                    location: Some(DiagLoc::new(case.loc.clone())),
                    hint: None,
                });
                continue;
            }

            let body_flow_state = self.analyze_block_stmt(&mut case.body);
            branch_states.push(body_flow_state);
        }

        if let Some(default_case) = &mut typed_switch.default_case {
            let body_flow_state = self.analyze_block_stmt(default_case);
            branch_states.push(body_flow_state);
        } else {
            branch_states.push(FlowState::Reachable);
        }

        let mut branch_states = Vec::new();

        // normalize each case body
        for case in &mut typed_switch.cases {
            let body_flow_state = self.analyze_block_stmt(&mut case.body);
            branch_states.push(body_flow_state);
        }

        // default case
        if let Some(default_case) = &mut typed_switch.default_case {
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

    fn analyze_if_stmt(
        &mut self,
        scope_id: ScopeID,
        typed_if: &mut TypedIfStmt,
        expected_type: Option<SemanticType>,
    ) -> FlowState {
        let consequent_state = self.analyze_block_stmt(&mut typed_if.then_block);

        self.analyze_expr(Some(scope_id), &mut typed_if.cond, expected_type.clone());

        let alternate_state = {
            if let Some(block_stmt) = &mut typed_if.else_block {
                self.analyze_block_stmt(&mut *block_stmt)
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
        if let Some(sema_ty) = self.analyze_expr(
            scope_id_opt,
            &mut typed_while.cond,
            Some(SemanticType::PlainType(PlainType::Bool)),
        ) {
            self.check_expr_type_must_be_condition(sema_ty, typed_while.loc.clone());
        }

        self.control_stack.push(ControlContext::While);
        self.analyze_block_stmt(&mut typed_while.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    fn analyze_for_loop(&mut self, scope_id_opt: Option<ScopeID>, typed_for: &mut TypedForStmt) -> FlowState {
        if let Some(initializer) = &mut typed_for.initializer {
            self.analyze_variable(scope_id_opt, initializer);
        }

        if let Some(typed_expr) = &mut typed_for.cond {
            if let Some(sema_ty) =
                self.analyze_expr(scope_id_opt, typed_expr, Some(SemanticType::PlainType(PlainType::Bool)))
            {
                self.check_expr_type_must_be_condition(sema_ty, typed_for.loc.clone());
            }
        }

        if let Some(typed_expr) = &mut typed_for.increment {
            self.analyze_expr(scope_id_opt, typed_expr, None);
        }

        self.control_stack.push(ControlContext::Loop);
        self.analyze_block_stmt(&mut typed_for.body);
        self.control_stack.pop();

        FlowState::Reachable
    }

    fn analyze_return(&mut self, scope_id: ScopeID, typed_return: &mut TypedReturnStmt) -> FlowState {
        let func_type = self.ty_ctx.current_func.clone().unwrap();
        let return_type = self
            .normalize_sema_type(Some(scope_id), *func_type.return_type, typed_return.loc.clone())
            .unwrap();

        if return_type.is_void() && typed_return.arg.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidFunctionReturnsValue),
                location: Some(DiagLoc::new(typed_return.loc.clone())),
                hint: None,
            });
        } else if let Some(typed_expr) = &mut typed_return.arg {
            if let Some(sema_ty) = self.analyze_expr(Some(scope_id), typed_expr, Some(return_type.clone())) {
                let expected = format_sema_ty(
                    return_type.const_inner().clone(),
                    &(self.symbol_formatter)(Some(scope_id)),
                );
                let got = format_sema_ty(sema_ty.const_inner().clone(), &(self.symbol_formatter)(Some(scope_id)));

                if !self.check_type_mismatch(
                    Some(scope_id),
                    sema_ty.const_inner().clone(),
                    return_type.const_inner().clone(),
                    typed_return.loc.clone(),
                ) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ReturnStatementTypeMismatch { expected, got }),
                        location: Some(DiagLoc::new(typed_return.loc.clone())),
                        hint: None,
                    });
                }
            }
        } else if !return_type.is_void() && typed_return.arg.is_none() {
            let argument_type = format_sema_ty(return_type.clone(), &(self.symbol_formatter)(Some(scope_id)));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ReturnStatementNeedsAnArgument { argument_type }),
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
                kind: Box::new(AnalyzerDiagKind::InvalidBreakStatement),
                location: Some(DiagLoc::new(typed_break.loc.clone())),
                hint: None,
            });
            FlowState::Reachable
        } else {
            FlowState::Unreachable
        }
    }

    fn analyze_goto(&mut self, scope_id: ScopeID, typed_goto: &mut TypedGotoStmt) {
        let scope_rc = self.resolver.resolve_local_scope(self.module_id, scope_id).unwrap();
        let scope_ref = scope_rc.borrow();

        if let Some(label_id) = scope_ref.resolve_label(&typed_goto.name) {
            typed_goto.label_id = Some(label_id);
        } else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UndefinedGotoLabel {
                    label_name: typed_goto.name.clone(),
                }),
                location: Some(DiagLoc::new(typed_goto.loc.clone())),
                hint: None,
            });
        }
        drop(scope_ref);
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
                kind: Box::new(AnalyzerDiagKind::InvalidContinueStatement),
                location: Some(DiagLoc::new(typed_continue.loc.clone())),
                hint: None,
            });
            FlowState::Reachable
        } else {
            FlowState::Unreachable
        }
    }

    fn analyze_global_var(&mut self, typed_global_var: &mut TypedGlobalVarStmt) {
        if let Some(mut expr) = typed_global_var.expr.clone() {
            match self.analyze_expr(None, &mut expr, typed_global_var.ty.clone()) {
                Some(sema_ty) => Some(sema_ty),
                None => return,
            };

            if !is_comptime_valid(&expr.kind) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::GlobalVariableExprNotComptimeValid),
                    location: Some(DiagLoc::new(typed_global_var.loc.clone())),
                    hint: None,
                });
                return;
            }

            typed_global_var.expr = Some(expr);
        }

        typed_global_var.ty = match &typed_global_var.ty {
            Some(sema_ty) => self.normalize_and_check_sema_ty(None, sema_ty.clone(), typed_global_var.loc.clone()),
            None => match typed_global_var.expr.as_ref().and_then(|expr| expr.sema_ty.clone()) {
                Some(sema_ty) => Some(sema_ty),
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::GlobalVarRequiresTypeAnnotation),
                        location: Some(DiagLoc::new(typed_global_var.loc.clone())),
                        hint: None,
                    });
                    return;
                }
            },
        };

        if let Some(sema_ty) = &typed_global_var.ty {
            self.validate_variable_type(
                None,
                sema_ty,
                typed_global_var.expr.is_some(),
                typed_global_var.loc.clone(),
            );
        }

        if typed_global_var.is_const && !matches!(typed_global_var.ty, Some(SemanticType::Const(..))) {
            typed_global_var.ty = Some(typed_global_var.ty.clone().unwrap().as_const());
        }

        if let Some(expr) = &typed_global_var.expr {
            if !is_comptime_valid(&expr.kind) && !matches!(typed_global_var.ty, Some(SemanticType::Const(..))) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::GlobalVariableExprNotComptimeValid),
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
                let expr_type = expr.sema_ty.clone().unwrap();

                if *expr_type.const_inner() != *target_type.const_inner() {
                    let lhs_type = format_sema_ty(target_type.clone(), &(self.symbol_formatter)(None));
                    let rhs_type = format_sema_ty(expr_type.clone(), &(self.symbol_formatter)(None));

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type }),
                        location: Some(DiagLoc::new(typed_global_var.loc.clone())),
                        hint: Some("Global variable initializers must exactly match the declared type.".into()),
                    });
                }
            }
        }
    }

    fn analyze_struct(&mut self, scope_id_opt: Option<ScopeID>, typed_struct: &mut TypedStructStmt, is_local: bool) {
        #[inline]
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

        self.validate_struct_repr_attr(
            &typed_struct.modifiers.repr_attr,
            typed_struct.fields.len(),
            &typed_struct.loc,
        );
        self.validate_align(&typed_struct.align, &typed_struct.loc);

        if let Some(generic_params) = &typed_struct.generic_params {
            self.analyze_generics_params(generic_params);
        }

        self.ty_ctx.current_self = Some(SemanticType::ResolvedSymbol(types::ResolvedSymbol::Struct(
            typed_struct.symbol_id,
        )));

        self.check_struct_name(typed_struct.name.clone(), typed_struct.loc.clone(), is_local);

        self.analyze_struct_fields(scope_id_opt, typed_struct);

        if typed_struct.generic_params.is_none() {
            self.analyze_non_generic_methods(self.module_id, &typed_struct.methods);
        }

        self.analyze_method_generic_params(&typed_struct.name, &typed_struct.methods, &typed_struct.generic_params);

        self.analyze_object_impls_interface(
            scope_id_opt,
            typed_struct.name.clone(),
            &typed_struct.impls,
            &typed_struct.methods,
        );

        // update symbol entry
        update_struct_symbol_entry(self, &typed_struct);
    }

    fn analyze_union(&mut self, scope_id_opt: Option<ScopeID>, typed_union: &mut TypedUnionStmt, is_local: bool) {
        #[inline]
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

        self.validate_union_repr_attr(
            &typed_union.modifiers.repr_attr,
            typed_union.fields.len(),
            &typed_union.loc,
        );
        self.validate_align(&typed_union.align, &typed_union.loc);

        if let Some(generic_params) = &typed_union.generic_params {
            self.analyze_generics_params(generic_params);
        }

        self.ty_ctx.current_self = Some(SemanticType::ResolvedSymbol(types::ResolvedSymbol::Union(
            typed_union.symbol_id,
        )));

        self.check_union_name(typed_union.name.clone(), typed_union.loc.clone(), is_local);

        self.analyze_union_fields(scope_id_opt, typed_union);

        if typed_union.generic_params.is_none() {
            self.analyze_non_generic_methods(self.module_id, &typed_union.methods);
        }

        self.analyze_method_generic_params(&typed_union.name, &typed_union.methods, &typed_union.generic_params);

        self.analyze_object_impls_interface(
            scope_id_opt,
            typed_union.name.clone(),
            &typed_union.impls,
            &typed_union.methods,
        );

        update_union_symbol_entry(self, &typed_union);
    }

    fn analyze_enum(&mut self, scope_id_opt: Option<ScopeID>, typed_enum: &mut TypedEnumStmt, is_local: bool) {
        #[inline]
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

        if let Some(generic_params) = &typed_enum.generic_params {
            self.analyze_generics_params(generic_params);
        }

        self.ty_ctx.current_self = Some(SemanticType::ResolvedSymbol(types::ResolvedSymbol::Enum(
            typed_enum.symbol_id,
        )));

        self.check_enum_name(typed_enum.name.clone(), typed_enum.loc.clone(), is_local);

        self.analyze_enum_variants(scope_id_opt, typed_enum);

        if typed_enum.generic_params.is_none() {
            self.analyze_non_generic_methods(self.module_id, &typed_enum.methods);
        }

        self.analyze_method_generic_params(&typed_enum.name, &typed_enum.methods, &typed_enum.generic_params);

        self.analyze_object_impls_interface(
            scope_id_opt,
            typed_enum.name.clone(),
            &typed_enum.impls,
            &typed_enum.methods,
        );

        self.validate_enum_repr_attr(
            &typed_enum.modifiers.repr_attr,
            typed_enum.align.is_some(),
            &typed_enum.loc,
        );
        self.validate_align(&typed_enum.align, &typed_enum.loc);
        self.validate_enum_tag_type(scope_id_opt, &typed_enum.tag_type, &typed_enum.loc);

        update_enum_symbol_entry(self, &typed_enum);
    }

    fn analyze_func_def(&mut self, typed_func_def: &mut TypedFuncDefStmt) {
        self.analyze_entry_func(typed_func_def);

        let is_public = typed_func_def.modifiers.vis.is_public();
        let is_generic_func = typed_func_def.generic_params.is_some();

        if let Some(generic_params) = &typed_func_def.generic_params {
            self.analyze_generics_params(generic_params);
        }

        self.ty_ctx.current_func = Some(TypedFuncType {
            symbol_id: Some(typed_func_def.symbol_id),
            def_module_id: Some(self.module_id),
            params: typed_func_params_as_func_type_params(&typed_func_def.params),
            return_type: Box::new(typed_func_def.return_type.clone()),
            is_public,
            loc: typed_func_def.loc.clone(),
        });

        self.normalize_func_params(&mut typed_func_def.params, typed_func_def.loc.clone());

        typed_func_def.return_type =
            match self.normalize_sema_type(None, typed_func_def.return_type.clone(), typed_func_def.loc.clone()) {
                Some(sema_ty) => sema_ty,
                None => return,
            };

        if !is_generic_func {
            self.analyze_func_body(&mut typed_func_def.body, &typed_func_def.return_type);
        }

        update_global_symbol!(
            self,
            typed_func_def.module_id,
            typed_func_def.symbol_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func,
            {
                resolved_func.func_sig.params = typed_func_def.params.clone();
                resolved_func.func_sig.return_type = typed_func_def.return_type.clone();
            }
        );
    }

    fn analyze_func_decl(&mut self, typed_func_decl: &mut TypedFuncDeclStmt) {
        if typed_func_decl.generic_params.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::GenericFunctionDeclaration),
                location: Some(DiagLoc::new(typed_func_decl.loc.clone())),
                hint: None,
            });
        }

        self.check_duplicate_param_names(
            &typed_func_decl.params.list,
            typed_func_decl.params.variadic.as_ref(),
            DiagLoc::new(typed_func_decl.loc.clone()),
        );

        typed_func_decl.return_type =
            match self.normalize_sema_type(None, typed_func_decl.return_type.clone(), typed_func_decl.loc.clone()) {
                Some(sema_ty) => sema_ty,
                None => return,
            };

        self.normalize_func_params(&mut typed_func_decl.params, typed_func_decl.loc.clone());

        update_global_symbol!(self, typed_func_decl.module_id, typed_func_decl.symbol_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func, {
                resolved_func.func_sig.params = typed_func_decl.params.clone();
                resolved_func.func_sig.return_type = typed_func_decl.return_type.clone();
            }
        );
    }

    fn analyze_interface(&mut self, typed_interface: &TypedInterfaceStmt) {
        if let Some(generic_params) = &typed_interface.generic_params {
            self.analyze_generics_params(generic_params);
        }

        self.check_interface_name(typed_interface.name.clone(), typed_interface.loc.clone(), false);

        let mut name_list: Vec<String> = Vec::new();

        let resolved_interface = self
            .resolver
            .lookup_symbol_entry_with_id(typed_interface.symbol_id)
            .unwrap();

        let interface_name = resolved_interface.as_interface().unwrap().interface_sig.name.clone();

        for method in &typed_interface.methods {
            if !method.params.is_instance_method() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceMethodsMustHaveSelfModifier),
                    location: Some(DiagLoc::new(method.loc.clone())),
                    hint: None,
                });
            }

            if name_list.contains(&method.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InterfaceDuplicateMethod {
                        interface_name: interface_name.clone(),
                        method_name: method.name.clone(),
                    }),
                    location: Some(DiagLoc::new(method.loc.clone())),
                    hint: None,
                });
                continue;
            }

            name_list.push(method.name.clone());
        }
    }

    fn analyze_typedef(&mut self, scope_id_opt: Option<ScopeID>, typed_typedef: &mut TypedTypedefStmt) {
        typed_typedef.ty =
            match self.normalize_sema_type(scope_id_opt, typed_typedef.ty.clone(), typed_typedef.loc.clone()) {
                Some(sema_ty) => sema_ty,
                None => return,
            };
    }

    fn analyze_variable(&mut self, scope_id_opt: Option<ScopeID>, typed_variable: &mut TypedVarStmt) {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        if let Some(sema_ty) = &typed_variable.ty {
            typed_variable.ty = self.normalize_sema_type(scope_id_opt, sema_ty.clone(), typed_variable.loc.clone());
        }

        if let Some(rhs) = &mut typed_variable.rhs {
            let inferred_ty = self.analyze_expr(scope_id_opt, rhs, typed_variable.ty.clone());

            if inferred_ty.is_none() {
                // unhealthy expr, we can't continue this
                return;
            }

            if typed_variable.ty.is_none() {
                if let Some(sema_ty) = inferred_ty {
                    typed_variable.ty = Some(sema_ty);
                }
            }
        }

        let ty_is_const = typed_variable.ty.as_ref().map(|t| t.is_const()).unwrap_or(false);

        if typed_variable.is_const != ty_is_const {
            typed_variable.ty = typed_variable.ty.clone().map(|t| t.as_const());
        }

        if !typed_variable.is_const && ty_is_const {
            // example:
            // var x: const int = expr;

            self.reporter.report(Diag {
                level: DiagLevel::Warning,
                kind: Box::new(AnalyzerDiagKind::ConstQualifiedTypeAssignedToNonConstVariable),
                location: Some(DiagLoc::new(typed_variable.loc.clone())),
                hint: Some(
                    "Prefer declaring the variable itself as const instead of using a const-qualified type."
                        .to_string(),
                ),
            });
        }

        if let Some(sema_ty) = &typed_variable.ty {
            self.validate_variable_type(
                scope_id_opt,
                sema_ty,
                typed_variable.rhs.is_some(),
                typed_variable.loc.clone(),
            );
        }

        scope_opt.inspect(|scope| {
            let mut scope_ref = scope.borrow_mut();
            scope_ref.insert(
                typed_variable.name.clone(),
                LocalSymbol::new(LocalSymbolKind::Variable(ResolvedVariable {
                    module_id: self.module_id,
                    symbol_id: typed_variable.symbol_id,
                    typed_variable: typed_variable.clone(),
                })),
            );
            drop(scope_ref);
        });

        if let Some(expr) = &mut typed_variable.rhs {
            if let Some(target_type) = &typed_variable.ty {
                if !self.check_type_mismatch(
                    None,
                    expr.sema_ty.clone().unwrap(),
                    target_type.clone(),
                    typed_variable.loc.clone(),
                ) {
                    let lhs_type = format_sema_ty(target_type.clone(), &(self.symbol_formatter)(None));
                    let rhs_type = format_sema_ty(expr.sema_ty.clone().unwrap(), &(self.symbol_formatter)(None));

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type }),
                        location: Some(DiagLoc::new(typed_variable.loc.clone())),
                        hint: None,
                    });
                }
            }
        }
    }

    pub(crate) fn analyze_assign(&mut self, scope_id_opt: Option<ScopeID>, assign: &mut TypedAssignExpr) {
        let lhs_type = match self.analyze_expr(scope_id_opt, &mut assign.lhs, None) {
            Some(sema_ty) => sema_ty,
            None => return,
        };

        let rhs_type = match self.analyze_expr(scope_id_opt, &mut assign.rhs, Some(lhs_type.clone())) {
            Some(sema_ty) => sema_ty,
            None => return,
        };

        if lhs_type.is_const() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotAssignToConstLValue),
                location: Some(DiagLoc::new(assign.loc.clone())),
                hint: None,
            });
        }

        debug_assert!(assign.kind == AssignKind::Default);

        if !self.check_type_mismatch(scope_id_opt, rhs_type.clone(), lhs_type.clone(), assign.loc.clone()) {
            let lhs_type = format_sema_ty(lhs_type, &(self.symbol_formatter)(scope_id_opt));
            let rhs_type = format_sema_ty(rhs_type, &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type }),
                location: Some(DiagLoc::new(assign.loc.clone())),
                hint: None,
            });
        }
    }
}

// ============================================================
// Helper Functions
// ============================================================
impl<'a> AnalysisContext<'a> {
    fn analyze_object_impls_interface(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        object_name: String,
        impls: &Vec<TypedImplementInterface>,
        method_ids: &HashMap<String, SymbolID>,
    ) {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        for implement_interface in impls {
            let sym = self
                .resolver
                .resolve_local_or_global_symbol(scope_opt.clone(), implement_interface.symbol_id)
                .unwrap();

            let interface_name = sym.symbol_name().unwrap_or("UNKNOWN".to_string());

            let resolved_interface = match sym.as_interface() {
                Some(resolved_interface) => resolved_interface,
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::SymbolIsNotInterface {
                            symbol_name: interface_name,
                        }),
                        location: Some(DiagLoc::new(implement_interface.loc.clone())),
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
                    kind: Box::new(AnalyzerDiagKind::InternalSymbolAccess {
                        symbol_name: interface_name.clone(),
                    }),
                    location: Some(DiagLoc::new(implement_interface.loc.clone())),
                    hint: None,
                });
                continue;
            }

            let interface_method_decls = &resolved_interface.interface_sig.methods;
            let mut interface_method_sigs: Vec<FuncSig> = Vec::new();

            if let Some(generic_params) = &resolved_interface.interface_sig.generic_params {
                let Some(type_args) = &implement_interface.type_args else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::MissingTypeArgs {
                            type_name: interface_name.clone(),
                        }),
                        location: Some(DiagLoc::new(implement_interface.loc.clone())),
                        hint: None,
                    });
                    continue;
                };

                let generic_type_opt = match self.init_generic_type_with_symbol_id(
                    scope_id_opt,
                    scope_opt.clone(),
                    implement_interface.symbol_id,
                    &mut Some(type_args.clone()),
                    None,
                    Some(generic_params),
                    false,
                    implement_interface.loc.clone(),
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
                            interface_name: interface_name.clone(),
                        }),
                        location: Some(DiagLoc::new(implement_interface.loc.clone())),
                        hint: None,
                    });
                    continue;
                }

                let object_method_symbol_id = method_ids.get(&interface_method_sig.name).unwrap();
                let mut sym = self
                    .resolver
                    .resolve_local_or_global_symbol(scope_opt.clone(), *object_method_symbol_id)
                    .unwrap();

                let object_method = sym.as_method_mut().unwrap();

                // check method signature mismatch
                if object_method.func_sig != interface_method_sig {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InterfaceMethodTypeMismatch {
                            object_name: object_name.clone(),
                            interface_name: interface_name.clone(),
                            method_name: interface_method_sig.name.clone(),
                        }),
                        location: Some(DiagLoc::new(object_method.func_sig.loc.clone())),
                        hint: None,
                    });
                }
            }
        }
    }

    fn analyze_method_generic_params(
        &mut self,
        object_name: &String,
        methods: &HashMap<String, SymbolID>,
        generic_params_opt: &Option<TypedGenericParamsList>,
    ) {
        if let Some(generic_params) = generic_params_opt {
            for method_id in methods.values().cloned() {
                let sym = self.resolver.resolve_local_or_global_symbol(None, method_id).unwrap();

                if let Some(method_generic_params) = sym.symbol_method_generic_params() {
                    for method_generic_param in &method_generic_params.list {
                        if generic_params
                            .lookup_named(&method_generic_param.param_name.name)
                            .is_some()
                        {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::ShadowsObjectGenericParam {
                                    param_name: method_generic_param.param_name.name.clone(),
                                    object_name: object_name.clone(),
                                }),
                                location: Some(DiagLoc::new(method_generic_param.param_name.loc.clone())),
                                hint: Some("Consider to rename the field to a different name.".to_string()),
                            });
                        }
                    }
                }
            }
        }
    }

    fn analyze_generics_params(&mut self, generic_params: &TypedGenericParamsList) {
        let mut collected_names: Vec<String> = Vec::new();
        for generic_param in &generic_params.list {
            if collected_names.contains(&generic_param.param_name.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateGenericParam {
                        param_name: generic_param.param_name.name.clone(),
                    }),
                    location: Some(DiagLoc::new(generic_param.param_name.loc.clone())),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
            }

            collected_names.push(generic_param.param_name.name.clone());
        }
    }

    fn analyze_enum_variants(&mut self, scope_id_opt: Option<ScopeID>, typed_enum: &mut TypedEnumStmt) {
        let is_repr_c = typed_enum.is_repr_c();
        let mut variant_names: Vec<String> = Vec::new();

        for variant in &mut typed_enum.variants {
            let variant_ident = match variant {
                TypedEnumVariant::Ident(ident) => ident,
                TypedEnumVariant::Valued(ident, typed_expr) => {
                    typed_expr.sema_ty = match self.analyze_expr(scope_id_opt, typed_expr, None) {
                        Some(sema_ty) => Some(sema_ty),
                        None => continue,
                    };

                    if is_repr_c && !typed_expr.sema_ty.as_ref().unwrap().is_integer() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ReprCEnumWithNonIntegerVariant),
                            location: Some(DiagLoc::new(SourceLoc::from_loc(
                                ident.loc.clone(),
                                typed_enum.loc.file_path.clone(),
                            ))),
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
                            location: Some(DiagLoc::new(SourceLoc::from_loc(
                                ident.loc.clone(),
                                typed_enum.loc.file_path.clone(),
                            ))),
                            hint: None,
                        });
                        continue;
                    }

                    for field in typed_enum_valued_fields {
                        field.ty = match self.normalize_sema_type(scope_id_opt, field.ty.clone(), field.loc.clone()) {
                            Some(sema_ty) => sema_ty,
                            None => continue,
                        };

                        self.validate_field_type(scope_id_opt, typed_enum.symbol_id, &field.ty, field.loc.clone());
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
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        variant_ident.loc.clone(),
                        typed_enum.loc.file_path.clone(),
                    ))),
                    hint: Some("Consider to rename the variant to a different name.".to_string()),
                });
                continue;
            }

            variant_names.push(variant_ident.value.clone());
        }
    }

    fn analyze_struct_fields(&mut self, scope_id_opt: Option<ScopeID>, typed_struct: &mut TypedStructStmt) {
        let mut field_names: Vec<String> = Vec::new();

        for field in &mut typed_struct.fields {
            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        object_name: typed_struct.name.clone(),
                        field_name: field.name.clone(),
                    }),
                    location: Some(DiagLoc::new(field.loc.clone())),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
                continue;
            }

            field.ty = match self.normalize_sema_type(scope_id_opt, field.ty.clone(), field.loc.clone()) {
                Some(sema_ty) => sema_ty,
                None => continue,
            };

            self.validate_field_type(scope_id_opt, typed_struct.symbol_id, &field.ty, field.loc.clone());
            field_names.push(field.name.clone());
        }
    }

    fn analyze_union_fields(&mut self, scope_id_opt: Option<ScopeID>, typed_union: &mut TypedUnionStmt) {
        let mut field_names: Vec<String> = Vec::new();

        for field in &mut typed_union.fields {
            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        field_name: field.name.clone(),
                        object_name: typed_union.name.clone(),
                    }),
                    location: Some(DiagLoc::new(field.loc.clone())),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
            }

            match self.normalize_sema_type(scope_id_opt, field.ty.clone(), field.loc.clone()) {
                Some(sema_ty) => {
                    field.ty = sema_ty;
                }
                None => continue,
            }

            self.validate_field_type(scope_id_opt, typed_union.symbol_id, &field.ty, field.loc.clone());

            field_names.push(field.name.clone());
        }
    }

    fn analyze_tuple_identifier_pattern(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        is_const: bool,
        symbol_id: SymbolID,
        sema_ty: &SemanticType,
        rhs: &TypedExprStmt,
    ) {
        let mut ty = sema_ty.clone();
        if is_const && !matches!(ty, SemanticType::Const(..)) {
            ty = ty.as_const();
        }

        if let Some(scope_id) = scope_id_opt {
            update_local_symbol!(self, scope_id, symbol_id,
                LocalSymbolKind::Variable(resolved_variable) => resolved_variable, {
                    resolved_variable.typed_variable.ty = Some(ty);
                    resolved_variable.typed_variable.rhs = Some(rhs.clone());
                }
            );
        }
    }

    fn check_duplicate_param_names(
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
                            kind: Box::new(AnalyzerDiagKind::DuplicateFuncParameter {
                                param_name: typed_func_param.name.clone(),
                                param_idx: param_idx.try_into().unwrap(),
                            }),
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
                TypedFuncVariadicParams::Typed(ident, _) => {
                    if param_names.contains(&ident.name) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::DuplicateFuncVariadicParameter {
                                param_name: ident.name.clone(),
                            }),
                            location: Some(location.clone()),
                            hint: Some("Consider to rename the parameter to a different name.".to_string()),
                        });
                    }
                }
                TypedFuncVariadicParams::UntypedCStyle => {}
            }
        }
    }

    pub(crate) fn analyze_func_body(&mut self, body: &mut TypedBlockStmt, return_type: &SemanticType) {
        let state = self.analyze_block_stmt(body);
        if !return_type.is_void() && state != FlowState::Returns {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MissingReturn),
                location: Some(DiagLoc::new(body.loc.clone())),
                hint: Some("Not all control paths return a value.".to_string()),
            });
        }
    }

    fn analyze_non_generic_methods(&mut self, module_id: ModuleID, methods: &HashMap<String, SymbolID>) {
        let mut local_methods_list: Vec<(SymbolID, FuncSig, Box<TypedBlockStmt>)> = Vec::new();

        // forward method declaration resolving
        for symbol_id in methods.values() {
            let (mut func_sig, func_body_opt) = {
                let mut global_symbols = self.resolver.global_symbols.lock().unwrap();
                let symbol_table = global_symbols.get_mut(&module_id).unwrap();
                let symbol_entry = symbol_table.entries.get_mut(symbol_id).unwrap();

                match &mut symbol_entry.kind {
                    SymbolEntryKind::Method(m) => (m.func_sig.clone(), m.func_body.take()),
                    _ => unreachable!(),
                }
            };

            self.ty_ctx.current_method_symbol_id = Some(*symbol_id);
            self.ty_ctx.current_func = Some(TypedFuncType {
                symbol_id: Some(*symbol_id),
                def_module_id: Some(self.module_id),
                params: typed_func_params_as_func_type_params(&func_sig.params),
                return_type: Box::new(func_sig.return_type.clone()),
                is_public: func_sig.modifiers.vis.is_public(),
                loc: func_sig.loc.clone(),
            });
            self.check_method_name(func_sig.name.clone(), func_sig.loc.clone());

            self.normalize_func_params(&mut func_sig.params, func_sig.loc.clone());

            func_sig.return_type =
                match self.normalize_sema_type(None, func_sig.return_type.clone(), func_sig.loc.clone()) {
                    Some(sema_ty) => sema_ty,
                    None => return,
                };

            if let Some(typed_func_param_kind) = func_sig.params.list.first() {
                if let TypedFuncParamKind::SelfModifier(typed_self_modifier) = typed_func_param_kind.clone() {
                    func_sig.params.list[0] = TypedFuncParamKind::SelfModifier(typed_self_modifier.clone());
                }
            }

            if let Some(func_body) = func_body_opt {
                local_methods_list.push((*symbol_id, func_sig.clone(), func_body));
            }

            let mut global_symbols = self.resolver.global_symbols.lock().unwrap();
            let symbol_table = global_symbols.get_mut(&module_id).unwrap();
            let symbol_entry = symbol_table.entries.get_mut(&symbol_id).unwrap();
            if let SymbolEntryKind::Method(m) = &mut symbol_entry.kind {
                m.func_sig = func_sig;
            }
        }

        // analyze methods bodies
        for (symbol_id, func_sig, mut func_body) in local_methods_list {
            self.ty_ctx.current_method_symbol_id = Some(symbol_id);
            self.ty_ctx.current_func = Some(TypedFuncType {
                symbol_id: Some(symbol_id),
                def_module_id: Some(self.module_id),
                params: typed_func_params_as_func_type_params(&func_sig.params),
                return_type: Box::new(func_sig.return_type.clone()),
                is_public: func_sig.modifiers.vis.is_public(),
                loc: func_sig.loc.clone(),
            });
            let state = self.analyze_block_stmt(&mut func_body);

            if !func_sig.return_type.is_void() && state != FlowState::Returns {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::MissingReturn),
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

        self.ty_ctx.current_method_symbol_id = None;
    }

    fn analyze_entry_func(&mut self, typed_func_def: &mut TypedFuncDefStmt) {
        let is_public = typed_func_def.modifiers.vis.is_public();

        if typed_func_def.name == "main" {
            let mut entry_points = self.entry_points.lock().unwrap();
            entry_points.push(typed_func_def.loc.clone());

            if !is_public {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::PrivateEntryPoint),
                    location: Some(DiagLoc::new(typed_func_def.loc.clone())),
                    hint: Some("Declare it as 'pub' so the runtime and linker can reliably discover it.".to_string()),
                });
            }
        }
    }

    pub(crate) fn validate_variable_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        sema_ty: &SemanticType,
        is_init: bool,
        loc: SourceLoc,
    ) {
        if sema_ty.const_inner().is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidVariableType),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
        }

        if sema_ty.const_inner().is_const() && !is_init {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ConstVariableMustBeInitialized),
                location: Some(DiagLoc::new(loc.clone())),
                hint: Some("Declare the variable with an initializer or remove the 'const' qualifier.".to_string()),
            });
        }

        if sema_ty.const_inner().is_func_type() && !is_init {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UninitializedLambda),
                location: Some(DiagLoc::new(loc.clone())),
                hint: Some("Assign a function or lambda expression to this variable at declaration.".to_string()),
            });
        }

        self.check_sema_ty(scope_id_opt, sema_ty.clone(), loc);
    }

    pub(crate) fn validate_field_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        object_symbol_id: SymbolID,
        sema_ty: &SemanticType,
        loc: SourceLoc,
    ) {
        let sema_ty = sema_ty.const_inner();

        if sema_ty.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidFieldType),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
        }

        if sema_ty.count_const_layers() >= 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::RedundantConstQualifier),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
        }

        let sema_ty_as_symbol_id_opt = sema_ty.maybe_generic_base_symbol_id();

        if sema_ty_as_symbol_id_opt.map(|symbol_id| symbol_id == object_symbol_id) == Some(true) {
            let type_name = format_sema_ty(sema_ty.clone(), &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InfiniteSizeRecursiveType { type_name }),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
            return;
        }

        self.check_sema_ty(scope_id_opt, sema_ty.clone(), loc);
    }

    pub(crate) fn validate_param_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        sema_ty: &SemanticType,
        loc: SourceLoc,
    ) {
        let sema_ty = sema_ty.const_inner();

        if sema_ty.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidParameterType),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
        }

        if sema_ty.count_const_layers() >= 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::RedundantConstQualifier),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
        }

        self.check_sema_ty(scope_id_opt, sema_ty.clone(), loc);
    }

    pub(crate) fn validate_struct_repr_attr(
        &mut self,
        repr_attr: &Option<ReprAttr>,
        fields_count: usize,
        loc: &SourceLoc,
    ) {
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
                            location: Some(DiagLoc::new(loc.clone())),
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
                            location: Some(DiagLoc::new(loc.clone())),
                            hint: Some("Remove either 'packed' or 'transparent'.".to_string()),
                        });
                    }
                }
            }
        }
    }

    pub(crate) fn validate_union_repr_attr(
        &mut self,
        repr_attr: &Option<ReprAttr>,
        fields_count: usize,
        loc: &SourceLoc,
    ) {
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
                location: Some(DiagLoc::new(loc.clone())),
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
                            location: Some(DiagLoc::new(loc.clone())),
                            hint: Some("Add or remove fields to have exactly one field, or remove the 'transparent' attribute.".to_string()),
                        });
                        return;
                    }
                }
            }
        }
    }

    pub(crate) fn validate_enum_repr_attr(&mut self, repr_attr: &Option<ReprAttr>, has_align: bool, loc: &SourceLoc) {
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
                location: Some(DiagLoc::new(loc.clone())),
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
                            location: Some(DiagLoc::new(loc.clone())),
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
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: None,
                    });
                    return;
                }
            }
        }
    }

    pub(crate) fn validate_align(&mut self, align: &Option<usize>, loc: &SourceLoc) {
        if let Some(align) = align {
            if !align.is_power_of_two() {
                self.reporter.report(Diag {
                    kind: Box::new(AnalyzerDiagKind::InvalidAlign { value: *align }),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: Some("Valid alignments are 1, 2, 4, 8, 16, etc.".to_string()),
                });
            }
        }
    }

    pub(crate) fn validate_enum_tag_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        tag_type: &Option<SemanticType>,
        loc: &SourceLoc,
    ) {
        if let Some(tag_type) = tag_type {
            let tag_type = tag_type.const_inner();
            let valid = tag_type.is_integer() || tag_type.is_char() || tag_type.is_bool();

            if !valid {
                let got = format_sema_ty(tag_type.clone(), &(self.symbol_formatter)(scope_id_opt));

                self.reporter.report(Diag {
                    kind: Box::new(AnalyzerDiagKind::InvalidEnumTagType { got }),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
            }
        }
    }

    pub(crate) fn fold_consts(&mut self, scope_id_opt: Option<ScopeID>, expr: &mut TypedExprStmt) {
        let mut folder = ConstFolder::new(self);
        folder.fold_expr(scope_id_opt, expr);
    }

    fn resolve_any_variable_expr(
        &mut self,
        scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<TypedExprStmt> {
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt.clone(), symbol_id)?;

        match match &sym {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => {
                let variable = &local_symbol.as_variable()?.typed_variable;
                let mut expr = variable.rhs.clone()?;

                if let Some(sema_ty) = &variable.ty {
                    expr.sema_ty = Some(sema_ty.clone());
                }
                Some(expr)
            }
            LocalOrGlobalSymbol::GlobalSymbol(global_symbol) => match global_symbol.as_global_var() {
                Some(resolved_global_var) => {
                    let mut expr = resolved_global_var.global_var_sig.rhs.clone()?;

                    if let Some(sema_ty) = &resolved_global_var.global_var_sig.ty {
                        expr.sema_ty = Some(sema_ty.clone());
                    }

                    Some(expr)
                }
                None => None,
            },
        } {
            Some(typed_expr) => Some(typed_expr),
            None => None,
        }
    }
}

impl<'ctx> ConstResolver for AnalysisContext<'ctx> {
    fn resolve_symbol_expr(&mut self, symbol: SymbolID) -> Option<TypedExprStmt> {
        let resolved = self.resolve_any_variable_expr(None, symbol)?;
        Some(resolved)
    }

    fn symbol_is_const(&mut self, symbol: SymbolID) -> bool {
        let resolved = self.resolve_any_variable_expr(None, symbol);

        match resolved {
            Some(expr) => expr.sema_ty.as_ref().map(|t| t.is_const()).unwrap_or(false),

            None => false,
        }
    }
}
