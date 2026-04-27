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

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::Ident;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::flow_state::{ControlRegion, FlowState};
use cyrusc_typed_ast::{
    decls::EnumDecl,
    format::{format_enum_decl, format_sema_type},
    stmts::{
        TypedBreakStmt, TypedContinueStmt, TypedEnumVariant, TypedForStmt, TypedIfStmt, TypedReturnStmt,
        TypedSwitchCasePattern, TypedSwitchCasePatternKind, TypedSwitchStmt, TypedWhileStmt,
    },
    substitute::instantiate_enum_decl_with_type_args,
    types::SemaType,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_switch(&mut self, switch_stmt: &mut TypedSwitchStmt) -> FlowState {
        let Some(operand_type) = self.analyze_expr(&mut switch_stmt.operand, None) else {
            return FlowState::Reachable;
        };

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
            } else if operand_type.is_plain_type() || operand_type.is_char_pointer() {
                todo!()
            } else {
                let expr_type = format_sema_type(operand_type, this.formatter);
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

    fn analyze_switch_on_enum(&mut self, switch_stmt: &mut TypedSwitchStmt, operand_type: &SemaType) -> FlowState {
        let named_type = operand_type.as_named_type().unwrap();
        let enum_decl_id = named_type.type_decl_id.as_enum().unwrap();
        let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

        let inst_enum_decl = instantiate_enum_decl_with_type_args(&enum_decl, &named_type.type_args);

        for case in &mut switch_stmt.cases {
            self.analyze_switch_on_enum_case_patterns(&case.patterns, &inst_enum_decl);

            self.analyze_block_stmt(&mut case.body);
        }

        FlowState::Reachable
    }

    fn analyze_switch_on_enum_case_patterns(
        &mut self,
        patterns: &Vec<TypedSwitchCasePattern>,
        inst_enum_decl: &EnumDecl,
    ) {
        let enum_name = format_enum_decl(&inst_enum_decl, self.formatter);

        let mut exporting_pattern_count = 0;

        fn find_variant<'a>(enum_decl: &'a EnumDecl, ident: &Ident) -> Option<&'a TypedEnumVariant> {
            enum_decl.variants.iter().find(|variant| match variant {
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
            inst_enum_decl: &EnumDecl,
            enum_name: &String,
            exporting_pattern_count: &mut usize,
        ) {
            match &pattern.kind {
                TypedSwitchCasePatternKind::Wildcard => {
                    // does not export anything
                }

                TypedSwitchCasePatternKind::Binding { .. } => {
                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::OnlyVariantPatternIsAllowedInSwitch),
                        loc: Some(pattern.loc),
                        hint: None,
                    });

                    *exporting_pattern_count += 1;
                }

                // .EnumUnit
                TypedSwitchCasePatternKind::EnumUnit(ident) => {
                    if find_variant(inst_enum_decl, ident).is_none() {
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
                }

                // Tuple Variant
                TypedSwitchCasePatternKind::EnumTupleVariant { variant, items } => {
                    let Some(enum_variant) = find_variant(inst_enum_decl, variant) else {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                                enum_name: enum_name.clone(),
                                variant_name: variant.as_string(),
                            }),
                            loc: Some(pattern.loc),
                            hint: None,
                        });
                        return;
                    };

                    match enum_variant {
                        TypedEnumVariant::Tuple { fields, .. } => {
                            if fields.len() != items.len() {
                                this.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(
                                        AnalyzerDiagKind::TupleExportedValuesAndTupleElementsCountMismatch {
                                            expected: fields.len(),
                                            provided: items.len(),
                                        },
                                    ),
                                    loc: Some(pattern.loc),
                                    hint: None,
                                });
                            }

                            *exporting_pattern_count += 1;

                            for (pattern, field) in items.iter().zip(fields) {
                                expose_pattern_bindings(this, pattern, &field.ty);
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
                    variant,
                    items,
                    has_rest: _,
                } => {
                    let Some(enum_variant) = find_variant(inst_enum_decl, variant) else {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                                enum_name: enum_name.clone(),
                                variant_name: variant.as_string(),
                            }),
                            loc: Some(pattern.loc),
                            hint: None,
                        });
                        return;
                    };

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

                            *exporting_pattern_count += 1;

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
            analyze_pattern(self, pattern, inst_enum_decl, &enum_name, &mut exporting_pattern_count);
        }

        if exporting_pattern_count > 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MultipleExportingPatternsInSwitchCase),
                loc: None,
                hint: None,
            });
        }
    }
}

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
            self.analyze_variable(initializer);
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

        let Some(ret_type) = self.normalize_and_check_type_formation(*func_type.ret_type, ret.loc) else {
            return FlowState::Reachable;
        };

        if ret_type.is_void() && ret.arg.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidFunctionReturnsValue),
                loc: Some(ret.loc),
                hint: None,
            });
        } else if let Some(expr) = &mut ret.arg {
            if let Some(sema_type) = self.analyze_expr(expr, Some(ret_type.clone())) {
                if !self.is_assignable_to(sema_type.clone(), ret_type.clone(), expr.loc) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ReturnStatementTypeMismatch {
                            expected: format_sema_type(ret_type.const_inner().clone(), self.formatter),
                            got: format_sema_type(sema_type.const_inner().clone(), self.formatter),
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
}
