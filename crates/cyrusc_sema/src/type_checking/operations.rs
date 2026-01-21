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
use crate::{analyze::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::operators::{InfixOperator, PrefixOperator};
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc, source_loc::SourceLoc};
use cyrusc_resolver::symbols::LocalScopeRef;
use cyrusc_tast::{
    ScopeID,
    exprs::*,
    format::format_sema_ty,
    generics::mapping_ctx::mapping_ctx_eq_refcell,
    types::{PlainType, SemanticType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_infix_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        infix_expr: &mut TypedInfixExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let lhs_type = match self.analyze_expr(scope_id_opt, &mut infix_expr.lhs, expected_type.clone()) {
            Some(sema_ty) => sema_ty.const_inner().clone(),
            None => return None,
        };

        let rhs_type = match self.analyze_expr(scope_id_opt, &mut infix_expr.rhs, Some(lhs_type.clone())) {
            Some(sema_ty) => sema_ty.const_inner().clone(),
            None => return None,
        };

        match infix_expr.op {
            InfixOperator::Add | InfixOperator::Sub | InfixOperator::Mul | InfixOperator::Div | InfixOperator::Rem => {
                self.analyze_arithmetic_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone())
            }
            InfixOperator::LessThan
            | InfixOperator::LessEqual
            | InfixOperator::GreaterThan
            | InfixOperator::GreaterEqual => {
                self.analyze_compare_expr(scope_id_opt, lhs_type, rhs_type, false, infix_expr.loc.clone())
            }
            InfixOperator::Equal | InfixOperator::NotEqual => {
                self.analyze_compare_expr(scope_id_opt, lhs_type, rhs_type, true, infix_expr.loc.clone())
            }
            InfixOperator::Or => self.analyze_or_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone()),
            InfixOperator::And => self.analyze_and_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone()),
            InfixOperator::BitwiseAnd
            | InfixOperator::BitwiseOr
            | InfixOperator::BitwiseXor
            | InfixOperator::BitwiseAndNot => {
                self.analyze_bitwise_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone())
            }
            InfixOperator::ShiftLeft => {
                self.analyze_left_shift_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone())
            }
            InfixOperator::ShiftRight => {
                self.analyze_right_shift_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone())
            }
        }
    }

    pub(crate) fn analyze_addr_of_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        addr_of: &mut TypedAddrOfExpr,
    ) -> Option<SemanticType> {
        if !addr_of.operand.is_lvalue() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::AddressOfRvalue),
                location: Some(DiagLoc::new(addr_of.loc.clone())),
                hint: None,
            });
            return None;
        }

        let operand_inner_type = addr_of.operand.sema_ty.clone();
        let operand_type = match self.analyze_expr(scope_id_opt, &mut addr_of.operand, operand_inner_type) {
            Some(sema_ty) => sema_ty.const_inner().clone(),
            None => return None,
        };

        Some(SemanticType::Pointer(Box::new(operand_type)))
    }

    pub(crate) fn analyze_deref_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        deref: &mut TypedDerefExpr,
    ) -> Option<SemanticType> {
        let operand_inner_type = deref.operand.sema_ty.clone();
        let operand_type = match self.analyze_expr(scope_id_opt, &mut deref.operand, operand_inner_type) {
            Some(sema_ty) => sema_ty.const_inner().clone(),
            None => return None,
        };

        deref.operand.sema_ty = Some(operand_type.clone());

        if (!deref.operand.is_lvalue() || operand_type.as_func_type().is_some()) && !operand_type.is_pointer() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::DerefNonPointerValue),
                location: Some(DiagLoc::new(deref.loc.clone())),
                hint: None,
            });
            return None;
        }

        let inner_ty = match operand_type {
            SemanticType::Pointer(sema_ty) => *sema_ty,
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DerefNonPointerValue),
                    location: Some(DiagLoc::new(deref.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        if inner_ty.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::DerefVoidPointerValue),
                location: Some(DiagLoc::new(deref.loc.clone())),
                hint: Some("Cast 'void*' to a concrete pointer type before dereferencing it.".to_string()),
            });
            return None;
        }

        Some(inner_ty)
    }

    pub(crate) fn analyze_sizeof_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        sizeof_expr: &mut TypedSizeOfExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let symbol_id = match &sizeof_expr.operand.kind {
            TypedExprKind::SemanticType(sema_ty) => {
                if let SemanticType::UnresolvedSymbol(symbol_id) = sema_ty {
                    *symbol_id
                } else {
                    self.normalize_type(scope_id_opt, sema_ty.clone(), sizeof_expr.loc.clone())?;
                    return Some(SemanticType::PlainType(PlainType::USize));
                }
            }
            TypedExprKind::Symbol(symbol_id, ..) => *symbol_id,
            _ => {
                self.analyze_expr(scope_id_opt, &mut sizeof_expr.operand, expected_type);
                return Some(SemanticType::PlainType(PlainType::USize));
            }
        };

        let local_scope_opt =
            scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .unwrap();

        if sym.as_global_var().is_some() || sym.as_variable().is_some() {
            // consider as expr
            self.analyze_expr(scope_id_opt, &mut sizeof_expr.operand, expected_type);
        } else {
            // consider as type
            self.normalize_type(
                scope_id_opt,
                SemanticType::UnresolvedSymbol(symbol_id),
                sizeof_expr.loc.clone(),
            )?;
        }

        Some(SemanticType::PlainType(PlainType::USize))
    }

    pub(crate) fn analyze_prefix_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        prefix_expr: &mut TypedPrefixExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let operand_type = match self.analyze_expr(scope_id_opt, &mut prefix_expr.operand, expected_type) {
            Some(sema_ty) => sema_ty.const_inner().clone(),
            None => return None,
        };

        match prefix_expr.op {
            PrefixOperator::BitwiseNot => {
                let valid_concrete_type = match &operand_type {
                    SemanticType::PlainType(basic_concrete_type) => {
                        if basic_concrete_type.is_integer() {
                            Some(basic_concrete_type.clone())
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_concrete_type {
                    Some(sema_ty) => Some(SemanticType::PlainType(sema_ty.clone())),
                    None => {
                        let operand_type = format_sema_ty(operand_type, &(self.symbol_formatter)(scope_id_opt));

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::PrefixMinusOnNonInteger { operand_type }),
                            location: Some(DiagLoc::new(prefix_expr.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
            PrefixOperator::Bang => {
                let valid_concrete_type = match &operand_type {
                    SemanticType::PlainType(basic_concrete_type) => {
                        if basic_concrete_type.is_bool() {
                            Some(basic_concrete_type)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_concrete_type {
                    Some(sema_ty) => Some(SemanticType::PlainType(sema_ty.clone())),
                    None => {
                        let operand_type = format_sema_ty(operand_type, &(self.symbol_formatter)(scope_id_opt));

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::PrefixBangOnNonBool { operand_type }),
                            location: Some(DiagLoc::new(prefix_expr.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
            PrefixOperator::Minus => {
                let valid_concrete_type = match &operand_type {
                    SemanticType::PlainType(basic_concrete_type) => {
                        if basic_concrete_type.is_integer() || basic_concrete_type.is_float() {
                            if !basic_concrete_type.is_signed() {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::UnaryOperatorMinusOnUnsignedInteger),
                                    location: Some(DiagLoc::new(prefix_expr.loc.clone())),
                                    hint: Some(
                                        "Use a signed type if you need to represent negative values.".to_string(),
                                    ),
                                });
                                return None;
                            }

                            Some(basic_concrete_type)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_concrete_type {
                    Some(sema_ty) => Some(SemanticType::PlainType(sema_ty.clone())),
                    None => {
                        let operand_type = format_sema_ty(operand_type, &(self.symbol_formatter)(scope_id_opt));

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::PrefixMinusOnNonInteger { operand_type }),
                            location: Some(DiagLoc::new(prefix_expr.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
        }
    }

    pub(crate) fn analyze_unary_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unary_expr: &mut TypedUnaryExpr,
    ) -> Option<SemanticType> {
        let operand_inner_type = unary_expr.operand.sema_ty.clone();
        let operand_type = match self.analyze_expr(scope_id_opt, &mut unary_expr.operand, operand_inner_type) {
            Some(sema_ty) => sema_ty.const_inner().clone(),
            None => return None,
        };

        if operand_type.is_const() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotAssignToConstLValue),
                location: Some(DiagLoc::new(unary_expr.loc.clone())),
                hint: None,
            });
            return None;
        }

        if !(operand_type.is_integer() && unary_expr.operand.is_lvalue()) {
            let operand_type = format_sema_ty(operand_type, &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidUnary { operand_type }),
                location: Some(DiagLoc::new(unary_expr.loc.clone())),
                hint: None,
            });
            return None;
        }

        Some(operand_type)
    }

    fn analyze_arithmetic_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        self.analyze_binary_expr(scope_id_opt, lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            let valid = (lhs.is_integer() && rhs.is_integer()) || (lhs.is_float() && rhs.is_float());

            if valid {
                if let (SemanticType::PlainType(lhs_basic), SemanticType::PlainType(rhs_basic)) = (lhs, rhs) {
                    PlainType::widen_type(lhs_basic, rhs_basic)
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn analyze_compare_enums(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
    ) -> Option<SemanticType> {
        let enum_symbol_id1 = lhs_type.const_inner().as_enum_symbol_id().unwrap();
        let enum_symbol_id2 = rhs_type.const_inner().as_enum_symbol_id().unwrap();

        let local_or_global_symbol1 = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), enum_symbol_id1)
            .unwrap();

        let local_or_global_symbol2 = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, enum_symbol_id2)
            .unwrap();

        let resolved_enum1 = local_or_global_symbol1.as_enum()?;
        let resolved_enum2 = local_or_global_symbol2.as_enum()?;

        if resolved_enum1.symbol_id == resolved_enum2.symbol_id {
            Some(SemanticType::PlainType(PlainType::Bool))
        } else {
            None
        }
    }

    fn analyze_compare_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        cmp_eq: bool,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let lhs_type = lhs_type.const_inner();
        let rhs_type = rhs_type.const_inner();

        let local_scope_opt =
            scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));

        if let (Some(generic_type1), Some(generic_type2)) = (lhs_type.as_generic_type(), rhs_type.as_generic_type()) {
            let equal_mapping_ctx = mapping_ctx_eq_refcell(
                self.mapping_ctx_arena.clone(),
                &generic_type1.mapping_ctx,
                &generic_type2.mapping_ctx,
            );
            let equal_base = generic_type1.base == generic_type2.base;
            let is_enum = self
                .resolver
                .resolve_enum_symbol(local_scope_opt.clone(), generic_type1.base)
                .is_some();

            if !(is_enum && equal_mapping_ctx && equal_base) {
                let lhs_type_str = format_sema_ty(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
                let rhs_type_str = format_sema_ty(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                        lhs_type: lhs_type_str,
                        rhs_type: rhs_type_str,
                    }),
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
                return None;
            } else {
                return Some(SemanticType::PlainType(PlainType::Bool));
            }
        } else if lhs_type.is_enum() && rhs_type.is_enum() {
            let lhs_type_str = format_sema_ty(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type_str = format_sema_ty(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

            match self.analyze_compare_enums(local_scope_opt, lhs_type.clone(), rhs_type.clone()) {
                Some(sema_ty) => return Some(sema_ty),
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                            lhs_type: lhs_type_str,
                            rhs_type: rhs_type_str,
                        }),
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        } else if !self.check_type_mismatch(scope_id_opt, rhs_type.clone(), lhs_type.clone(), loc.clone()) {
            let lhs_type_str = format_sema_ty(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type_str = format_sema_ty(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                    lhs_type: lhs_type_str,
                    rhs_type: rhs_type_str,
                }),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
            return None;
        }

        self.analyze_binary_expr(scope_id_opt, lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            if (lhs.is_integer() && rhs.is_integer()) || (lhs.is_float() && rhs.is_float()) {
                Some(PlainType::Bool)
            } else if cmp_eq {
                // allow pointer comparisons
                if let (SemanticType::Pointer(_), SemanticType::Pointer(_)) = (&lhs, &rhs) {
                    Some(PlainType::Bool)
                } else if let (SemanticType::Pointer(_), SemanticType::PlainType(PlainType::Null)) = (&lhs, &rhs) {
                    Some(PlainType::Bool)
                } else if let (SemanticType::PlainType(PlainType::Bool), SemanticType::PlainType(PlainType::Bool)) =
                    (&lhs, &rhs)
                {
                    Some(PlainType::Bool)
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn analyze_binary_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: SourceLoc,
        type_checker: impl Fn(&mut Self, SemanticType, SemanticType) -> Option<PlainType>,
    ) -> Option<SemanticType> {
        let lhs_type = lhs_type.const_inner();
        let rhs_type = rhs_type.const_inner();

        if !self.check_type_mismatch(scope_id_opt, rhs_type.clone(), lhs_type.clone(), loc.clone()) {
            let lhs_type_str = format_sema_ty(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type_str = format_sema_ty(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                    lhs_type: lhs_type_str,
                    rhs_type: rhs_type_str,
                }),
                location: Some(DiagLoc::new(
                    loc.clone()
                )),
                hint: Some("Consider adding an explicit cast to either the left-hand or right-hand operand to make their types compatible.".to_string()),
            });
            return None;
        }

        match type_checker(self, lhs_type.clone(), rhs_type.clone()) {
            Some(result_basic) => Some(SemanticType::PlainType(result_basic)),
            None => {
                let lhs_type_str = format_sema_ty(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
                let rhs_type_str = format_sema_ty(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                        lhs_type: lhs_type_str,
                        rhs_type: rhs_type_str,
                    }),
                    location: Some(DiagLoc::new(loc)),
                    hint: None,
                });
                None
            }
        }
    }

    fn analyze_or_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        match (lhs_type.clone(), rhs_type.clone()) {
            (SemanticType::PlainType(PlainType::Null), SemanticType::Pointer(inner_pointer_type)) => {
                Some(SemanticType::Pointer(inner_pointer_type))
            }
            (SemanticType::Pointer(inner_pointer_type), SemanticType::PlainType(PlainType::Null)) => {
                Some(SemanticType::Pointer(inner_pointer_type))
            }
            (SemanticType::Pointer(inner_pointer_type1), SemanticType::Pointer(inner_pointer_type2)) => {
                if *inner_pointer_type1 == *inner_pointer_type2 {
                    Some(SemanticType::Pointer(inner_pointer_type1))
                } else {
                    None
                }
            }
            (
                null_concrete_type @ SemanticType::PlainType(PlainType::Null),
                SemanticType::PlainType(PlainType::Null),
            ) => Some(null_concrete_type),
            _ => self.analyze_binary_expr(scope_id_opt, lhs_type, rhs_type, loc, |_, lhs, rhs| match (lhs, rhs) {
                (SemanticType::PlainType(lhs_basic), SemanticType::PlainType(rhs_basic))
                    if lhs_basic.is_bool() && rhs_basic.is_bool() =>
                {
                    Some(PlainType::Bool)
                }
                _ => None,
            }),
        }
    }

    fn analyze_and_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        self.analyze_binary_expr(scope_id_opt, lhs_type, rhs_type, loc, |_, lhs, rhs| match (lhs, rhs) {
            (SemanticType::PlainType(lhs_basic), SemanticType::PlainType(rhs_basic))
                if lhs_basic.is_bool() && rhs_basic.is_bool() =>
            {
                Some(PlainType::Bool)
            }
            _ => None,
        })
    }

    fn analyze_left_shift_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        self.analyze_binary_expr(scope_id_opt, lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            if let (SemanticType::PlainType(lhs_basic), SemanticType::PlainType(rhs_basic)) = (&lhs, &rhs) {
                if lhs_basic.is_integer() && rhs_basic.is_integer() {
                    Some(PlainType::widen_type(lhs_basic.clone(), rhs_basic.clone())?)
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn analyze_right_shift_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        self.analyze_binary_expr(
            scope_id_opt,
            lhs_type.clone(),
            rhs_type.clone(),
            loc.clone(),
            |this, lhs, rhs| {
                if let (SemanticType::PlainType(lhs_basic), SemanticType::PlainType(rhs_basic)) = (&lhs, &rhs) {
                    // rhs must be unsigned
                    if rhs_basic.is_signed() {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::RhsOfShiftMustBeUnsignedInteger),
                            location: Some(DiagLoc::new(loc.clone())),
                            hint: None,
                        });
                        return None;
                    }

                    if lhs_basic.is_integer() && rhs_basic.is_integer() {
                        Some(PlainType::widen_type(lhs_basic.clone(), rhs_basic.clone())?)
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
        )
    }

    fn analyze_bitwise_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        self.analyze_binary_expr(scope_id_opt, lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            // only allow integer types
            if let (Some(lhs_basic), Some(rhs_basic)) = (lhs.as_basic_type(), rhs.as_basic_type()) {
                if lhs_basic.is_integer() && rhs_basic.is_integer() {
                    return Some(PlainType::widen_type(lhs_basic.clone(), rhs_basic.clone()).unwrap());
                }
            }

            None
        })
    }
}
