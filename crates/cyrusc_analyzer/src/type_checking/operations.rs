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
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::symbols::table::SymbolEntryMut;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    exprs::*,
    format::{SymbolFormatterFn, format_sema_type},
    generics::mapping_ctx::mapping_ctx_eq_refcell,
    types::{PlainType, SemanticType},
};

impl<'a, M: SymbolEntryMut> AnalysisContext<'a, M> {
    fn analyze_pointer_arithmetic_type(
        &mut self,
        lhs_type: &SemanticType,
        rhs_type: &SemanticType,
        is_addition: bool,
    ) -> Option<SemanticType> {
        if lhs_type.is_pointer() && rhs_type.is_integer() {
            return Some(lhs_type.clone());
        } else if lhs_type.is_integer() && rhs_type.is_pointer() {
            return Some(rhs_type.clone());
        } else if lhs_type.is_pointer() && rhs_type.is_pointer() && !is_addition {
            return Some(SemanticType::PlainType(PlainType::ISize));
        } else {
            None
        }
    }

    pub(crate) fn analyze_infix_expr_type(
        &mut self,
        infix: &mut TypedInfixExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let lhs_type = match self.analyze_expr(&mut infix.lhs, expected_type.clone()) {
            Some(sema_type) => sema_type.const_inner().clone(),
            None => return None,
        };

        let rhs_type = match self.analyze_expr(&mut infix.rhs, Some(lhs_type.clone())) {
            Some(sema_type) => sema_type.const_inner().clone(),
            None => return None,
        };

        match infix.op {
            InfixOperator::Add => {
                if let Some(sema_type) = self.analyze_pointer_arithmetic_type(&lhs_type, &rhs_type, true) {
                    return Some(sema_type);
                }

                self.analyze_arithmetic_expr(lhs_type, rhs_type, infix.loc)
            }
            InfixOperator::Sub => {
                if let Some(sema_type) = self.analyze_pointer_arithmetic_type(&lhs_type, &rhs_type, false) {
                    return Some(sema_type);
                }

                self.analyze_arithmetic_expr(lhs_type, rhs_type, infix.loc)
            }
            InfixOperator::Mul | InfixOperator::Div | InfixOperator::Rem => {
                self.analyze_arithmetic_expr(lhs_type, rhs_type, infix.loc)
            }
            InfixOperator::LessThan
            | InfixOperator::LessEqual
            | InfixOperator::GreaterThan
            | InfixOperator::GreaterEqual => self.analyze_compare_expr(lhs_type, rhs_type, false, infix.loc),
            InfixOperator::Equal | InfixOperator::NotEqual => {
                self.analyze_compare_expr(lhs_type, rhs_type, true, infix.loc)
            }
            InfixOperator::Or => self.analyze_or_expr(lhs_type, rhs_type, infix.loc),
            InfixOperator::And => self.analyze_and_expr(lhs_type, rhs_type, infix.loc),
            InfixOperator::BitwiseAnd
            | InfixOperator::BitwiseOr
            | InfixOperator::BitwiseXor
            | InfixOperator::BitwiseAndNot => self.analyze_bitwise_expr(lhs_type, rhs_type, infix.loc),
            InfixOperator::ShiftLeft => self.analyze_left_shift_expr(lhs_type, rhs_type, infix.loc),
            InfixOperator::ShiftRight => self.analyze_right_shift_expr(lhs_type, rhs_type, infix.loc),
        }
    }

    pub(crate) fn analyze_addr_of_expr_type(&mut self, addr_of: &mut TypedAddrOfExpr) -> Option<SemanticType> {
        if !addr_of.operand.is_lvalue() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::AddressOfRvalue),
                loc: Some(addr_of.loc),
                hint: None,
            });
            return None;
        }

        let expected_type = addr_of.operand.sema_type.clone();

        let operand_type = match self.analyze_expr(&mut addr_of.operand, expected_type) {
            Some(sema_type) => sema_type.const_inner().clone(),
            None => return None,
        };

        Some(SemanticType::Pointer(Box::new(operand_type)))
    }

    pub(crate) fn analyze_deref_expr_type(&mut self, deref: &mut TypedDerefExpr) -> Option<SemanticType> {
        let expected_type = deref.operand.sema_type.clone();

        let operand_type = match self.analyze_expr(&mut deref.operand, expected_type) {
            Some(sema_type) => sema_type.const_inner().clone(),
            None => return None,
        };

        deref.operand.sema_type = Some(operand_type.clone());

        if (!deref.operand.is_lvalue() || operand_type.as_func_type().is_some()) && !operand_type.is_pointer() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::DerefNonPointerValue),
                loc: Some(deref.loc),
                hint: None,
            });
            return None;
        }

        let inner_type = match operand_type {
            SemanticType::Pointer(sema_type) => *sema_type,
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DerefNonPointerValue),
                    loc: Some(deref.loc),
                    hint: None,
                });
                return None;
            }
        };

        if inner_type.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::DerefVoidPointerValue),
                loc: Some(deref.loc),
                hint: Some("Cast 'void*' to a concrete pointer type before dereferencing it.".to_string()),
            });
            return None;
        }

        Some(inner_type)
    }

    pub(crate) fn analyze_prefix_expr_type(
        &mut self,
        prefix: &mut TypedPrefixExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let operand_type = match self.analyze_expr(&mut prefix.operand, expected_type) {
            Some(sema_type) => sema_type.const_inner().clone(),
            None => return None,
        };

        match prefix.op {
            PrefixOperator::BitwiseNot => {
                let valid_plain_type = match &operand_type {
                    SemanticType::PlainType(plain_type) => {
                        if plain_type.is_integer() {
                            Some(plain_type.clone())
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_plain_type {
                    Some(sema_type) => Some(SemanticType::PlainType(sema_type.clone())),
                    None => {
                        let operand_type = format_sema_type(operand_type, fmt_symbol);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::PrefixMinusOnNonInteger { operand_type }),
                            loc: Some(prefix.loc),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
            PrefixOperator::Bang => {
                let valid_plain_type = match &operand_type {
                    SemanticType::PlainType(plain_type) => {
                        if plain_type.is_bool() {
                            Some(plain_type)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_plain_type {
                    Some(sema_type) => Some(SemanticType::PlainType(sema_type.clone())),
                    None => {
                        let operand_type = format_sema_type(operand_type, fmt_symbol);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::PrefixBangOnNonBool { operand_type }),
                            loc: Some(prefix.loc),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
            PrefixOperator::Minus => {
                let valid_plain_type = match &operand_type {
                    SemanticType::PlainType(plain_type) => {
                        if plain_type.is_integer() {
                            if !plain_type.is_signed() {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::UnaryOperatorMinusOnUnsignedInteger),
                                    loc: Some(prefix.loc),
                                    hint: Some(
                                        "Use a signed type if you need to represent negative values.".to_string(),
                                    ),
                                });
                                return None;
                            }

                            Some(plain_type)
                        } else if plain_type.is_float() {
                            Some(plain_type)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_plain_type {
                    Some(sema_type) => Some(SemanticType::PlainType(sema_type.clone())),
                    None => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::PrefixMinusOnNonInteger {
                                operand_type: format_sema_type(operand_type, fmt_symbol),
                            }),
                            loc: Some(prefix.loc),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
        }
    }

    pub(crate) fn analyze_unary_expr_type(&mut self, unary: &mut TypedUnaryExpr) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let expected_type = unary.operand.sema_type.clone();
        let operand_type = self.analyze_expr(&mut unary.operand, expected_type)?;

        if operand_type.is_const() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotAssignToConstLValue),
                loc: Some(unary.loc),
                hint: None,
            });
            return None;
        }

        if !(operand_type.is_integer() && unary.operand.is_lvalue()) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidUnary {
                    operand_type: format_sema_type(operand_type, fmt_symbol),
                }),
                loc: Some(unary.loc),
                hint: None,
            });
            return None;
        }

        Some(operand_type)
    }

    fn analyze_arithmetic_expr(
        &mut self,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: Loc,
    ) -> Option<SemanticType> {
        self.analyze_binary_expr(lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            let valid = (lhs.is_integer() && rhs.is_integer()) || (lhs.is_float() && rhs.is_float());

            if valid {
                if let (SemanticType::PlainType(lhs_basic), SemanticType::PlainType(rhs_basic)) = (lhs, rhs) {
                    Some(SemanticType::PlainType(
                        PlainType::widen_type(lhs_basic, rhs_basic).unwrap(),
                    ))
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn analyze_compare_enums(&mut self, lhs_type: SemanticType, rhs_type: SemanticType) -> Option<SemanticType> {
        let enum_id1 = lhs_type.const_inner().as_enum_symbol_id().unwrap();
        let enum_id2 = rhs_type.const_inner().as_enum_symbol_id().unwrap();

        let resolved_enum1 = self.query.get_enum(enum_id1)?;
        let resolved_enum2 = self.query.get_enum(enum_id2)?;

        if resolved_enum1.symbol_id == resolved_enum2.symbol_id {
            Some(SemanticType::PlainType(PlainType::Bool))
        } else {
            None
        }
    }

    fn analyze_compare_expr(
        &mut self,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        cmp_eq: bool,
        loc: Loc,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let lhs_type = lhs_type.const_inner();
        let rhs_type = rhs_type.const_inner();

        if let (Some(generic_type1), Some(generic_type2)) = (lhs_type.as_generic_type(), rhs_type.as_generic_type()) {
            let equal_mapping_ctx = mapping_ctx_eq_refcell(
                self.mapping_ctx_arena.clone(),
                &generic_type1.generic_params,
                &generic_type1.mapping_ctx,
                &generic_type2.generic_params,
                &generic_type2.mapping_ctx,
            );

            let equal_base = generic_type1.base == generic_type2.base;
            let is_enum = self.query.get_enum(generic_type1.base).is_some();

            if !(is_enum && equal_mapping_ctx && equal_base) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                        lhs_type: format_sema_type(lhs_type.clone(), fmt_symbol),
                        rhs_type: format_sema_type(rhs_type.clone(), fmt_symbol),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
                return None;
            } else {
                return Some(SemanticType::PlainType(PlainType::Bool));
            }
        } else if lhs_type.is_enum() && rhs_type.is_enum() {
            match self.analyze_compare_enums(lhs_type.clone(), rhs_type.clone()) {
                Some(sema_type) => return Some(sema_type),
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                            lhs_type: format_sema_type(lhs_type.clone(), fmt_symbol),
                            rhs_type: format_sema_type(rhs_type.clone(), fmt_symbol),
                        }),
                        loc: Some(loc),
                        hint: None,
                    });
                    return None;
                }
            }
        } else if !self.check_type_mismatch(rhs_type.clone(), lhs_type.clone(), loc) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                    lhs_type: format_sema_type(lhs_type.clone(), fmt_symbol),
                    rhs_type: format_sema_type(rhs_type.clone(), fmt_symbol),
                }),
                loc: Some(loc),
                hint: None,
            });
            return None;
        }

        self.analyze_binary_expr(lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            if (lhs.is_integer() && rhs.is_integer()) || (lhs.is_float() && rhs.is_float()) {
                Some(SemanticType::PlainType(PlainType::Bool))
            } else if cmp_eq {
                // allow pointer comparisons
                if let (SemanticType::Pointer(_), SemanticType::Pointer(_)) = (&lhs, &rhs) {
                    Some(SemanticType::PlainType(PlainType::Bool))
                } else if let (SemanticType::Pointer(_), SemanticType::PlainType(PlainType::Null)) = (&lhs, &rhs) {
                    Some(SemanticType::PlainType(PlainType::Bool))
                } else if let (SemanticType::PlainType(PlainType::Bool), SemanticType::PlainType(PlainType::Bool)) =
                    (&lhs, &rhs)
                {
                    Some(SemanticType::PlainType(PlainType::Bool))
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

        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: Loc,
        type_checker: impl Fn(&mut Self, SemanticType, SemanticType) -> Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let lhs_type = lhs_type.const_inner();
        let rhs_type = rhs_type.const_inner();

        if !self.check_type_mismatch(rhs_type.clone(), lhs_type.clone(), loc) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                    lhs_type: format_sema_type(lhs_type.clone(), fmt_symbol),
                    rhs_type: format_sema_type(rhs_type.clone(), fmt_symbol),
                }),
                loc: Some(loc),
                hint: Some(
                    "Consider adding an explicit cast to either the lhs or rhs operand to make their types compatible."
                        .to_string(),
                ),
            });
            return None;
        }

        match type_checker(self, lhs_type.clone(), rhs_type.clone()) {
            Some(sema_type) => Some(sema_type),
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                        lhs_type: format_sema_type(lhs_type.clone(), fmt_symbol),
                        rhs_type: format_sema_type(rhs_type.clone(), fmt_symbol),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
                None
            }
        }
    }

    fn analyze_or_expr(&mut self, lhs_type: SemanticType, rhs_type: SemanticType, loc: Loc) -> Option<SemanticType> {
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
            (null_sema_ty @ SemanticType::PlainType(PlainType::Null), SemanticType::PlainType(PlainType::Null)) => {
                Some(null_sema_ty)
            }
            _ => self.analyze_binary_expr(lhs_type, rhs_type, loc, |_, lhs, rhs| match (lhs, rhs) {
                (SemanticType::PlainType(lhs_basic), SemanticType::PlainType(rhs_basic))
                    if lhs_basic.is_bool() && rhs_basic.is_bool() =>
                {
                    Some(SemanticType::PlainType(PlainType::Bool))
                }
                _ => None,
            }),
        }
    }

    fn analyze_and_expr(&mut self, lhs_type: SemanticType, rhs_type: SemanticType, loc: Loc) -> Option<SemanticType> {
        self.analyze_binary_expr(lhs_type, rhs_type, loc, |_, lhs, rhs| match (lhs, rhs) {
            (SemanticType::PlainType(lhs_basic), SemanticType::PlainType(rhs_basic))
                if lhs_basic.is_bool() && rhs_basic.is_bool() =>
            {
                Some(SemanticType::PlainType(PlainType::Bool))
            }
            _ => None,
        })
    }

    fn analyze_left_shift_expr(
        &mut self,
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: Loc,
    ) -> Option<SemanticType> {
        self.analyze_binary_expr(lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            if let (SemanticType::PlainType(lhs_basic), SemanticType::PlainType(rhs_basic)) = (&lhs, &rhs) {
                if lhs_basic.is_integer() && rhs_basic.is_integer() {
                    Some(SemanticType::PlainType(PlainType::widen_type(
                        lhs_basic.clone(),
                        rhs_basic.clone(),
                    )?))
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
        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: Loc,
    ) -> Option<SemanticType> {
        self.analyze_binary_expr(lhs_type.clone(), rhs_type.clone(), loc, |this, lhs, rhs| {
            if let (SemanticType::PlainType(lhs_basic), SemanticType::PlainType(rhs_basic)) = (&lhs, &rhs) {
                // rhs must be unsigned
                if rhs_basic.is_signed() {
                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::RhsOfShiftMustBeUnsignedInteger),
                        loc: Some(loc),
                        hint: None,
                    });
                    return None;
                }

                if lhs_basic.is_integer() && rhs_basic.is_integer() {
                    Some(SemanticType::PlainType(PlainType::widen_type(
                        lhs_basic.clone(),
                        rhs_basic.clone(),
                    )?))
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn analyze_bitwise_expr(
        &mut self,

        lhs_type: SemanticType,
        rhs_type: SemanticType,
        loc: Loc,
    ) -> Option<SemanticType> {
        self.analyze_binary_expr(lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            // only allow integer types
            if let (Some(lhs_basic), Some(rhs_basic)) = (lhs.as_basic_type(), rhs.as_basic_type()) {
                if lhs_basic.is_integer() && rhs_basic.is_integer() {
                    return Some(SemanticType::PlainType(
                        PlainType::widen_type(lhs_basic.clone(), rhs_basic.clone()).unwrap(),
                    ));
                }
            }

            None
        })
    }
}
