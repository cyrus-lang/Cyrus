// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::operators::{InfixOperator, PrefixOperator};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    exprs::*,
    format::format_sema_type,
    types::{PlainType, SemaType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_infix(
        &mut self,
        infix: &mut TypedInfixExpr,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let mut lhs_type = match self.analyze_expr(&mut infix.lhs, expected_type.clone()) {
            Some(ty) => ty.const_inner().clone(),
            None => return None,
        };

        let mut rhs_type = match self.analyze_expr(&mut infix.rhs, Some(lhs_type.clone())) {
            Some(ty) => ty.const_inner().clone(),
            None => return None,
        };

        lhs_type = self.expand_sema_type(lhs_type, infix.loc);
        rhs_type = self.expand_sema_type(rhs_type, infix.loc);

        match infix.op {
            InfixOperator::Add => {
                if let Some(ty) = self.analyze_pointer_arithmetic_type(&lhs_type, &rhs_type, true) {
                    return Some(ty);
                }

                self.analyze_arithmetic_expr(lhs_type, rhs_type, infix.loc)
            }
            InfixOperator::Sub => {
                if let Some(ty) = self.analyze_pointer_arithmetic_type(&lhs_type, &rhs_type, false) {
                    return Some(ty);
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

    pub(crate) fn analyze_addr_of(&mut self, addr_of: &mut TypedAddrOfExpr) -> Option<SemaType> {
        let expected_type = addr_of.operand.ty.clone();

        let operand_type = match self.analyze_expr(&mut addr_of.operand, expected_type) {
            Some(ty) => ty.const_inner().clone(),
            None => return None,
        };

        let is_operand_const = self.is_const_qualified_lvalue(&addr_of.operand);

        if !addr_of.operand.is_lvalue() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::AddressOfRvalue),
                loc: Some(addr_of.loc),
                hint: None,
            });
            return None;
        }

        if is_operand_const {
            Some(SemaType::Pointer(Box::new(operand_type.as_const())))
        } else {
            Some(SemaType::Pointer(Box::new(operand_type)))
        }
    }

    pub(crate) fn analyze_deref(&mut self, deref: &mut TypedDerefExpr) -> Option<SemaType> {
        let expected_type = deref.operand.ty.clone();

        let operand_type = match self.analyze_expr(&mut deref.operand, expected_type) {
            Some(sema_type) => sema_type.const_inner().clone(),
            None => return None,
        };

        deref.operand.ty = Some(operand_type.clone());

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
            SemaType::Pointer(inner) => *inner,
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

    pub(crate) fn analyze_prefix(
        &mut self,
        prefix: &mut TypedPrefixExpr,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let mut operand_type = match self.analyze_expr(&mut prefix.operand, expected_type) {
            Some(sema_type) => sema_type.const_inner().clone(),
            None => return None,
        };

        // expand operand type
        operand_type = self.expand_sema_type(operand_type, prefix.loc);

        match prefix.op {
            PrefixOperator::BitwiseNot => {
                let valid_plain_type = match &operand_type {
                    SemaType::Plain(plain_type) => {
                        if plain_type.is_integer() {
                            Some(plain_type.clone())
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_plain_type {
                    Some(sema_type) => Some(SemaType::Plain(sema_type.clone())),
                    None => {
                        let operand_type = format_sema_type(operand_type, self.formatter);

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
                    SemaType::Plain(plain_type) => {
                        if plain_type.is_integer_or_bool() {
                            Some(plain_type)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_plain_type {
                    Some(plain_type) => Some(SemaType::Plain(plain_type.clone())),
                    None => {
                        let operand_type = format_sema_type(operand_type, self.formatter);

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
                    SemaType::Plain(plain_type) => {
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
                    Some(sema_type) => Some(SemaType::Plain(sema_type.clone())),
                    None => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::PrefixMinusOnNonInteger {
                                operand_type: format_sema_type(operand_type, self.formatter),
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

    pub(crate) fn analyze_unary(&mut self, unary: &mut TypedUnaryExpr) -> Option<SemaType> {
        let expected_type = unary.operand.ty.clone();

        let mut operand_type = self.analyze_expr(&mut unary.operand, expected_type)?;

        if self.is_const_qualified_lvalue(&unary.operand) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotAssignToConstLValue),
                loc: Some(unary.loc),
                hint: None,
            });
            return None;
        }

        if !unary.operand.is_lvalue() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnaryOnTemporary {
                    operand_type: format_sema_type(operand_type.clone(), self.formatter),
                }),
                loc: Some(unary.loc),
                hint: None,
            });
        }

        // expand operand type
        operand_type = self.expand_sema_type(operand_type, unary.loc);

        if !((operand_type.is_integer()) || operand_type.is_pointer()) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidUnary {
                    operand_type: format_sema_type(operand_type, self.formatter),
                }),
                loc: Some(unary.loc),
                hint: None,
            });
            return None;
        }

        Some(operand_type)
    }

    fn analyze_pointer_arithmetic_type(
        &mut self,
        lhs_type: &SemaType,
        rhs_type: &SemaType,
        is_addition: bool,
    ) -> Option<SemaType> {
        if lhs_type.is_pointer() && rhs_type.is_integer() {
            return Some(lhs_type.clone());
        } else if lhs_type.is_integer() && rhs_type.is_pointer() {
            return Some(rhs_type.clone());
        } else if lhs_type.is_pointer() && rhs_type.is_pointer() && !is_addition {
            return Some(SemaType::Plain(PlainType::ISize));
        } else {
            None
        }
    }

    fn analyze_arithmetic_expr(&mut self, lhs_type: SemaType, rhs_type: SemaType, loc: Loc) -> Option<SemaType> {
        self.analyze_binary_expr(lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            let valid = (lhs.is_integer() && rhs.is_integer()) || (lhs.is_float() && rhs.is_float());

            if valid {
                if let (SemaType::Plain(lhs_basic), SemaType::Plain(rhs_basic)) = (lhs, rhs) {
                    Some(SemaType::Plain(PlainType::widen_type(lhs_basic, rhs_basic).unwrap()))
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn analyze_compare_expr(
        &mut self,
        lhs_type: SemaType,
        rhs_type: SemaType,
        cmp_eq: bool,
        loc: Loc,
    ) -> Option<SemaType> {
        let lhs_type = lhs_type.const_inner();
        let rhs_type = rhs_type.const_inner();

        if lhs_type.is_enum() && rhs_type.is_enum() {
            return Some(SemaType::Plain(PlainType::Bool));
        } else if !self.is_assignable_to(rhs_type.clone(), lhs_type.clone(), loc) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                    lhs_type: format_sema_type(lhs_type.clone(), self.formatter),
                    rhs_type: format_sema_type(rhs_type.clone(), self.formatter),
                }),
                loc: Some(loc),
                hint: None,
            });
            return None;
        }

        self.analyze_binary_expr(lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            if (lhs.is_integer() && rhs.is_integer()) || (lhs.is_float() && rhs.is_float()) {
                Some(SemaType::Plain(PlainType::Bool))
            } else if cmp_eq {
                // allow pointer comparisons
                if let (SemaType::Pointer(_), SemaType::Pointer(_)) = (&lhs, &rhs) {
                    Some(SemaType::Plain(PlainType::Bool))
                } else if let (SemaType::Pointer(_), SemaType::Plain(PlainType::Null)) = (&lhs, &rhs) {
                    Some(SemaType::Plain(PlainType::Bool))
                } else if let (SemaType::Plain(PlainType::Bool), SemaType::Plain(PlainType::Bool)) = (&lhs, &rhs) {
                    Some(SemaType::Plain(PlainType::Bool))
                } else if let (SemaType::Plain(_), SemaType::Plain(_)) = (&lhs, &rhs) {
                    Some(SemaType::Plain(PlainType::Bool))
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
        lhs_type: SemaType,
        rhs_type: SemaType,
        loc: Loc,
        type_checker: impl Fn(&mut Self, SemaType, SemaType) -> Option<SemaType>,
    ) -> Option<SemaType> {
        let lhs_type = lhs_type.const_inner();
        let rhs_type = rhs_type.const_inner();

        if !self.is_assignable_to(rhs_type.clone(), lhs_type.clone(), loc) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                    lhs_type: format_sema_type(lhs_type.clone(), self.formatter),
                    rhs_type: format_sema_type(rhs_type.clone(), self.formatter),
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
            Some(ty) => Some(ty),
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidInfix {
                        lhs_type: format_sema_type(lhs_type.clone(), self.formatter),
                        rhs_type: format_sema_type(rhs_type.clone(), self.formatter),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
                None
            }
        }
    }

    fn analyze_or_expr(&mut self, lhs_type: SemaType, rhs_type: SemaType, loc: Loc) -> Option<SemaType> {
        match (lhs_type.clone(), rhs_type.clone()) {
            (SemaType::Plain(PlainType::Null), SemaType::Pointer(inner_pointer_type)) => {
                Some(SemaType::Pointer(inner_pointer_type))
            }
            (SemaType::Pointer(inner_pointer_type), SemaType::Plain(PlainType::Null)) => {
                Some(SemaType::Pointer(inner_pointer_type))
            }
            (SemaType::Pointer(inner_pointer_type1), SemaType::Pointer(inner_pointer_type2)) => {
                if *inner_pointer_type1 == *inner_pointer_type2 {
                    Some(SemaType::Pointer(inner_pointer_type1))
                } else {
                    None
                }
            }
            (null_sema_ty @ SemaType::Plain(PlainType::Null), SemaType::Plain(PlainType::Null)) => Some(null_sema_ty),
            _ => self.analyze_binary_expr(lhs_type, rhs_type, loc, |_, lhs, rhs| match (lhs, rhs) {
                (SemaType::Plain(lhs_basic), SemaType::Plain(rhs_basic))
                    if lhs_basic.is_bool() && rhs_basic.is_bool() =>
                {
                    Some(SemaType::Plain(PlainType::Bool))
                }
                _ => None,
            }),
        }
    }

    fn analyze_and_expr(&mut self, lhs_type: SemaType, rhs_type: SemaType, loc: Loc) -> Option<SemaType> {
        self.analyze_binary_expr(lhs_type, rhs_type, loc, |_, lhs, rhs| match (lhs, rhs) {
            (SemaType::Plain(lhs_basic), SemaType::Plain(rhs_basic)) if lhs_basic.is_bool() && rhs_basic.is_bool() => {
                Some(SemaType::Plain(PlainType::Bool))
            }
            _ => None,
        })
    }

    fn analyze_left_shift_expr(&mut self, lhs_type: SemaType, rhs_type: SemaType, loc: Loc) -> Option<SemaType> {
        self.analyze_binary_expr(lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            if let (SemaType::Plain(lhs_basic), SemaType::Plain(rhs_basic)) = (&lhs, &rhs) {
                if lhs_basic.is_integer() && rhs_basic.is_integer() {
                    Some(SemaType::Plain(PlainType::widen_type(
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

    fn analyze_right_shift_expr(&mut self, lhs_type: SemaType, rhs_type: SemaType, loc: Loc) -> Option<SemaType> {
        self.analyze_binary_expr(lhs_type.clone(), rhs_type.clone(), loc, |this, lhs, rhs| {
            if let (SemaType::Plain(plain_type1), SemaType::Plain(plain_type2)) = (&lhs, &rhs) {
                // rhs must be unsigned
                if plain_type2.is_signed() {
                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::RhsOfShiftMustBeUnsignedInteger),
                        loc: Some(loc),
                        hint: None,
                    });
                    return None;
                }

                if plain_type1.is_integer() && plain_type2.is_integer() {
                    Some(SemaType::Plain(PlainType::widen_type(
                        plain_type1.clone(),
                        plain_type2.clone(),
                    )?))
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn analyze_bitwise_expr(&mut self, lhs_type: SemaType, rhs_type: SemaType, loc: Loc) -> Option<SemaType> {
        self.analyze_binary_expr(lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            // only allow integer types
            if let (Some(plain_type1), Some(plain_type2)) = (lhs.as_plain_type(), rhs.as_plain_type()) {
                if plain_type1.is_integer() && plain_type2.is_integer() {
                    return Some(SemaType::Plain(
                        PlainType::widen_type(plain_type1.clone(), plain_type2.clone()).unwrap(),
                    ));
                }
            }

            None
        })
    }
}
