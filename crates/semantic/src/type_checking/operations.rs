use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use ast::{
    operators::{InfixOperator, PrefixOperator},
    source_loc::SourceLoc,
};
use diagcentral::{Diag, DiagLevel, DiagLoc};
use resolver::scope::LocalScopeRef;
use typed_ast::{
    ScopeID, TypedAddressOf, TypedDereference, TypedExpressionKind, TypedInfixExpression, TypedPrefixExpression,
    TypedSizeOfExpression, TypedUnaryExpression,
    format::format_concrete_type,
    types::{BasicSemanticType, SemanticType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_infix_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        infix_expr: &mut TypedInfixExpression,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let lhs_type = match self.analyze_typed_expr_type(scope_id_opt, &mut infix_expr.lhs, expected_type.clone()) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let rhs_type = match self.analyze_typed_expr_type(scope_id_opt, &mut infix_expr.rhs, Some(lhs_type.clone())) {
            Some(concrete_type) => concrete_type,
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

    pub(crate) fn analyze_address_of_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        address_of: &mut TypedAddressOf,
    ) -> Option<SemanticType> {
        if !address_of.operand.is_lvalue() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::AddressOfRvalue,
                location: Some(DiagLoc::new(address_of.loc.clone())),
                hint: None,
            });
            return None;
        }

        let operand_inner_type = address_of.operand.concrete_type.clone();
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut address_of.operand, operand_inner_type)
        {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        Some(SemanticType::Pointer(Box::new(operand_type)))
    }

    pub(crate) fn analyze_dereference_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        dereference: &mut TypedDereference,
    ) -> Option<SemanticType> {
        let operand_inner_type = dereference.operand.concrete_type.clone();
        let operand_type =
            match self.analyze_typed_expr_type(scope_id_opt, &mut dereference.operand, operand_inner_type) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        dereference.operand.concrete_type = Some(operand_type.clone());

        if !dereference.operand.is_lvalue() || operand_type.as_func_type().is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::DerefNonPointerValue,
                location: Some(DiagLoc::new(dereference.loc.clone())),
                hint: None,
            });
            return None;
        }

        let pointer_inner_type = match operand_type {
            SemanticType::Pointer(concrete_type) => *concrete_type,
            _ => unreachable!(),
        };

        if pointer_inner_type.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::DerefVoidPointerValue,
                location: Some(DiagLoc::new(dereference.loc.clone())),
                hint: Some("Cast 'void*' to a concrete pointer type before dereferencing it.".to_string()),
            });
            return None;
        }

        Some(pointer_inner_type)
    }

    pub(crate) fn analyze_sizeof_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        sizeof_expr: &mut TypedSizeOfExpression,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let symbol_id = match &sizeof_expr.expr.kind {
            TypedExpressionKind::SemanticType(concrete_type) => {
                if let SemanticType::UnresolvedSymbol(symbol_id) = concrete_type {
                    *symbol_id
                } else {
                    self.normalize_type(scope_id_opt, concrete_type.clone(), sizeof_expr.loc.clone())?;
                    return Some(SemanticType::BasicType(BasicSemanticType::SizeT));
                }
            }
            TypedExpressionKind::Symbol(symbol_id, ..) => *symbol_id,
            _ => {
                self.analyze_typed_expr_type(scope_id_opt, &mut sizeof_expr.expr, expected_type);
                return Some(SemanticType::BasicType(BasicSemanticType::SizeT));
            }
        };

        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .unwrap();

        if local_or_global_symbol.as_global_var().is_some() || local_or_global_symbol.as_variable().is_some() {
            // consider as expr
            self.analyze_typed_expr_type(scope_id_opt, &mut sizeof_expr.expr, expected_type);
        } else {
            // consider as type
            self.normalize_type(
                scope_id_opt,
                SemanticType::UnresolvedSymbol(symbol_id),
                sizeof_expr.loc.clone(),
            )?;
        }

        Some(SemanticType::BasicType(BasicSemanticType::SizeT))
    }

    pub(crate) fn analyze_prefix_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        prefix_expr: &mut TypedPrefixExpression,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut prefix_expr.operand, expected_type) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        match prefix_expr.op {
            PrefixOperator::BitwiseNot => {
                let valid_concrete_type = match &operand_type {
                    SemanticType::BasicType(basic_concrete_type) => {
                        if basic_concrete_type.is_integer() {
                            Some(basic_concrete_type.clone())
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_concrete_type {
                    Some(concrete_type) => Some(SemanticType::BasicType(concrete_type.clone())),
                    None => {
                        let operand_type = format_concrete_type(operand_type, &(self.symbol_formatter)(scope_id_opt));

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::PrefixMinusOnNonInteger { operand_type },
                            location: Some(DiagLoc::new(prefix_expr.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
            PrefixOperator::Bang => {
                let valid_concrete_type = match &operand_type {
                    SemanticType::BasicType(basic_concrete_type) => {
                        if basic_concrete_type.is_bool() {
                            Some(basic_concrete_type)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_concrete_type {
                    Some(concrete_type) => Some(SemanticType::BasicType(concrete_type.clone())),
                    None => {
                        let operand_type = format_concrete_type(operand_type, &(self.symbol_formatter)(scope_id_opt));

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::PrefixBangOnNonBool { operand_type },
                            location: Some(DiagLoc::new(prefix_expr.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
            PrefixOperator::Minus => {
                let valid_concrete_type = match &operand_type {
                    SemanticType::BasicType(basic_concrete_type) => {
                        if basic_concrete_type.is_integer() || basic_concrete_type.is_float() {
                            Some(basic_concrete_type)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_concrete_type {
                    Some(concrete_type) => Some(SemanticType::BasicType(concrete_type.clone())),
                    None => {
                        let operand_type = format_concrete_type(operand_type, &(self.symbol_formatter)(scope_id_opt));

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::PrefixMinusOnNonInteger { operand_type },
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
        unary_expr: &mut TypedUnaryExpression,
    ) -> Option<SemanticType> {
        let operand_inner_type = unary_expr.operand.concrete_type.clone();
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut unary_expr.operand, operand_inner_type)
        {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        if operand_type.is_const() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::CannotAssignToConstLValue,
                location: Some(DiagLoc::new(unary_expr.loc.clone())),
                hint: None,
            });
            return None;
        }

        if !operand_type.is_integer() {
            let operand_type = format_concrete_type(operand_type, &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidUnary { operand_type },
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
                if let (SemanticType::BasicType(lhs_basic), SemanticType::BasicType(rhs_basic)) = (lhs, rhs) {
                    BasicSemanticType::bigger_type(lhs_basic, rhs_basic)
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
        let enum_symbol_id1 = lhs_type.as_enum_symbol_id().unwrap();
        let enum_symbol_id2 = rhs_type.as_enum_symbol_id().unwrap();

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
            Some(SemanticType::BasicType(BasicSemanticType::Bool))
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
        let lhs_type = lhs_type.get_const_inner();
        let rhs_type = rhs_type.get_const_inner();

        if lhs_type.is_enum() && rhs_type.is_enum() {
            let lhs_type_str = format_concrete_type(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type_str = format_concrete_type(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

            let local_scope_opt =
                scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

            match self.analyze_compare_enums(local_scope_opt, lhs_type.clone(), rhs_type.clone()) {
                Some(concrete_type) => return Some(concrete_type),
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::InvalidInfix {
                            lhs_type: lhs_type_str,
                            rhs_type: rhs_type_str,
                        },
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        } else if !self.check_type_mismatch(scope_id_opt, rhs_type.clone(), lhs_type.clone(), loc.clone()) {
            let lhs_type_str = format_concrete_type(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type_str = format_concrete_type(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidInfix {
                    lhs_type: lhs_type_str,
                    rhs_type: rhs_type_str,
                },
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
            return None;
        }

        self.analyze_binary_expr(scope_id_opt, lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            if (lhs.is_integer() && rhs.is_integer()) || (lhs.is_float() && rhs.is_float()) {
                Some(BasicSemanticType::Bool)
            } else if cmp_eq {
                // allow pointer comparisons
                if let (SemanticType::Pointer(_), SemanticType::Pointer(_)) = (&lhs, &rhs) {
                    Some(BasicSemanticType::Bool)
                } else if let (SemanticType::Pointer(_), SemanticType::BasicType(BasicSemanticType::Null)) =
                    (&lhs, &rhs)
                {
                    Some(BasicSemanticType::Bool)
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
        type_checker: impl Fn(&mut Self, SemanticType, SemanticType) -> Option<BasicSemanticType>,
    ) -> Option<SemanticType> {
        let lhs_type = lhs_type.get_const_inner();
        let rhs_type = rhs_type.get_const_inner();

        if !self.check_type_mismatch(scope_id_opt, rhs_type.clone(), lhs_type.clone(), loc.clone()) {
            let lhs_type_str = format_concrete_type(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type_str = format_concrete_type(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidInfix {
                    lhs_type: lhs_type_str,
                    rhs_type: rhs_type_str,
                },
                location: Some(DiagLoc::new(
                    loc.clone()
                )),
                hint: Some("Consider adding an explicit cast to either the left-hand or right-hand operand to make their types compatible.".to_string()),
            });
            return None;
        }

        match type_checker(self, lhs_type.clone(), rhs_type.clone()) {
            Some(result_basic) => Some(SemanticType::BasicType(result_basic)),
            None => {
                let lhs_type_str = format_concrete_type(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
                let rhs_type_str = format_concrete_type(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidInfix {
                        lhs_type: lhs_type_str,
                        rhs_type: rhs_type_str,
                    },
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
            (SemanticType::BasicType(BasicSemanticType::Null), SemanticType::Pointer(inner_pointer_type)) => {
                Some(SemanticType::Pointer(inner_pointer_type))
            }
            (SemanticType::Pointer(inner_pointer_type), SemanticType::BasicType(BasicSemanticType::Null)) => {
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
                null_concrete_type @ SemanticType::BasicType(BasicSemanticType::Null),
                SemanticType::BasicType(BasicSemanticType::Null),
            ) => Some(null_concrete_type),
            _ => self.analyze_binary_expr(scope_id_opt, lhs_type, rhs_type, loc, |_, lhs, rhs| match (lhs, rhs) {
                (SemanticType::BasicType(lhs_basic), SemanticType::BasicType(rhs_basic))
                    if lhs_basic.is_bool() && rhs_basic.is_bool() =>
                {
                    Some(BasicSemanticType::Bool)
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
            (SemanticType::BasicType(lhs_basic), SemanticType::BasicType(rhs_basic))
                if lhs_basic.is_bool() && rhs_basic.is_bool() =>
            {
                Some(BasicSemanticType::Bool)
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
            if let (SemanticType::BasicType(lhs_basic), SemanticType::BasicType(rhs_basic)) = (&lhs, &rhs) {
                if lhs_basic.is_integer() && rhs_basic.is_integer() {
                    Some(BasicSemanticType::bigger_type(lhs_basic.clone(), rhs_basic.clone())?)
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
                if let (SemanticType::BasicType(lhs_basic), SemanticType::BasicType(rhs_basic)) = (&lhs, &rhs) {
                    // rhs must be unsigned
                    if rhs_basic.is_signed() {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::RhsOfShiftMustBeUnsignedInteger,
                            location: Some(DiagLoc::new(loc.clone())),
                            hint: None,
                        });
                        return None;
                    }

                    if lhs_basic.is_integer() && rhs_basic.is_integer() {
                        Some(BasicSemanticType::bigger_type(lhs_basic.clone(), rhs_basic.clone())?)
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
                    return Some(BasicSemanticType::bigger_type(lhs_basic.clone(), rhs_basic.clone()).unwrap());
                }
            }

            None
        })
    }
}
