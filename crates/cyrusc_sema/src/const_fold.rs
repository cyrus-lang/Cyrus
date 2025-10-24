use crate::context::AnalysisContext;
use ast::{
    LiteralKind,
    operators::{InfixOperator, PrefixOperator},
};
use resolver::symbols::{LocalOrGlobalSymbol, LocalScopeRef};
use tast::{ScopeID, SymbolID, TypedExprStmt, TypedExprKind, TypedLiteralExpr};

impl<'a> AnalysisContext<'a> {
    fn extract_literal_value(&self, typed_literal: &TypedLiteralExpr) -> Option<i64> {
        match &typed_literal.kind {
            LiteralKind::Integer(value, ..) => Some(*value),
            _ => None,
        }
    }

    fn resolve_var_or_global_var_rhs_expr(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> Option<TypedExprStmt> {
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
            .unwrap();

        match match &sym {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => {
                let typed_variable = &local_symbol.as_variable().unwrap().typed_variable;
                let mut typed_expr = typed_variable.rhs.clone().unwrap();
                if let Some(sema_ty) = &typed_variable.ty {
                    typed_expr.sema_ty = Some(sema_ty.clone());
                }
                Some(typed_expr)
            }
            LocalOrGlobalSymbol::GlobalSymbol(global_symbol) => match global_symbol.as_global_var() {
                Some(resolved_global_var) => {
                    let mut typed_expr = resolved_global_var.global_var_sig.rhs.clone().unwrap();
                    if let Some(sema_ty) = &resolved_global_var.global_var_sig.ty {
                        typed_expr.sema_ty = Some(sema_ty.clone());
                    }
                    Some(typed_expr)
                }
                None => None,
            },
        } {
            Some(typed_expr) => Some(typed_expr),
            None => None,
        }
    }

    pub(crate) fn const_expr_as_raw_integer(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &TypedExprStmt,
    ) -> Option<i64> {  
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let integer_result = match &typed_expr.kind {
            TypedExprKind::Symbol(symbol_id, ..) => {
                match self.resolve_var_or_global_var_rhs_expr(local_scope_opt, *symbol_id) {
                    Some(var_rhs_typed_expr) => {
                        if var_rhs_typed_expr.sema_ty.clone().unwrap().is_const() {
                            Some(self.const_expr_as_raw_integer(scope_id_opt, &var_rhs_typed_expr)?)
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            TypedExprKind::Literal(typed_literal) => self.extract_literal_value(typed_literal),
            TypedExprKind::Prefix(typed_prefix_expr) => {
                let integer = self.const_expr_as_raw_integer(scope_id_opt, &typed_prefix_expr.operand)?;

                let result = match &typed_prefix_expr.op {
                    PrefixOperator::Bang => {
                        if integer == 0 {
                            1
                        } else {
                            0
                        }
                    }
                    PrefixOperator::Minus => -integer,
                    PrefixOperator::BitwiseNot => !integer,
                };

                Some(result)
            }
            TypedExprKind::Infix(typed_infix_expr) => {
                let lhs = self.const_expr_as_raw_integer(scope_id_opt, &typed_infix_expr.lhs)?;
                let rhs = self.const_expr_as_raw_integer(scope_id_opt, &typed_infix_expr.rhs)?;

                let result = match &typed_infix_expr.op {
                    InfixOperator::Add => lhs.wrapping_add(rhs),
                    InfixOperator::Sub => lhs.wrapping_sub(rhs),
                    InfixOperator::Mul => lhs.wrapping_mul(rhs),
                    InfixOperator::Div => {
                        if rhs == 0 {
                            return None;
                        }
                        lhs.wrapping_div(rhs)
                    }
                    InfixOperator::Rem => {
                        if rhs == 0 {
                            return None;
                        }
                        lhs.wrapping_rem(rhs)
                    }
                    InfixOperator::LessThan => {
                        if lhs < rhs {
                            1
                        } else {
                            0
                        }
                    }
                    InfixOperator::LessEqual => {
                        if lhs <= rhs {
                            1
                        } else {
                            0
                        }
                    }
                    InfixOperator::GreaterThan => {
                        if lhs > rhs {
                            1
                        } else {
                            0
                        }
                    }
                    InfixOperator::GreaterEqual => {
                        if lhs >= rhs {
                            1
                        } else {
                            0
                        }
                    }
                    InfixOperator::Equal => {
                        if lhs == rhs {
                            1
                        } else {
                            0
                        }
                    }
                    InfixOperator::NotEqual => {
                        if lhs != rhs {
                            1
                        } else {
                            0
                        }
                    }
                    InfixOperator::Or => {
                        if (lhs != 0) || (rhs != 0) {
                            1
                        } else {
                            0
                        }
                    }
                    InfixOperator::And => {
                        if (lhs != 0) && (rhs != 0) {
                            1
                        } else {
                            0
                        }
                    }
                    InfixOperator::BitwiseAnd => lhs & rhs,
                    InfixOperator::BitwiseOr => lhs | rhs,
                    InfixOperator::BitwiseXor => lhs ^ rhs,
                    InfixOperator::BitwiseAndNot => lhs & !rhs,
                    InfixOperator::ShiftRight => lhs.wrapping_shr(rhs as u32),
                    InfixOperator::ShiftLeft => lhs.wrapping_shl(rhs as u32),
                };

                Some(result)
            }
            _ => None,
        };

        integer_result
    }
}
