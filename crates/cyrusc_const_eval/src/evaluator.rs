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

use crate::diagnostics::ConstEvalError;
use crate::resolver::ConstResolver;
use crate::value::ConstValue;
use cyrusc_ast::operators::{InfixOperator, PrefixOperator};
use cyrusc_tokens::literals::LiteralKind;
use cyrusc_typed_ast::SymbolID;
use cyrusc_typed_ast::exprs::*;
use cyrusc_typed_ast::types::SemanticType;
use std::collections::HashMap;

pub struct ConstEvaluator<'a, R: ConstResolver> {
    pub resolver: &'a mut R,
    cache: HashMap<SymbolID, ConstValue>,
    evaluating: HashMap<SymbolID, ()>,
}

impl<'a, R: ConstResolver> ConstEvaluator<'a, R> {
    pub fn new(resolver: &'a mut R) -> Self {
        Self {
            resolver,
            cache: HashMap::new(),
            evaluating: HashMap::new(),
        }
    }

    pub fn eval_expr(&mut self, expr: &TypedExprStmt) -> Result<ConstValue, ConstEvalError> {
        let raw = match &expr.kind {
            TypedExprKind::Symbol(symbol) => self.eval_symbol(symbol.symbol_id),
            TypedExprKind::Literal(lit) => self.eval_literal(lit),
            TypedExprKind::Prefix(prefix) => self.eval_prefix(prefix),
            TypedExprKind::Infix(infix) => self.eval_infix(infix),
            _ => Err(ConstEvalError::UnsupportedExpr),
        }?;

        self.coerce_to_expected_type(raw, &expr.sema_ty.as_ref().unwrap())
    }

    fn eval_literal(&self, literal: &TypedLiteralExpr) -> Result<ConstValue, ConstEvalError> {
        match &literal.kind {
            LiteralKind::Integer(int_value, None) => Ok(ConstValue::Int(*int_value)),
            LiteralKind::Bool(bool_value) => Ok(ConstValue::Bool(*bool_value)),
            LiteralKind::Float(float_value, None) => Ok(ConstValue::Float(*float_value)),
            _ => Err(ConstEvalError::UnsupportedExpr),
        }
    }

    fn eval_symbol(&mut self, symbol_id: SymbolID) -> Result<ConstValue, ConstEvalError> {
        if let Some(const_value) = self.cache.get(&symbol_id) {
            return Ok(const_value.clone());
        }

        if self.evaluating.contains_key(&symbol_id) {
            return Err(ConstEvalError::CyclicConst(symbol_id));
        }

        if !self.resolver.symbol_is_const(symbol_id) {
            return Err(ConstEvalError::NonConstSymbol(symbol_id));
        }

        self.evaluating.insert(symbol_id, ());

        let expr = self
            .resolver
            .resolve_symbol_expr(symbol_id)
            .ok_or(ConstEvalError::UnsupportedExpr)?;

        let const_value = self.eval_expr(&expr)?;

        self.cache.insert(symbol_id, const_value.clone());
        self.evaluating.remove(&symbol_id);

        Ok(const_value)
    }

    fn eval_prefix(&mut self, expr: &TypedPrefixExpr) -> Result<ConstValue, ConstEvalError> {
        let const_value = self.eval_expr(&expr.operand)?;

        match expr.op {
            PrefixOperator::Minus => match const_value {
                ConstValue::Int(v) => Ok(ConstValue::Int(-v)),
                ConstValue::Float(v) => Ok(ConstValue::Float(-v)),
                _ => Err(ConstEvalError::TypeError),
            },
            PrefixOperator::Bang => {
                let v = const_value.as_int().ok_or(ConstEvalError::TypeError)?;
                Ok(ConstValue::Bool(v == 0))
            }
            PrefixOperator::BitwiseNot => {
                let v = const_value.as_int().ok_or(ConstEvalError::TypeError)?;
                Ok(ConstValue::Int(!v))
            }
        }
    }

    fn eval_infix(&mut self, expr: &TypedInfixExpr) -> Result<ConstValue, ConstEvalError> {
        let lhs = self.eval_expr(&expr.lhs)?;
        let rhs = self.eval_expr(&expr.rhs)?;

        if let (Some(lhs), Some(rhs)) = (lhs.as_float(), rhs.as_float()) {
            let result = match expr.op {
                InfixOperator::Add => ConstValue::Float(lhs + rhs),
                InfixOperator::Sub => ConstValue::Float(lhs - rhs),
                InfixOperator::Mul => ConstValue::Float(lhs * rhs),
                InfixOperator::Div => ConstValue::Float(lhs / rhs),
                InfixOperator::Rem => ConstValue::Float(lhs % rhs),
                InfixOperator::LessThan => ConstValue::Bool(lhs < rhs),
                InfixOperator::LessEqual => ConstValue::Bool(lhs <= rhs),
                InfixOperator::GreaterThan => ConstValue::Bool(lhs > rhs),
                InfixOperator::GreaterEqual => ConstValue::Bool(lhs >= rhs),
                InfixOperator::Equal => ConstValue::Bool(lhs == rhs),
                InfixOperator::NotEqual => ConstValue::Bool(lhs != rhs),

                _ => return Err(ConstEvalError::TypeError),
            };

            return Ok(result);
        }

        if let (Some(lhs), Some(rhs)) = (lhs.as_int(), rhs.as_int()) {
            let result = match expr.op {
                InfixOperator::Add => lhs.wrapping_add(rhs),
                InfixOperator::Sub => lhs.wrapping_sub(rhs),
                InfixOperator::Mul => lhs.wrapping_mul(rhs),

                InfixOperator::Div => {
                    if rhs == 0 {
                        return Err(ConstEvalError::DivisionByZero);
                    }
                    lhs.wrapping_div(rhs)
                }

                InfixOperator::Rem => {
                    if rhs == 0 {
                        return Err(ConstEvalError::DivisionByZero);
                    }
                    lhs.wrapping_rem(rhs)
                }

                InfixOperator::BitwiseAnd => lhs & rhs,
                InfixOperator::BitwiseOr => lhs | rhs,
                InfixOperator::BitwiseXor => lhs ^ rhs,
                InfixOperator::BitwiseAndNot => lhs & !rhs,
                InfixOperator::ShiftRight => lhs.wrapping_shr(rhs as u32),
                InfixOperator::ShiftLeft => lhs.wrapping_shl(rhs as u32),
                InfixOperator::LessThan => return Ok(ConstValue::Bool(lhs < rhs)),
                InfixOperator::LessEqual => return Ok(ConstValue::Bool(lhs <= rhs)),
                InfixOperator::GreaterThan => return Ok(ConstValue::Bool(lhs > rhs)),
                InfixOperator::GreaterEqual => return Ok(ConstValue::Bool(lhs >= rhs)),
                InfixOperator::Equal => return Ok(ConstValue::Bool(lhs == rhs)),
                InfixOperator::NotEqual => return Ok(ConstValue::Bool(lhs != rhs)),
                InfixOperator::Or => return Ok(ConstValue::Bool(lhs != 0 || rhs != 0)),
                InfixOperator::And => return Ok(ConstValue::Bool(lhs != 0 && rhs != 0)),
            };

            return Ok(ConstValue::Int(result));
        }

        Err(ConstEvalError::TypeError)
    }

    fn coerce_to_expected_type(
        &self,
        value: ConstValue,
        expected_type: &SemanticType,
    ) -> Result<ConstValue, ConstEvalError> {
        if expected_type.is_integer() {
            return match value {
                ConstValue::Int(value) => Ok(ConstValue::Int(value)),
                ConstValue::Bool(value) => Ok(ConstValue::Int(if value { 1 } else { 0 })),
                ConstValue::Float(value) => Ok(ConstValue::Int(value as i128)),
            };
        }

        if expected_type.is_bool() {
            return match value {
                ConstValue::Bool(value) => Ok(ConstValue::Bool(value)),
                ConstValue::Int(value) => Ok(ConstValue::Bool(value != 0)),
                ConstValue::Float(value) => Ok(ConstValue::Bool(value != 0.0)),
            };
        }

        if expected_type.is_float() {
            return match value {
                ConstValue::Float(value) => Ok(ConstValue::Float(value)),
                ConstValue::Int(value) => Ok(ConstValue::Float(value as f64)),
                ConstValue::Bool(value) => Ok(ConstValue::Float(if value { 1.0 } else { 0.0 })),
            };
        }

        Ok(value)
    }
}
