// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::diagnostics::ConstEvalError;
use crate::resolver::ConstResolver;
use crate::value::ConstValue;
use cyrusc_ast::operators::{InfixOperator, PrefixOperator};
use cyrusc_internal::abi::target::ABITarget;
use cyrusc_internal::analyzer_state::AnalyzerState;
use cyrusc_internal::cir::lower::lower_sema_type;
use cyrusc_internal::cir::typectx::CIRTypeContext;
use cyrusc_tokens::literals::LiteralKind;
use cyrusc_typed_ast::builtins::{
    TypedBuiltin, TypedBuiltinFunc, TypedBuiltinKind, TypedBuiltinPhase, builtin_spec_of, lookup_builtin,
};
use cyrusc_typed_ast::decls::DeclID;
use cyrusc_typed_ast::decls::table::DeclTablesRegistry;
use cyrusc_typed_ast::exprs::*;
use cyrusc_typed_ast::types::SemaType;
use std::collections::HashMap;
use std::sync::Arc;

pub struct ConstEvaluator<'a, R: ConstResolver> {
    pub resolver: &'a R,
    cache: HashMap<DeclID, ConstValue>,
    evaluating: HashMap<DeclID, ()>,
    decl_tables: &'a DeclTablesRegistry,
    target: &'a ABITarget,
    tctx: Arc<CIRTypeContext>,
    analyzer_state: &'a dyn AnalyzerState,
}

impl<'a, R: ConstResolver> ConstEvaluator<'a, R> {
    pub fn new(
        resolver: &'a R,
        decl_tables: &'a DeclTablesRegistry,
        target: &'a ABITarget,
        tctx: Arc<CIRTypeContext>,
        analyzer_state: &'a dyn AnalyzerState,
    ) -> Self {
        Self {
            resolver,
            cache: HashMap::new(),
            evaluating: HashMap::new(),
            decl_tables,
            target,
            tctx,
            analyzer_state,
        }
    }

    pub fn eval_expr(
        &mut self,
        expr: &TypedExpr,
        analyzer_state: &'a dyn AnalyzerState,
    ) -> Result<ConstValue, ConstEvalError> {
        let const_value = match &expr.kind {
            TypedExprKind::Symbol(symbol_expr) => {
                let decl_id = symbol_expr.as_decl_id().unwrap();

                if let Some(global_var_decl_id) = decl_id.as_global_var() {
                    let global_var = self.decl_tables.global_var_decl(global_var_decl_id);

                    // const-eval in analyzer layer is only
                    // valid for `const and non-static global vars`
                    if !global_var.is_static {
                        return self.eval_symbol(decl_id);
                    }
                }

                return Err(ConstEvalError::UnsupportedExpr);
            }
            TypedExprKind::Literal(lit) => self.eval_literal(lit),
            TypedExprKind::Prefix(prefix) => self.eval_prefix(prefix),
            TypedExprKind::Infix(infix) => self.eval_infix(infix),

            TypedExprKind::Builtin(builtin) => self.eval_builtin(builtin, analyzer_state),

            _ => Err(ConstEvalError::UnsupportedExpr),
        }?;

        self.coerce_to_expected_type(const_value, &expr.ty.as_ref().unwrap())
    }

    fn eval_literal(&self, literal: &TypedLiteralExpr) -> Result<ConstValue, ConstEvalError> {
        match &literal.kind {
            LiteralKind::Integer(int_value, _) => Ok(ConstValue::Int(int_value.as_int())),
            LiteralKind::Float(float_value, _) => Ok(ConstValue::Float(*float_value)),
            LiteralKind::Bool(bool_value) => Ok(ConstValue::Bool(*bool_value)),

            _ => Err(ConstEvalError::UnsupportedExpr),
        }
    }

    fn eval_symbol(&mut self, decl_id: DeclID) -> Result<ConstValue, ConstEvalError> {
        if let Some(const_value) = self.cache.get(&decl_id) {
            return Ok(const_value.clone());
        }

        if self.evaluating.contains_key(&decl_id) {
            return Err(ConstEvalError::CyclicConst(decl_id));
        }

        if !self.resolver.is_decl_const(decl_id) {
            return Err(ConstEvalError::NonConstSymbol(decl_id));
        }

        self.evaluating.insert(decl_id, ());

        let expr = self
            .resolver
            .get_var_rhs_expr(decl_id)
            .ok_or(ConstEvalError::UnsupportedExpr)?;

        let const_value = self.eval_expr(&expr, self.analyzer_state)?;

        self.cache.insert(decl_id, const_value.clone());
        self.evaluating.remove(&decl_id);

        Ok(const_value)
    }

    fn eval_prefix(&mut self, expr: &TypedPrefixExpr) -> Result<ConstValue, ConstEvalError> {
        let const_value = self.eval_expr(&expr.operand, self.analyzer_state)?;

        match expr.op {
            PrefixOperator::Minus => match const_value {
                ConstValue::Int(value) => {
                    if value == i128::MIN {
                        Ok(ConstValue::Int(i128::MIN))
                    } else {
                        match value.checked_neg() {
                            Some(negated) => Ok(ConstValue::Int(negated)),
                            None => Err(ConstEvalError::UnsupportedExpr),
                        }
                    }
                }
                ConstValue::Float(value) => Ok(ConstValue::Float(-value)),

                _ => Err(ConstEvalError::TypeError),
            },
            PrefixOperator::Bang => {
                let value = const_value.as_int().ok_or(ConstEvalError::TypeError)?;
                Ok(ConstValue::Bool(value == 0))
            }
            PrefixOperator::BitwiseNot => {
                let value = const_value.as_int().ok_or(ConstEvalError::TypeError)?;
                Ok(ConstValue::Int(!value))
            }
        }
    }

    fn eval_infix(&mut self, expr: &TypedInfixExpr) -> Result<ConstValue, ConstEvalError> {
        let lhs = self.eval_expr(&expr.lhs, self.analyzer_state)?;
        let rhs = self.eval_expr(&expr.rhs, self.analyzer_state)?;

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

    pub fn coerce_to_expected_type(
        &self,
        value: ConstValue,
        expected_type: &SemaType,
    ) -> Result<ConstValue, ConstEvalError> {
        if expected_type.is_integer() {
            return match value {
                ConstValue::Int(value) => Ok(ConstValue::Int(value)),
                ConstValue::Bool(value) => Ok(ConstValue::Int(if value { 1 } else { 0 })),
                ConstValue::Float(value) => Ok(ConstValue::Int(value as i128)),
                ConstValue::String(_) => return Err(ConstEvalError::TypeError),
                ConstValue::Type(_) => return Err(ConstEvalError::TypeError),
            };
        }

        if expected_type.is_bool() {
            return match value {
                ConstValue::Bool(value) => Ok(ConstValue::Bool(value)),
                ConstValue::Int(value) => Ok(ConstValue::Bool(value != 0)),
                ConstValue::Float(value) => Ok(ConstValue::Bool(value != 0.0)),
                ConstValue::String(_) => return Err(ConstEvalError::TypeError),
                ConstValue::Type(_) => return Err(ConstEvalError::TypeError),
            };
        }

        if expected_type.is_float() {
            return match value {
                ConstValue::Float(value) => Ok(ConstValue::Float(value)),
                ConstValue::Int(value) => Ok(ConstValue::Float(value as f64)),
                ConstValue::Bool(value) => Ok(ConstValue::Float(if value { 1.0 } else { 0.0 })),
                ConstValue::String(_) => return Err(ConstEvalError::TypeError),
                ConstValue::Type(_) => return Err(ConstEvalError::TypeError),
            };
        }

        Ok(value)
    }
}

impl<'a, R: ConstResolver> ConstEvaluator<'a, R> {
    pub fn eval_builtin(
        &self,
        builtin: &TypedBuiltin,
        analyzer_state: &'a dyn AnalyzerState,
    ) -> Result<ConstValue, ConstEvalError> {
        let Some(builtin_func) = builtin.as_builtin_func() else {
            return Err(ConstEvalError::UnsupportedExpr);
        };

        let Some(builtin_kind) = lookup_builtin(&builtin_func.name.value) else {
            return Err(ConstEvalError::UnsupportedExpr);
        };

        let builtin_spec = builtin_spec_of(builtin_kind);

        // SPECIAL CASE:
        if builtin_spec.kind == TypedBuiltinKind::Cast {
            return self.eval_cast(builtin_func);
        }

        if builtin_spec.phase != TypedBuiltinPhase::ConstEval {
            return Err(ConstEvalError::UnsupportedExpr);
        }

        match builtin_spec.kind {
            TypedBuiltinKind::FuncName => self.eval_func_name(builtin_func, analyzer_state),
            TypedBuiltinKind::MethodName => self.eval_method_name(builtin_func, analyzer_state),
            TypedBuiltinKind::ModuleName => self.eval_module_name(builtin_func, analyzer_state),
            TypedBuiltinKind::FileName => self.eval_file_name(builtin_func, analyzer_state),
            TypedBuiltinKind::Line => self.eval_line(builtin_func),
            TypedBuiltinKind::Column => self.eval_column(builtin_func),
            TypedBuiltinKind::SizeOf => self.eval_sizeof(builtin_func),
            TypedBuiltinKind::AlignOf => self.eval_alignof(builtin_func),
            TypedBuiltinKind::OffsetOf => self.eval_offsetof(builtin_func),
            TypedBuiltinKind::TypeOf => self.eval_typeof(builtin_func),

            _ => return Err(ConstEvalError::UnsupportedExpr),
        }
    }

    fn eval_cast(&self, builtin_func: &TypedBuiltinFunc) -> Result<ConstValue, ConstEvalError> {
        let target_type = builtin_func.args.get(0).unwrap().kind.as_type_expr().unwrap();
        let operand = builtin_func.args.get(1).unwrap();

        if let Some(literal) = operand.kind.as_literal() {
            match &literal.kind {
                LiteralKind::Integer(value, suffix_opt) => {
                    if suffix_opt.is_none() {
                        return Ok(ConstValue::Int(value.as_int()));
                    }
                }
                LiteralKind::Float(value, suffix_opt) => {
                    if suffix_opt.is_none() {
                        return Ok(ConstValue::Float(*value));
                    }
                }
                LiteralKind::Char(value) => {
                    if target_type.is_integer() {
                        return Ok(ConstValue::Int(*value as i128));
                    }
                }
                LiteralKind::Bool(_) | LiteralKind::String(_, _) | LiteralKind::Null => {}
            }
        }

        Err(ConstEvalError::UnsupportedExpr)
    }

    fn eval_func_name(
        &self,
        _builtin_func: &TypedBuiltinFunc,
        analyzer_state: &'a dyn AnalyzerState,
    ) -> Result<ConstValue, ConstEvalError> {
        Ok(ConstValue::String(analyzer_state.func_name().to_string()))
    }

    fn eval_method_name(
        &self,
        _builtin_func: &TypedBuiltinFunc,
        analyzer_state: &'a dyn AnalyzerState,
    ) -> Result<ConstValue, ConstEvalError> {
        Ok(ConstValue::String(analyzer_state.method_name().to_string()))
    }

    fn eval_module_name(
        &self,
        _builtin_func: &TypedBuiltinFunc,
        analyzer_state: &'a dyn AnalyzerState,
    ) -> Result<ConstValue, ConstEvalError> {
        Ok(ConstValue::String(analyzer_state.module_name().to_string()))
    }

    fn eval_file_name(
        &self,
        _builtin_func: &TypedBuiltinFunc,
        analyzer_state: &'a dyn AnalyzerState,
    ) -> Result<ConstValue, ConstEvalError> {
        Ok(ConstValue::String(analyzer_state.file_name().to_string()))
    }

    fn eval_line(&self, builtin_func: &TypedBuiltinFunc) -> Result<ConstValue, ConstEvalError> {
        Ok(ConstValue::Int(builtin_func.loc.line.try_into().unwrap()))
    }

    fn eval_column(&self, builtin_func: &TypedBuiltinFunc) -> Result<ConstValue, ConstEvalError> {
        Ok(ConstValue::Int(builtin_func.loc.column.try_into().unwrap()))
    }

    fn eval_sizeof(&self, builtin_func: &TypedBuiltinFunc) -> Result<ConstValue, ConstEvalError> {
        let arg = builtin_func.args.first().unwrap();

        let Some(arg_type) = arg.ty.as_ref() else {
            return Err(ConstEvalError::TypeError);
        };

        let cir_type = lower_sema_type(self.decl_tables, self.target, self.tctx.clone(), &arg_type);
        let layout = self.tctx.layout_of(&cir_type);

        Ok(ConstValue::Int(layout.size.try_into().unwrap()))
    }

    fn eval_alignof(&self, builtin_func: &TypedBuiltinFunc) -> Result<ConstValue, ConstEvalError> {
        let arg = builtin_func.args.first().unwrap();
        let arg_type = arg.ty.as_ref().unwrap();

        let cir_type = lower_sema_type(self.decl_tables, self.target, self.tctx.clone(), &arg_type);
        let layout = self.tctx.layout_of(&cir_type);

        Ok(ConstValue::Int(layout.align.try_into().unwrap()))
    }

    fn eval_offsetof(&self, builtin_func: &TypedBuiltinFunc) -> Result<ConstValue, ConstEvalError> {
        let type_expr = &builtin_func.args[0];
        let field_expr = &builtin_func.args[1];

        let ty = type_expr.ty.as_ref().ok_or(ConstEvalError::TypeError)?;

        let field_name = field_expr
            .literal_const_string_value()
            .ok_or(ConstEvalError::TypeError)?;

        let cir_type = lower_sema_type(self.decl_tables, self.target, self.tctx.clone(), ty);
        let layout = self.tctx.layout_of(&cir_type);

        let struct_decl_id = ty.as_struct().ok_or(ConstEvalError::TypeError)?;
        let struct_decl = self.decl_tables.struct_decl(struct_decl_id);

        for (i, field) in struct_decl.fields.iter().enumerate() {
            if field.name == field_name {
                let field_offset = &layout.lookup_field_offset(i);

                return Ok(ConstValue::Int((*field_offset).try_into().unwrap()));
            }
        }

        Err(ConstEvalError::TypeError)
    }

    fn eval_typeof(&self, builtin_func: &TypedBuiltinFunc) -> Result<ConstValue, ConstEvalError> {
        let operand = &builtin_func.args[0];

        let ty = operand.ty.as_ref().ok_or(ConstEvalError::TypeError)?;

        Ok(ConstValue::Type(ty.clone()))
    }
}
