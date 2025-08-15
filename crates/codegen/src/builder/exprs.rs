use std::ops::Add;

use crate::builder::{
    module::CodeGenBuilder,
    values::{InternalValue, InternalValueKind},
};
use ast::{
    LiteralKind,
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
    token::Location,
};
use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    types::BasicTypeEnum,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
};
use resolver::scope::LocalScopeRef;
use typed_ast::{
    SymbolID, TypedExpression, TypedExpressionKind, TypedFuncCall, TypedInfixExpression, TypedLiteral,
    TypedPrefixExpression, TypedUnaryExpression,
    types::{BasicConcreteType, ConcreteType},
};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_expr(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        typed_expr: &TypedExpression,
    ) -> InternalValue<'a> {
        match &typed_expr.kind {
            TypedExpressionKind::Symbol(symbol_id) => self.build_lvalue_with_symbol_id(*symbol_id),
            TypedExpressionKind::Literal(typed_literal) => self.build_literal(local_scope_opt, typed_literal),
            TypedExpressionKind::Prefix(typed_prefix_expr) => {
                self.build_prefix_expr(local_scope_opt, typed_prefix_expr)
            }
            TypedExpressionKind::Infix(typed_infix_expr) => self.build_infix_expr(local_scope_opt, typed_infix_expr),
            TypedExpressionKind::Unary(typed_unary_expr) => self.build_unary_expr(local_scope_opt, typed_unary_expr),
            TypedExpressionKind::Assignment(typed_assignment) => todo!(),
            TypedExpressionKind::Cast(typed_cast) => todo!(),
            TypedExpressionKind::Array(typed_array) => todo!(),
            TypedExpressionKind::ArrayIndex(typed_array_index) => todo!(),
            TypedExpressionKind::AddressOf(typed_address_of) => todo!(),
            TypedExpressionKind::Dereference(typed_dereference) => todo!(),
            TypedExpressionKind::StructInit(typed_struct_init) => todo!(),
            TypedExpressionKind::FuncCall(typed_func_call) => self.build_func_call(local_scope_opt, typed_func_call),
            TypedExpressionKind::FieldAccess(typed_field_access) => todo!(),
            TypedExpressionKind::MethodCall(typed_method_call) => todo!(),
            TypedExpressionKind::UnnamedStructValue(typed_unnamed_struct_value) => todo!(),
        }
    }

    fn build_func_call(&self, local_scope_opt: Option<LocalScopeRef>, func_call: &TypedFuncCall) -> InternalValue<'a> {
        let module_id = self.resolver.lookup_symbol_id_in_modules(func_call.symbol_id).unwrap();
        let symbol_entry = self
            .resolver
            .lookup_symbol_entry_with_id(module_id, func_call.symbol_id)
            .unwrap();
        let resolved_func = symbol_entry.as_func().unwrap();
        let return_type = resolved_func.func_sig.return_type.clone();
        let fn_value = self.get_or_declare_func(func_call.symbol_id, resolved_func);

        let args: Vec<BasicMetadataValueEnum<'a>> = func_call
            .args
            .iter()
            .map(|typed_expr| {
                let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
                self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue)
                    .as_basic_value()
                    .into()
            })
            .collect();

        let call_result = self
            .llvmbuilder
            .build_call(fn_value, &args, "call")
            .unwrap()
            .try_as_basic_value();

        if let Some(_) = call_result.right() {
            let null_literal =
                BasicValueEnum::PointerValue(self.llvmctx.ptr_type(AddressSpace::default()).const_null());

            InternalValue::new(return_type, InternalValueKind::RValue(null_literal))
        } else if let Some(basic_value) = call_result.left() {
            InternalValue::new(return_type, InternalValueKind::RValue(basic_value))
        } else {
            unreachable!()
        }
    }

    fn build_unary_expr(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        unary_expr: &TypedUnaryExpression,
    ) -> InternalValue<'a> {
        let lvalue = self.build_expr(local_scope_opt.clone(), &unary_expr.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt, lvalue);

        match unary_expr.op {
            UnaryOperator::PreIncrement => {
                // let rvalue_clone = rvalue.clone();
                // rvalue_clone
                todo!();
            }
            UnaryOperator::PreDecrement => todo!(),
            UnaryOperator::PostIncrement => todo!(),
            UnaryOperator::PostDecrement => todo!(),
        }
    }

    fn build_add(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_int_add(lhs, rhs, "add").unwrap());
                InternalValue::new(lhs_rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_add(lhs, rhs, "add").unwrap());
                InternalValue::new(lhs_rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn build_sub(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_int_sub(lhs, rhs, "sub").unwrap());
                InternalValue::new(lhs_rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_sub(lhs, rhs, "sub").unwrap());
                InternalValue::new(lhs_rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn build_mul(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_int_mul(lhs, rhs, "mul").unwrap());
                InternalValue::new(lhs_rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_mul(lhs, rhs, "mul").unwrap());
                InternalValue::new(lhs_rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn build_div(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::IntValue(self.llvmbuilder.build_int_signed_div(lhs, rhs, "div").unwrap());
                InternalValue::new(lhs_rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_div(lhs, rhs, "div").unwrap());
                InternalValue::new(lhs_rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn build_rem(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::IntValue(self.llvmbuilder.build_int_signed_rem(lhs, rhs, "rem").unwrap());
                InternalValue::new(lhs_rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_rem(lhs, rhs, "rem").unwrap());
                InternalValue::new(lhs_rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn build_cmp(
        &self,
        lhs_rvalue: InternalValue<'a>,
        rhs_rvalue: InternalValue<'a>,
        int_pred: IntPredicate,
        float_pred: FloatPredicate,
    ) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let cmp = self.llvmbuilder.build_int_compare(int_pred, lhs, rhs, "cmp").unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_float_compare(float_pred, lhs, rhs, "cmp")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_cmp_eq(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "cmp")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "cmp")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "cmp")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_cmp_neq(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "cmp")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_float_compare(FloatPredicate::ONE, lhs, rhs, "cmp")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "cmp")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_logical_or(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let or_value = self.llvmbuilder.build_or(lhs, rhs, "cmp").unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(or_value.as_basic_value_enum()),
                )
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                // Use LLVM PHI to implement this feature.
                // func main() {
                //     #ptr1: int* = null;
                //     #ptr2: int* = null;
                //     #result = ptr1 || ptr2;
                // }
                todo!();
            }
            _ => unreachable!(),
        }
    }

    fn build_logical_and(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvmbuilder.build_and(lhs, rhs, "cmp").unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(and_value.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_infix_expr(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        infix_expr: &TypedInfixExpression,
    ) -> InternalValue<'a> {
        let lhs_lvalue = self.build_expr(local_scope_opt.clone(), &infix_expr.lhs);
        let rhs_lvalue = self.build_expr(local_scope_opt.clone(), &infix_expr.rhs);
        let lhs_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lhs_lvalue);
        let rhs_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), rhs_lvalue);

        let get_signed = || {
            let rhs_signed = rhs_rvalue.value_type.as_basic_type().unwrap().is_signed();
            let lhs_signed = lhs_rvalue.value_type.as_basic_type().unwrap().is_signed();
            assert!(lhs_signed == rhs_signed);
            rhs_signed
        };

        match infix_expr.op {
            InfixOperator::Add => self.build_add(lhs_rvalue, rhs_rvalue),
            InfixOperator::Sub => self.build_sub(lhs_rvalue, rhs_rvalue),
            InfixOperator::Mul => self.build_mul(lhs_rvalue, rhs_rvalue),
            InfixOperator::Div => self.build_div(lhs_rvalue, rhs_rvalue),
            InfixOperator::Rem => self.build_rem(lhs_rvalue, rhs_rvalue),
            InfixOperator::LessThan => {
                if get_signed() {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SLT, FloatPredicate::OLT)
                } else {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::ULT, FloatPredicate::OLT)
                }
            }
            InfixOperator::LessEqual => {
                if get_signed() {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SLE, FloatPredicate::OLE)
                } else {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::ULE, FloatPredicate::OLE)
                }
            }
            InfixOperator::GreaterThan => {
                if get_signed() {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SGT, FloatPredicate::OGT)
                } else {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::UGT, FloatPredicate::OGT)
                }
            }
            InfixOperator::GreaterEqual => {
                if get_signed() {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::SGE, FloatPredicate::OGE)
                } else {
                    self.build_cmp(lhs_rvalue, rhs_rvalue, IntPredicate::UGE, FloatPredicate::OGE)
                }
            }
            InfixOperator::Equal => self.build_cmp_eq(lhs_rvalue, rhs_rvalue),
            InfixOperator::NotEqual => self.build_cmp_neq(lhs_rvalue, rhs_rvalue),
            InfixOperator::Or => self.build_logical_or(lhs_rvalue, rhs_rvalue),
            InfixOperator::And => self.build_logical_and(lhs_rvalue, rhs_rvalue),
        }
    }

    fn build_negate(&self, rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(int_value) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_int_neg(int_value, "neg").unwrap());
                InternalValue::new(rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            BasicValueEnum::FloatValue(float_value) => {
                let basic_value =
                    BasicValueEnum::FloatValue(self.llvmbuilder.build_float_neg(float_value, "neg").unwrap());
                InternalValue::new(rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn build_logical_not(&self, rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(int_value) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_not(int_value, "neg").unwrap());
                InternalValue::new(rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
        }
    }

    fn build_sizeof(&self, local_scope_opt: Option<LocalScopeRef>, rvalue: InternalValue<'a>) -> InternalValue<'a> {
        let any_type = self.build_concrete_type(local_scope_opt.clone(), rvalue.value_type);
        let size_internal_value = InternalValue::new(
            ConcreteType::BasicType(BasicConcreteType::SizeT),
            InternalValueKind::RValue(BasicValueEnum::IntValue(any_type.size_of().unwrap())),
        );
        self.build_implicit_cast(
            local_scope_opt,
            ConcreteType::BasicType(BasicConcreteType::SizeT),
            size_internal_value,
        )
    }

    fn build_prefix_expr(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        prefix_expr: &TypedPrefixExpression,
    ) -> InternalValue<'a> {
        let lvalue = self.build_expr(local_scope_opt.clone(), &prefix_expr.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

        match prefix_expr.op {
            PrefixOperator::Bang => self.build_logical_not(rvalue),
            PrefixOperator::Minus => self.build_negate(rvalue),
            PrefixOperator::SizeOf => self.build_sizeof(local_scope_opt, rvalue),
        }
    }

    fn build_lvalue_with_symbol_id(&self, symbol_id: SymbolID) -> InternalValue<'a> {
        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&symbol_id).unwrap();
        let (pointer, concrete_type) = local_ir_value.as_lvalue().unwrap();
        let internal_value = InternalValue::new(concrete_type.clone().clone(), InternalValueKind::LValue(*pointer));

        drop(irreg);
        internal_value
    }

    fn build_literal(&self, local_scope_opt: Option<LocalScopeRef>, literal: &TypedLiteral) -> InternalValue<'a> {
        let basic_type_enum: BasicTypeEnum<'a> = self
            .build_concrete_type(local_scope_opt, literal.ty.clone())
            .try_into()
            .unwrap();

        let basic_value = match &literal.kind {
            LiteralKind::Bool(value) => {
                BasicValueEnum::IntValue(self.llvmctx.bool_type().const_int(*value as u64, false))
            }
            LiteralKind::Integer(value, _) => {
                let signed = literal.ty.clone().as_basic_type().unwrap().is_signed();

                BasicValueEnum::IntValue(
                    basic_type_enum
                        .into_int_type()
                        .const_int((*value).try_into().unwrap(), signed),
                )
            }
            LiteralKind::Float(value, _) => {
                BasicValueEnum::FloatValue(basic_type_enum.into_float_type().const_float(*value))
            }
            LiteralKind::Char(value) => {
                BasicValueEnum::IntValue(self.llvmctx.i8_type().const_int(*value as u64, false))
            }
            LiteralKind::Null => {
                BasicValueEnum::PointerValue(self.llvmctx.ptr_type(AddressSpace::default()).const_null())
            }
            LiteralKind::String(_, string_prefix) => todo!(),
        };

        InternalValue::new(literal.ty.clone(), InternalValueKind::RValue(basic_value))
    }
}
