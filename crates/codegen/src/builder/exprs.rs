use crate::builder::{
    module::CodeGenBuilder,
    values::{InternalValue, InternalValueKind},
};
use ast::{
    LiteralKind, StringPrefix,
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
};
use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    types::BasicTypeEnum,
    values::{ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum},
};
use resolver::scope::LocalScopeRef;
use typed_ast::{
    SymbolID, TypedAddressOf, TypedArray, TypedAssignment, TypedCast, TypedDereference, TypedExpression,
    TypedExpressionKind, TypedFuncCall, TypedInfixExpression, TypedLiteral, TypedPrefixExpression, TypedStructInit,
    TypedUnaryExpression, TypedUnnamedStructValue,
    types::{BasicConcreteType, ConcreteType, ResolvedSymbol},
};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_expr(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        typed_expr: &TypedExpression,
    ) -> InternalValue<'a> {
        match &typed_expr.kind {
            TypedExpressionKind::Symbol(symbol_id, ..) => self.build_lvalue_with_symbol_id(*symbol_id),
            TypedExpressionKind::Literal(typed_literal) => self.build_literal(local_scope_opt, typed_literal),
            TypedExpressionKind::Prefix(typed_prefix_expr) => {
                self.build_prefix_expr(local_scope_opt, typed_prefix_expr)
            }
            TypedExpressionKind::Infix(typed_infix_expr) => self.build_infix_expr(local_scope_opt, typed_infix_expr),
            TypedExpressionKind::Unary(typed_unary_expr) => self.build_unary_expr(local_scope_opt, typed_unary_expr),
            TypedExpressionKind::Assignment(typed_assign) => self.build_assign(local_scope_opt, typed_assign),
            TypedExpressionKind::Cast(typed_cast) => self.build_cast_expr(local_scope_opt, typed_cast),
            TypedExpressionKind::Array(typed_array) => self.build_array_expr(local_scope_opt, typed_array),
            TypedExpressionKind::ArrayIndex(typed_array_index) => todo!(),
            TypedExpressionKind::AddressOf(typed_address_of) => {
                self.build_address_of(local_scope_opt, typed_address_of)
            }
            TypedExpressionKind::Dereference(typed_dereference) => {
                self.build_dereference(local_scope_opt, typed_dereference)
            }
            TypedExpressionKind::StructInit(typed_struct_init) => {
                self.build_struct_init(local_scope_opt, typed_struct_init)
            }
            TypedExpressionKind::FuncCall(typed_func_call) => self.build_func_call(local_scope_opt, typed_func_call),
            TypedExpressionKind::FieldAccess(typed_field_access) => todo!(),
            TypedExpressionKind::MethodCall(typed_method_call) => todo!(),
            TypedExpressionKind::UnnamedStructValue(typed_unnamed_struct_value) => {
                self.build_unnamed_struct_value(local_scope_opt, typed_unnamed_struct_value)
            }
        }
    }

    fn build_unnamed_struct_value(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        unnamed_struct_value: &TypedUnnamedStructValue,
    ) -> InternalValue<'a> {
        let struct_type = self
            .build_unnamed_struct_type(
                local_scope_opt.clone(),
                &unnamed_struct_value.unnamed_struct_type.clone().unwrap(),
            )
            .into_struct_type();

        let mut struct_value = struct_type.get_undef();

        let mut all_const = true;
        let field_values: Vec<BasicValueEnum<'a>> = unnamed_struct_value
            .fields
            .iter()
            .map(|unnamed_struct_value_field| {
                let field_lvalue = self.build_expr(local_scope_opt.clone(), &unnamed_struct_value_field.field_value);
                let field_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), field_lvalue);
                let field_basic_value = field_rvalue.as_basic_value();
                if !self.is_basic_value_constant(field_basic_value) {
                    all_const = false;
                }
                field_basic_value
            })
            .collect();

        if all_const {
            struct_value = struct_type.const_named_struct(&field_values);
        } else {
            field_values.iter().enumerate().for_each(|(index, field_value)| {
                struct_value = self
                    .llvmbuilder
                    .build_insert_value(struct_value, *field_value, index.try_into().unwrap(), "insert")
                    .unwrap()
                    .into_struct_value();
            });
        }

        InternalValue::new(
            ConcreteType::UnnamedStruct(unnamed_struct_value.unnamed_struct_type.clone().unwrap()),
            InternalValueKind::RValue(struct_value.as_basic_value_enum()),
        )
    }

    fn build_struct_init(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        typed_struct_init: &TypedStructInit,
    ) -> InternalValue<'a> {
        let struct_type = self
            .build_concrete_type(
                local_scope_opt.clone(),
                ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(typed_struct_init.symbol_id)),
            )
            .into_struct_type();

        let mut struct_value = struct_type.get_undef();

        let mut all_const = true;
        let field_values: Vec<BasicValueEnum<'a>> = typed_struct_init
            .fields
            .iter()
            .map(|field_init| {
                let field_lvalue = self.build_expr(local_scope_opt.clone(), &field_init.value);
                let field_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), field_lvalue);
                let field_basic_value = field_rvalue.as_basic_value();
                if !self.is_basic_value_constant(field_basic_value) {
                    all_const = false;
                }
                field_basic_value
            })
            .collect();

        if all_const {
            struct_value = struct_type.const_named_struct(&field_values);
        } else {
            field_values.iter().enumerate().for_each(|(index, field_value)| {
                struct_value = self
                    .llvmbuilder
                    .build_insert_value(struct_value, *field_value, index.try_into().unwrap(), "insert")
                    .unwrap()
                    .into_struct_value();
            });
        }

        InternalValue::new(
            ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(typed_struct_init.symbol_id)),
            InternalValueKind::RValue(struct_value.as_basic_value_enum()),
        )
    }

    fn build_assign(&mut self, local_scope_opt: Option<LocalScopeRef>, assign: &TypedAssignment) -> InternalValue<'a> {
        let lhs_lvalue = self.build_expr(local_scope_opt.clone(), &assign.lhs);
        let rhs_lvalue = self.build_expr(local_scope_opt.clone(), &assign.rhs);
        let rhs_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), rhs_lvalue);

        assert!(lhs_lvalue.as_basic_value().is_pointer_value() == true);
        let pointer_value = lhs_lvalue.as_basic_value().into_pointer_value();

        self.llvmbuilder
            .build_store(pointer_value, rhs_rvalue.as_basic_value())
            .unwrap();
        rhs_rvalue
    }

    fn build_dereference(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        deref: &TypedDereference,
    ) -> InternalValue<'a> {
        let operand_type = deref.operand.concrete_type.clone().unwrap();
        let lvalue = self.build_expr(local_scope_opt.clone(), &deref.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

        InternalValue::new(operand_type, InternalValueKind::RValue(rvalue.as_basic_value()))
    }

    fn build_address_of(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        address_of: &TypedAddressOf,
    ) -> InternalValue<'a> {
        let operand_type = address_of.operand.concrete_type.clone().unwrap();
        let lvalue = self.build_expr(local_scope_opt.clone(), &address_of.operand);

        InternalValue::new(
            ConcreteType::Pointer(Box::new(operand_type)),
            InternalValueKind::RValue(lvalue.as_basic_value()),
        )
    }

    fn build_array_expr(&mut self, local_scope_opt: Option<LocalScopeRef>, array: &TypedArray) -> InternalValue<'a> {
        let array_concrete_type = array.array_type.as_array_type().unwrap();
        let element_type = array_concrete_type.element_type.clone();
        let array_type = self
            .build_concrete_type(local_scope_opt.clone(), array.array_type.clone())
            .into_array_type();

        let mut all_const = true;
        let elements: Vec<BasicValueEnum<'a>> = array
            .elements
            .iter()
            .map(|typed_expr| {
                let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
                let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
                let casted_rvalue = self
                    .build_implicit_cast(local_scope_opt.clone(), *element_type.clone(), rvalue)
                    .as_basic_value();
                if !self.is_basic_value_constant(casted_rvalue) {
                    all_const = false;
                }
                casted_rvalue
            })
            .collect();

        if all_const {
            let array_value = unsafe { ArrayValue::new_const_array(&array_type, &elements) };

            InternalValue::new(
                array.array_type.clone(),
                InternalValueKind::RValue(array_value.as_basic_value_enum()),
            )
        } else {
            let mut array_value = array_type.get_undef();

            elements.iter().enumerate().for_each(|(index, element)| {
                array_value = self
                    .llvmbuilder
                    .build_insert_value(array_value, *element, index.try_into().unwrap(), "insert")
                    .unwrap()
                    .into_array_value();
            });

            InternalValue::new(
                array.array_type.clone(),
                InternalValueKind::RValue(array_value.as_basic_value_enum()),
            )
        }
    }

    fn is_basic_value_constant(&self, basic_value: BasicValueEnum<'a>) -> bool {
        match basic_value {
            BasicValueEnum::IntValue(int_value) => int_value.is_const(),
            BasicValueEnum::FloatValue(float_value) => float_value.is_const(),
            BasicValueEnum::PointerValue(ptr_value) => ptr_value.is_const(),
            BasicValueEnum::StructValue(struct_value) => struct_value.is_const(),
            BasicValueEnum::ArrayValue(array_value) => array_value.is_const(),
            BasicValueEnum::VectorValue(vector_value) => vector_value.is_const(),
        }
    }

    fn build_cast_expr(&mut self, local_scope_opt: Option<LocalScopeRef>, cast: &TypedCast) -> InternalValue<'a> {
        let lvalue = self.build_expr(local_scope_opt.clone(), &cast.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

        let target_any_type = self.build_concrete_type(local_scope_opt, cast.target_type.clone());
        let any_value = self.build_cast(target_any_type, rvalue);
        InternalValue::new(
            cast.target_type.clone(),
            InternalValueKind::RValue(any_value.try_into().unwrap()),
        )
    }

    fn build_func_call(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        func_call: &TypedFuncCall,
    ) -> InternalValue<'a> {
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
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        unary_expr: &TypedUnaryExpression,
    ) -> InternalValue<'a> {
        let lvalue = self.build_expr(local_scope_opt.clone(), &unary_expr.operand);
        let lvalue_pointer = lvalue.as_basic_value().into_pointer_value();
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt, lvalue);
        let rvalue_basic_type = rvalue.value_type.as_basic_type().unwrap();
        let signed = rvalue_basic_type.is_signed();
        let unit_type = self
            .build_basic_concrete_type(rvalue_basic_type.clone())
            .into_int_type();
        let unit_value = InternalValue::new(
            rvalue.value_type.clone(),
            InternalValueKind::RValue(BasicValueEnum::IntValue(unit_type.const_int(1, signed))),
        );

        match unary_expr.op {
            UnaryOperator::PreIncrement => {
                let new_rhs_rvalue = self.build_add(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                new_rhs_rvalue
            }
            UnaryOperator::PreDecrement => {
                let new_rhs_rvalue = self.build_sub(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                new_rhs_rvalue
            }
            UnaryOperator::PostIncrement => {
                let rhs_rvalue_clone = rvalue.clone();
                let new_rhs_rvalue = self.build_add(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                rhs_rvalue_clone
            }
            UnaryOperator::PostDecrement => {
                let rhs_rvalue_clone = rvalue.clone();
                let new_rhs_rvalue = self.build_sub(rvalue, unit_value);
                self.llvmbuilder
                    .build_store(lvalue_pointer, new_rhs_rvalue.as_basic_value())
                    .unwrap();
                rhs_rvalue_clone
            }
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
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "eq")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
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
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "neq")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_float_compare(FloatPredicate::ONE, lhs, rhs, "neq")
                    .unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(cmp.as_basic_value_enum()),
                )
            }
            (BasicValueEnum::PointerValue(lhs), BasicValueEnum::PointerValue(rhs)) => {
                let cmp = self
                    .llvmbuilder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "neq")
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
                let or_value = self.llvmbuilder.build_or(lhs, rhs, "lor").unwrap();
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
                let and_value = self.llvmbuilder.build_and(lhs, rhs, "land").unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(and_value.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_xor(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvmbuilder.build_xor(lhs, rhs, "xor").unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(and_value.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_bitwise_and(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvmbuilder.build_and(lhs, rhs, "xor").unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(and_value.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_bitwise_or(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let and_value = self.llvmbuilder.build_or(lhs, rhs, "or").unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(and_value.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_bitwise_and_not(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                // ~rhs = rhs XOR all ones
                let all_ones = rhs.get_type().const_all_ones();
                let not_rhs = self.llvmbuilder.build_xor(rhs, all_ones, "not_rhs").unwrap();

                // lhs AND (~rhs)
                let and_not_value = self.llvmbuilder.build_and(lhs, not_rhs, "and_not").unwrap();

                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Int), // result is integer, not Bool
                    InternalValueKind::RValue(and_not_value.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_shift_left(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let shift_value = self.llvmbuilder.build_left_shift(lhs, rhs, "lshift").unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(shift_value.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_shift_right(&self, lhs_rvalue: InternalValue<'a>, rhs_rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match (lhs_rvalue.as_basic_value(), rhs_rvalue.as_basic_value()) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let rhs_basic_type = rhs_rvalue.value_type.as_basic_type().unwrap();
                let signed = rhs_basic_type.is_signed();

                let shift_value = self.llvmbuilder.build_right_shift(lhs, rhs, signed, "lshift").unwrap();
                InternalValue::new(
                    ConcreteType::BasicType(BasicConcreteType::Bool),
                    InternalValueKind::RValue(shift_value.as_basic_value_enum()),
                )
            }
            _ => unreachable!(),
        }
    }

    fn build_infix_expr(
        &mut self,
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
            InfixOperator::BitwiseAnd => self.build_bitwise_and(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseOr => self.build_bitwise_or(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseXor => self.build_xor(lhs_rvalue, rhs_rvalue),
            InfixOperator::BitwiseAndNot => self.build_bitwise_and_not(lhs_rvalue, rhs_rvalue),
            InfixOperator::ShiftLeft => self.build_shift_left(lhs_rvalue, rhs_rvalue),
            InfixOperator::ShiftRight => self.build_shift_right(lhs_rvalue, rhs_rvalue),
        }
    }

    fn build_bitwise_not(&self, rvalue: InternalValue<'a>) -> InternalValue<'a> {
        match rvalue.as_basic_value() {
            BasicValueEnum::IntValue(int_value) => {
                let basic_value = BasicValueEnum::IntValue(self.llvmbuilder.build_not(int_value, "neg").unwrap());
                InternalValue::new(rvalue.value_type.clone(), InternalValueKind::RValue(basic_value))
            }
            _ => unreachable!(),
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

    fn build_sizeof(&mut self, local_scope_opt: Option<LocalScopeRef>, rvalue: InternalValue<'a>) -> InternalValue<'a> {
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
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        prefix_expr: &TypedPrefixExpression,
    ) -> InternalValue<'a> {
        let lvalue = self.build_expr(local_scope_opt.clone(), &prefix_expr.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

        match prefix_expr.op {
            PrefixOperator::Bang => self.build_logical_not(rvalue),
            PrefixOperator::Minus => self.build_negate(rvalue),
            PrefixOperator::BitwiseNot => self.build_bitwise_not(rvalue),
            PrefixOperator::SizeOf => self.build_sizeof(local_scope_opt, rvalue),
        }
    }

    fn build_lvalue_with_symbol_id(&self, symbol_id: SymbolID) -> InternalValue<'a> {
        let irreg = self.irreg.borrow();
        let local_ir_value = irreg.get(&symbol_id).unwrap();
        let (pointer, concrete_type) = match local_ir_value.as_lvalue() {
            Some((pointer, concrete_type)) => (pointer.clone(), concrete_type.clone()),
            None => match local_ir_value.as_global_value() {
                Some((global_value, concrete_type)) => (global_value.as_pointer_value().clone(), concrete_type.clone()),
                None => panic!("Couldn't find any lvalue with this symbol id."),
            },
        };
        let internal_value = InternalValue::new(concrete_type.clone().clone(), InternalValueKind::LValue(pointer));

        drop(irreg);
        internal_value
    }

    fn build_literal(&mut self, local_scope_opt: Option<LocalScopeRef>, literal: &TypedLiteral) -> InternalValue<'a> {
        let basic_type_enum: BasicTypeEnum<'a> = self
            .build_concrete_type(local_scope_opt, literal.ty.clone().unwrap())
            .try_into()
            .unwrap();

        let basic_value = match &literal.kind {
            LiteralKind::Bool(value) => {
                BasicValueEnum::IntValue(self.llvmctx.bool_type().const_int(*value as u64, false))
            }
            LiteralKind::Integer(value, _) => {
                let signed = literal.ty.clone().unwrap().get_const_inner().as_basic_type().unwrap().is_signed();

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
            LiteralKind::String(value, string_prefix) => {
                if let Some(prefix) = string_prefix {
                    match prefix {
                        StringPrefix::C => self.build_c_style_string(value.clone(), literal.loc.clone(), 0),
                        StringPrefix::B => self.build_byte_string(value.clone(), literal.loc.clone(), 0),
                    }
                } else {
                    self.build_string_literal(value.clone(), literal.loc.clone(), 0)
                }
            }
        };

        InternalValue::new(literal.ty.clone().unwrap(), InternalValueKind::RValue(basic_value))
    }
}
