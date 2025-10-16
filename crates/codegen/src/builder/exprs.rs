use crate::{
    builder::{
        module::{CodeGenBuilder, LocalIRValue},
        values::{InternalValue, InternalValueKind},
    },
    llvm_set_current_location,
};
use ast::{
    LiteralKind, SelfModifierKind, StringPrefix,
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
    source_loc::SourceLoc,
};
use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    debug_info::AsDIScope,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, StructType},
    values::{ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, IntValue, PointerValue},
};
use resolver::{
    scope::{LocalOrGlobalSymbol, LocalScopeRef, ResolvedUnion, SymbolEntryKind},
    typed_func_type_from_func_sig,
};
use typed_ast::{
    SymbolID, TypedAddressOf, TypedArray, TypedArrayIndex, TypedAssignment, TypedCast, TypedDereference,
    TypedExpression, TypedExpressionKind, TypedFieldAccess, TypedFuncCall, TypedFuncParamKind, TypedInfixExpression,
    TypedLiteral, TypedMethodCall, TypedPrefixExpression, TypedSizeOfExpression, TypedStructInit,
    TypedTupleMemberAccess, TypedTupleValue, TypedUnaryExpression, TypedUnnamedStructValue,
    types::{BasicConcreteType, ConcreteType, ResolvedSymbol, TypedFuncType},
};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_expr(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        typed_expr: &TypedExpression,
    ) -> InternalValue<'a> {
        llvm_set_current_location!(&self, typed_expr.loc);

        match &typed_expr.kind {
            TypedExpressionKind::Symbol(symbol_id, _) => self.build_lvalue_with_symbol_id(local_scope_opt, *symbol_id),
            TypedExpressionKind::Literal(typed_literal) => self.build_literal(local_scope_opt, typed_literal),
            TypedExpressionKind::Prefix(typed_prefix_expr) => {
                self.build_prefix_expr(local_scope_opt, typed_prefix_expr)
            }
            TypedExpressionKind::Infix(typed_infix_expr) => self.build_infix_expr(local_scope_opt, typed_infix_expr),
            TypedExpressionKind::Unary(typed_unary_expr) => self.build_unary_expr(local_scope_opt, typed_unary_expr),
            TypedExpressionKind::Assignment(typed_assign) => self.build_assign(local_scope_opt, typed_assign),
            TypedExpressionKind::Cast(typed_cast) => self.build_cast_expr(local_scope_opt, typed_cast),
            TypedExpressionKind::Array(typed_array) => self.build_array_expr(local_scope_opt, typed_array),
            TypedExpressionKind::AddressOf(typed_address_of) => {
                self.build_address_of(local_scope_opt, typed_address_of)
            }
            TypedExpressionKind::Dereference(typed_dereference) => {
                self.build_dereference(local_scope_opt, typed_dereference)
            }
            TypedExpressionKind::StructInit(struct_init) => self.build_struct_init(local_scope_opt, struct_init),
            TypedExpressionKind::FuncCall(typed_func_call) => self.build_func_call(local_scope_opt, typed_func_call),
            TypedExpressionKind::FieldAccess(field_access) => self.build_field_access(local_scope_opt, field_access),
            TypedExpressionKind::ArrayIndex(typed_array_index) => {
                self.build_array_index(local_scope_opt, typed_array_index)
            }
            TypedExpressionKind::UnnamedStructValue(typed_unnamed_struct_value) => {
                self.build_unnamed_struct_value(local_scope_opt, typed_unnamed_struct_value)
            }
            TypedExpressionKind::MethodCall(method_call) => self.build_method_call(local_scope_opt, method_call),
            TypedExpressionKind::SizeOfExpression(typed_size_of_expr) => {
                self.build_sizeof(local_scope_opt, typed_size_of_expr)
            }
            TypedExpressionKind::Lambda(typed_lambda) => self.build_lambda_expr(typed_lambda),
            TypedExpressionKind::Tuple(tuple_value) => self.build_tuple_value(local_scope_opt, tuple_value),
            TypedExpressionKind::TupleMemberAccess(tuple_member_access) => {
                self.build_tuple_member_access(local_scope_opt, tuple_member_access)
            }
            TypedExpressionKind::ConcreteType(..) => unreachable!(),
        }
    }

    fn build_tuple_member_access(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        tuple_member_access: &TypedTupleMemberAccess,
    ) -> InternalValue<'a> {
        let lvalue = self.build_expr(local_scope_opt.clone(), &tuple_member_access.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

        let index_lvalue = self.build_expr(local_scope_opt.clone(), &tuple_member_access.index);
        let index_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), index_lvalue);

        let index_int_value = index_rvalue
            .as_basic_value()
            .into_int_value()
            .get_zero_extended_constant()
            .unwrap();

        let extracted_value = self
            .llvmbuilder
            .build_extract_value(
                rvalue.as_basic_value().into_struct_value(),
                index_int_value.try_into().unwrap(),
                "extractvalue",
            )
            .unwrap();

        let tuple_type = rvalue.value_type.as_tuple_type().unwrap();
        let element_type = tuple_type.type_list.get(index_int_value as usize).unwrap();

        InternalValue::new(element_type.clone(), InternalValueKind::RValue(extracted_value))
    }

    fn build_tuple_value(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        tuple_value: &TypedTupleValue,
    ) -> InternalValue<'a> {
        let mut const_elements = true;

        let tuple_type = self.get_tuple_type_from_tuple_value(tuple_value);

        let basic_values: Vec<BasicValueEnum<'a>> = tuple_value
            .expr_list
            .iter()
            .enumerate()
            .map(|(idx, expr)| {
                let target_type = tuple_type.type_list.get(idx).unwrap();

                let lvalue = self.build_expr(local_scope_opt.clone(), expr);
                let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
                let casted_rvalue = self.build_implicit_cast(local_scope_opt.clone(), target_type.clone(), rvalue);

                if !self.is_basic_value_constant(casted_rvalue.as_basic_value()) {
                    const_elements = false;
                }

                casted_rvalue.as_basic_value()
            })
            .collect();

        let tuple_struct_type = self
            .build_tuple_type(local_scope_opt.clone(), &tuple_type)
            .into_struct_type();

        if const_elements {
            let tuple_struct_value = tuple_struct_type.const_named_struct(&basic_values);

            InternalValue::new(
                ConcreteType::Tuple(tuple_type),
                InternalValueKind::RValue(tuple_struct_value.into()),
            )
        } else {
            let mut tuple_struct_value = tuple_struct_type.get_undef();

            basic_values.iter().enumerate().for_each(|(index, rvalue)| {
                tuple_struct_value = self
                    .llvmbuilder
                    .build_insert_value(tuple_struct_value, *rvalue, index.try_into().unwrap(), "insert")
                    .unwrap()
                    .into_struct_value();
            });

            InternalValue::new(
                ConcreteType::Tuple(tuple_type),
                InternalValueKind::RValue(tuple_struct_value.into()),
            )
        }
    }

    pub(crate) fn build_runtime_inbounds_check(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        pointer: PointerValue<'a>,
        pointee_ty: ConcreteType,
        index: InternalValue<'a>,
        array_length: u32,
    ) -> InternalValue<'a> {
        let ptr_type = self.llvmctx.ptr_type(AddressSpace::default());
        let current_block = self.blockreg.current_block_ref.unwrap();
        let current_func = self.blockreg.current_func_ref.unwrap();

        let failure_block = self.llvmctx.append_basic_block(current_func, "inbounds_check.failure");
        let success_block = self.llvmctx.append_basic_block(current_func, "inbounds_check.success");

        let pointee_basic_ty: BasicTypeEnum<'a> = self
            .build_concrete_type(local_scope_opt, pointee_ty.clone())
            .try_into()
            .unwrap();

        let target_data = self.llvmtm.get_target_data();
        let ptr_sized_int_type = self.llvmctx.ptr_sized_int_type(&target_data, None);
        let array_length_int_value = ptr_sized_int_type.const_int(array_length.into(), false);

        let compare_result = self
            .llvmbuilder
            .build_int_compare(
                inkwell::IntPredicate::ULT,
                index.as_basic_value().into_int_value(),
                array_length_int_value,
                "cmp",
            )
            .unwrap();

        self.llvmbuilder
            .build_conditional_branch(compare_result, success_block, failure_block)
            .unwrap();

        self.mark_block_terminated(current_block);

        self.llvmbuilder.position_at_end(failure_block);

        let panic_msg = self
            .build_global_str(
                format!(
                    "panic: Index out of bounds!\nAttempted to access index %d in an array of size {}.",
                    array_length
                ),
                SourceLoc::default(),
            )
            .0;

        let module = self.llvmmodule.borrow_mut();

        // call fprintf to display panic message

        let void_type = self.llvmctx.void_type();
        let i32_type = self.llvmctx.i32_type();
        let fprintf_type = i32_type.fn_type(
            &[
                BasicMetadataTypeEnum::from(ptr_type), // FILE *stream
                BasicMetadataTypeEnum::from(ptr_type), // const char *format
            ],
            true,
        );

        let fprintf_fn_value = match module.get_function("fprintf") {
            Some(fn_value) => fn_value,
            None => module.add_function("fprintf", fprintf_type, None),
        };

        let stderr_global = match module.get_global("stderr") {
            Some(global_value) => global_value,
            None => {
                let global_value = module.add_global(ptr_type, None, "stderr");
                global_value.set_linkage(inkwell::module::Linkage::External);
                global_value
            }
        };

        let stderr_val = self
            .llvmbuilder
            .build_load(ptr_type, stderr_global.as_pointer_value(), "stderr_val")
            .unwrap();

        self.llvmbuilder
            .build_call(
                fprintf_fn_value,
                &[
                    BasicMetadataValueEnum::PointerValue(stderr_val.into_pointer_value()),
                    BasicMetadataValueEnum::PointerValue(panic_msg.as_pointer_value()),
                    index.as_basic_value().into(),
                ],
                "call",
            )
            .unwrap();

        // exit program with status code 1

        let error_status_code = i32_type.const_int(1, false);

        let exit_fn_value = match module.get_function("exit") {
            Some(fn_value) => fn_value,
            None => {
                let exit_fn_type = void_type.fn_type(
                    &[
                        BasicMetadataTypeEnum::from(i32_type), // int status
                    ],
                    false,
                );
                module.add_function("exit", exit_fn_type, None)
            }
        };

        self.llvmbuilder
            .build_call(exit_fn_value, &[error_status_code.into()], "call")
            .unwrap();

        self.llvmbuilder.build_unreachable().unwrap();

        self.llvmbuilder.position_at_end(success_block);
        self.blockreg.current_block_ref = Some(success_block);

        let ordered_indexes: Vec<IntValue<'a>> = vec![index.as_basic_value().into_int_value()];

        let pointer_value = unsafe {
            self.llvmbuilder
                .build_in_bounds_gep(pointee_basic_ty, pointer, &ordered_indexes, "gep")
                .unwrap()
        };

        InternalValue::new(pointee_ty, InternalValueKind::LValue(pointer_value))
    }

    fn build_array_index_on_pointer(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        lvalue: PointerValue<'a>,   // should be T*, never [N x T]*
        index: InternalValue<'a>,   // expected integer index
        element_type: ConcreteType, // the concrete type of each element (T)
    ) -> InternalValue<'a> {
        let element_basic_type: BasicTypeEnum<'a> = self
            .build_concrete_type(local_scope_opt, element_type.clone())
            .try_into()
            .unwrap();

        let idx_int = index.as_basic_value().into_int_value();

        let element_ptr: PointerValue<'a> = unsafe {
            self.llvmbuilder
                .build_in_bounds_gep(element_basic_type, lvalue, &[idx_int], "elem_ptr")
                .unwrap()
        };

        InternalValue::new(element_type, InternalValueKind::LValue(element_ptr))
    }

    fn build_array_index(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        array_index: &TypedArrayIndex,
    ) -> InternalValue<'a> {
        let lvalue = self.build_expr(local_scope_opt.clone(), &array_index.operand);

        let index_lvalue = self.build_expr(local_scope_opt.clone(), &array_index.index);
        let index_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), index_lvalue);

        if let Some(array_type) = lvalue.value_type.as_array_type() {
            let array_capacity = self.build_array_capacity(local_scope_opt.clone(), array_type);

            self.build_runtime_inbounds_check(
                local_scope_opt,
                lvalue.as_basic_value().into_pointer_value(),
                *array_type.element_type.clone(),
                index_rvalue,
                array_capacity.try_into().unwrap(),
            )
        } else {
            // REVIEW Seems nasty, but works fine.
            // This happens because I didn't had the energy to differ lvalue from rvalue in operand of the array_index.
            // Maybe fixed it later. =)

            let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue.clone());

            if let Some(element_type) = rvalue.value_type.get_pointer_inner() {
                // lvalue
                if let Some(inner_element_type) = element_type.get_pointer_inner() {
                    // rvalue
                    self.build_array_index_on_pointer(
                        // actual element type
                        local_scope_opt,
                        rvalue.as_basic_value().into_pointer_value(),
                        index_rvalue,
                        inner_element_type,
                    )
                } else {
                    if let Some(element_type) = lvalue.value_type.get_pointer_inner() {
                        self.build_array_index_on_pointer(
                            // actual element type
                            local_scope_opt,
                            rvalue.as_basic_value().into_pointer_value(),
                            index_rvalue,
                            element_type,
                        )
                    } else {
                        unreachable!()
                    }
                }
            } else {
                if let Some(element_type) = lvalue.value_type.get_pointer_inner() {
                    self.build_array_index_on_pointer(
                        // actual element type
                        local_scope_opt,
                        rvalue.as_basic_value().into_pointer_value(),
                        index_rvalue,
                        element_type,
                    )
                } else {
                    unreachable!()
                }
            }
        }
    }

    fn build_union_field_access(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        field_access: &TypedFieldAccess,
    ) -> InternalValue<'a> {
        let lvalue_pointer = self
            .build_expr(local_scope_opt.clone(), &field_access.operand)
            .as_basic_value()
            .into_pointer_value();
        let field_type = field_access.field_ty.clone().unwrap();

        InternalValue::new(field_type.clone(), InternalValueKind::LValue(lvalue_pointer))
    }

    fn build_field_access(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        field_access: &TypedFieldAccess,
    ) -> InternalValue<'a> {
        let mut lvalue = match &field_access.operand.kind {
            TypedExpressionKind::Symbol(symbol_id, ..) => {
                let local_or_global_symbol = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt.clone(), *symbol_id)
                    .unwrap();

                if let Some(resolved_enum) = local_or_global_symbol.as_enum() {
                    return self.build_construct_enum_variant_no_field(
                        local_scope_opt,
                        resolved_enum,
                        field_access.field_name.clone(),
                    );
                } else {
                    self.build_lvalue_with_symbol_id(local_scope_opt.clone(), *symbol_id)
                }
            }
            _ => self.build_expr(local_scope_opt.clone(), &field_access.operand),
        };

        let mut operand_ty = field_access.operand.concrete_type.clone().unwrap();

        if operand_ty.is_pointer() && field_access.is_fat_arrow {
            operand_ty = operand_ty.as_rvalue(true); // unwraps the pointer type
            lvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
        }

        let lvalue_pointer = lvalue.as_basic_value().into_pointer_value();

        if let Some(symbol_id) = field_access.object_symbol_id {
            let local_or_global_symbol = self
                .resolver
                .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
                .unwrap();

            if let Some(..) = local_or_global_symbol.as_union() {
                // handle union field access
                return self.build_union_field_access(local_scope_opt.clone(), field_access);
            }
        }

        let pointee_struct_ty: StructType<'a>;
        if field_access.object_symbol_id.is_none() {
            // handle unnamed struct field access
            let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
            pointee_struct_ty = self
                .build_concrete_type(local_scope_opt, rvalue.value_type)
                .try_into()
                .unwrap();
        } else {
            let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
            let rvalue_basic_type = rvalue.as_basic_value().get_type();
            if rvalue_basic_type.is_struct_type() {
                // it tries to get struct type because generic types are monomorphic
                // and pointee_struct_ty cannot be generated through operant_ty.
                pointee_struct_ty = rvalue_basic_type.into_struct_type();
            } else {
                pointee_struct_ty = self
                    .build_concrete_type(local_scope_opt.clone(), operand_ty)
                    .try_into()
                    .unwrap();
            }
        }

        let extracted_value = self
            .llvmbuilder
            .build_struct_gep(
                pointee_struct_ty,
                lvalue_pointer,
                field_access.field_index.unwrap().try_into().unwrap(),
                "struct_gep",
            )
            .unwrap();

        InternalValue::new(
            field_access.field_ty.clone().unwrap(),
            InternalValueKind::LValue(extracted_value),
        )
    }

    pub(crate) fn build_unnamed_struct_value(
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

    fn build_union_init(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        resolved_union: &ResolvedUnion,
        struct_init: &TypedStructInit,
    ) -> InternalValue<'a> {
        let struct_type = self.get_or_declare_union_monomorph(resolved_union, &struct_init.type_args);
        let mut struct_value = struct_type.get_undef();

        let mut all_const = true;
        let field_values: Vec<BasicValueEnum<'a>> = struct_init
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
            ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(struct_init.symbol_id)),
            InternalValueKind::RValue(struct_value.as_basic_value_enum()),
        )
    }

    fn build_struct_init(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        struct_init: &TypedStructInit,
    ) -> InternalValue<'a> {
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), struct_init.symbol_id)
            .unwrap();

        if let Some(resolved_union) = local_or_global_symbol.as_union() {
            return self.build_union_init(local_scope_opt.clone(), resolved_union, struct_init);
        }

        let resolved_struct = local_or_global_symbol.as_struct().unwrap();

        let struct_type = self.get_or_declare_struct_monomorph(resolved_struct, &struct_init.type_args);
        let mut struct_value = struct_type.get_undef();

        let mut all_const = true;
        let field_values: Vec<BasicValueEnum<'a>> = struct_init
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
            ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(struct_init.symbol_id)),
            InternalValueKind::RValue(struct_value.as_basic_value_enum()),
        )
    }

    fn build_assign(&mut self, local_scope_opt: Option<LocalScopeRef>, assign: &TypedAssignment) -> InternalValue<'a> {
        let lhs_lvalue = self.build_expr(local_scope_opt.clone(), &assign.lhs);
        let rhs_lvalue = self.build_expr(local_scope_opt.clone(), &assign.rhs);
        let rhs_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), rhs_lvalue);

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
        let lvalue = self.build_expr(local_scope_opt.clone(), &deref.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

        InternalValue::new(
            rvalue.value_type.clone(),
            InternalValueKind::LValue(rvalue.as_basic_value().into_pointer_value()),
        )
    }

    fn build_address_of(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        address_of: &TypedAddressOf,
    ) -> InternalValue<'a> {
        let operand_type = address_of.operand.concrete_type.clone().unwrap();
        let lvalue = self.build_expr(local_scope_opt.clone(), &address_of.operand);

        if let Some(fn_value) = lvalue.as_func_value() {
            InternalValue::new(
                ConcreteType::Pointer(Box::new(operand_type)),
                InternalValueKind::RValue(self.build_cast_fn_value_to_pointer(fn_value).try_into().unwrap()),
            )
        } else {
            InternalValue::new(
                ConcreteType::Pointer(Box::new(operand_type)),
                InternalValueKind::RValue(lvalue.as_basic_value()),
            )
        }
    }

    fn build_array_expr(&mut self, local_scope_opt: Option<LocalScopeRef>, array: &TypedArray) -> InternalValue<'a> {
        let array_concrete_type = array.array_type.as_array_type().unwrap();
        let element_type = array_concrete_type.element_type.clone();
        let array_type = self
            .build_concrete_type(local_scope_opt.clone(), array.array_type.clone())
            .into_array_type();

        let required_len = self.build_array_capacity(local_scope_opt.clone(), &array_concrete_type);

        let mut all_const = true;
        let mut elements: Vec<BasicValueEnum<'a>> = Vec::with_capacity(required_len);

        for typed_expr in &array.elements {
            let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
            let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
            let casted_rvalue = self
                .build_implicit_cast(local_scope_opt.clone(), *element_type.clone(), rvalue)
                .as_basic_value();

            if !self.is_basic_value_constant(casted_rvalue) {
                all_const = false;
            }

            elements.push(casted_rvalue);
        }

        let element_basic_type: BasicTypeEnum<'a> = self
            .build_concrete_type(local_scope_opt, *element_type)
            .try_into()
            .unwrap();

        let zero_value = element_basic_type.const_zero();
        while elements.len() < required_len {
            elements.push(zero_value);
            all_const = false; // not all const anymore
        }

        if all_const {
            let array_value = unsafe { ArrayValue::new_const_array(&array_type.get_element_type(), &elements) };

            InternalValue::new(
                array.array_type.clone(),
                InternalValueKind::RValue(array_value.as_basic_value_enum()),
            )
        } else {
            let mut array_value = array_type.get_undef();
            for (index, element) in elements.iter().enumerate() {
                array_value = self
                    .llvmbuilder
                    .build_insert_value(array_value, *element, index.try_into().unwrap(), "insert")
                    .unwrap()
                    .into_array_value();
            }
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
            BasicValueEnum::ScalableVectorValue(..) => unreachable!(),
        }
    }

    fn build_cast_expr(&mut self, local_scope_opt: Option<LocalScopeRef>, cast: &TypedCast) -> InternalValue<'a> {
        let lvalue = self.build_expr(local_scope_opt.clone(), &cast.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
        let target_any_type = self.build_concrete_type(local_scope_opt, cast.target_type.clone());
        let casted_rvalue: BasicValueEnum<'a> = self.build_cast(target_any_type, rvalue).try_into().unwrap();
        InternalValue::new(cast.target_type.clone(), InternalValueKind::RValue(casted_rvalue))
    }

    fn build_method_call(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        method_call: &TypedMethodCall,
    ) -> InternalValue<'a> {
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), method_call.object_symbol_id.unwrap())
            .unwrap();

        let (methods, module_id) = {
            if let Some(resolved_struct) = local_or_global_symbol.as_struct() {
                (resolved_struct.struct_sig.methods.clone(), resolved_struct.module_id)
            } else if let Some(resolved_enum) = &local_or_global_symbol.as_enum() {
                let variant_opt = resolved_enum
                    .enum_sig
                    .variants
                    .iter()
                    .find(|variant| variant.get_identifier().as_string() == method_call.method_name);

                if variant_opt.is_some() {
                    return self.build_construct_enum_variant(
                        local_scope_opt,
                        resolved_enum,
                        method_call.method_name.clone(),
                        &method_call.args,
                        &method_call.type_args,
                    );
                } else {
                    (resolved_enum.enum_sig.methods.clone(), resolved_enum.module_id)
                }
            } else if let Some(resolved_union) = local_or_global_symbol.as_union() {
                (resolved_union.union_sig.methods.clone(), resolved_union.module_id)
            } else {
                unreachable!()
            }
        };

        let method_symbol_id = *methods.get(&method_call.method_name).unwrap();

        let symbol_entry = self
            .resolver
            .lookup_symbol_entry_with_id(module_id, method_symbol_id)
            .unwrap();

        let func_sig = match symbol_entry.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method.func_sig,
            _ => unreachable!(),
        };
        let return_type = func_sig.return_type.clone();
        let fn_value = self.get_or_declare_func(method_symbol_id, func_sig.clone());

        let mut args: Vec<BasicMetadataValueEnum<'a>> = method_call
            .args
            .iter()
            .map(|typed_expr| {
                let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
                self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue)
                    .as_basic_value()
                    .into()
            })
            .collect();

        if let Some(first_param) = func_sig.params.list.first() {
            if let TypedFuncParamKind::SelfModifier(typed_self_modifier) = first_param {
                let mut operand_lvalue = self.build_expr(local_scope_opt.clone(), &method_call.operand);

                if method_call.operand.concrete_type.clone().unwrap().is_pointer() {
                    operand_lvalue = self.build_dereference(
                        local_scope_opt.clone(),
                        &TypedDereference {
                            operand: method_call.operand.clone(),
                            loc: method_call.loc.clone(),
                        },
                    );
                }

                match typed_self_modifier.kind {
                    SelfModifierKind::Copied => {
                        let operand_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt, operand_lvalue);
                        args.insert(0, operand_rvalue.as_basic_value().into());
                    }
                    SelfModifierKind::Referenced => {
                        // Pass operand as basic value (after optional dereference)
                        args.insert(0, operand_lvalue.as_basic_value().into());
                    }
                }
            }
        }

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

    fn build_func_type_call(
        &mut self,
        fn_pointer: PointerValue<'a>,
        lowered_args: &Vec<BasicMetadataValueEnum<'a>>,
        func_type: &TypedFuncType,
    ) -> InternalValue<'a> {
        let fn_type = self.build_func_type(func_type.params.clone(), *func_type.return_type.clone());
        let call_result = self
            .llvmbuilder
            .build_indirect_call(fn_type, fn_pointer, &lowered_args, "indirectcall")
            .unwrap()
            .try_as_basic_value();

        if let Some(_) = call_result.right() {
            let null_literal =
                BasicValueEnum::PointerValue(self.llvmctx.ptr_type(AddressSpace::default()).const_null());

            InternalValue::new(*func_type.return_type.clone(), InternalValueKind::RValue(null_literal))
        } else if let Some(basic_value) = call_result.left() {
            InternalValue::new(*func_type.return_type.clone(), InternalValueKind::RValue(basic_value))
        } else {
            unreachable!()
        }
    }

    fn build_func_call(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        func_call: &TypedFuncCall,
    ) -> InternalValue<'a> {
        let lvalue = self.build_expr(local_scope_opt.clone(), &func_call.operand);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

        let lowered_args = self.build_func_args(local_scope_opt.clone(), &func_call.args);

        if rvalue.as_basic_value().is_pointer_value() {
            let func_type = rvalue.value_type.as_func_type().unwrap();
            self.build_func_type_call(rvalue.as_basic_value().into_pointer_value(), &lowered_args, func_type)
        } else {
            let fn_value = rvalue.as_func_value().unwrap();

            let call_result = self
                .llvmbuilder
                .build_call(fn_value, &lowered_args, "call")
                .unwrap()
                .try_as_basic_value();

            if let Some(_) = call_result.right() {
                let null_literal =
                    BasicValueEnum::PointerValue(self.llvmctx.ptr_type(AddressSpace::default()).const_null());
                InternalValue::new(rvalue.value_type, InternalValueKind::RValue(null_literal))
            } else if let Some(basic_value) = call_result.left() {
                InternalValue::new(rvalue.value_type, InternalValueKind::RValue(basic_value))
            } else {
                unreachable!()
            }
        }
    }

    fn build_func_args(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        args: &Vec<TypedExpression>,
    ) -> Vec<BasicMetadataValueEnum<'a>> {
        args.iter()
            .map(|typed_expr| {
                let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
                self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue)
                    .as_basic_value()
                    .into()
            })
            .collect()
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

    pub(crate) fn build_cmp_eq(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        lhs_rvalue: InternalValue<'a>,
        rhs_rvalue: InternalValue<'a>,
    ) -> InternalValue<'a> {
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
            _ => {
                if lhs_rvalue.value_type.is_enum() && rhs_rvalue.value_type.is_enum() {
                    return self.build_compare_enum_variants(
                        local_scope_opt,
                        lhs_rvalue.clone(),
                        rhs_rvalue.clone(),
                        true,
                    );
                }

                unreachable!()
            }
        }
    }

    pub(crate) fn build_cmp_neq(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        lhs_rvalue: InternalValue<'a>,
        rhs_rvalue: InternalValue<'a>,
    ) -> InternalValue<'a> {
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
            _ => {
                if lhs_rvalue.value_type.is_enum() && rhs_rvalue.value_type.is_enum() {
                    return self.build_compare_enum_variants(
                        local_scope_opt,
                        lhs_rvalue.clone(),
                        rhs_rvalue.clone(),
                        false,
                    );
                }

                unreachable!()
            }
        }
    }

    fn build_null_coalescing_pointers(&self, lhs: PointerValue<'a>, rhs: PointerValue<'a>) -> InternalValue<'a> {
        // cond: lhs == null
        let is_null = self
            .llvmbuilder
            .build_is_null(lhs, "lhs_is_null")
            .expect("icmp eq null");

        let selected = self
            .llvmbuilder
            .build_select(is_null, rhs, lhs, "null_coalesce")
            .expect("select")
            .into_pointer_value();

        InternalValue::new(
            ConcreteType::Pointer(Box::new(ConcreteType::BasicType(BasicConcreteType::Void))),
            InternalValueKind::RValue(selected.as_basic_value_enum()),
        )
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
                self.build_null_coalescing_pointers(lhs, rhs)
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
        let lhs_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lhs_lvalue.clone());
        let rhs_rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), rhs_lvalue.clone());

        let get_signed = || rhs_rvalue.value_type.as_basic_type().unwrap().is_signed();

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
            InfixOperator::Equal => self.build_cmp_eq(local_scope_opt, lhs_rvalue, rhs_rvalue),
            InfixOperator::NotEqual => self.build_cmp_neq(local_scope_opt, lhs_rvalue, rhs_rvalue),
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

    fn build_sizeof_type(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        concrete_type: &ConcreteType,
    ) -> InternalValue<'a> {
        let any_type = self.build_concrete_type(local_scope_opt.clone(), concrete_type.clone());
        let size_internal_value = InternalValue::new(
            ConcreteType::BasicType(BasicConcreteType::SizeT),
            InternalValueKind::RValue(BasicValueEnum::IntValue(any_type.size_of().unwrap())),
        );
        return self.build_implicit_cast(
            local_scope_opt,
            ConcreteType::BasicType(BasicConcreteType::SizeT),
            size_internal_value,
        );
    }

    fn build_sizeof_expr(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        typed_expr: &TypedExpression,
    ) -> InternalValue<'a> {
        let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
        let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);

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

    fn build_sizeof(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        sizeof_expr: &TypedSizeOfExpression,
    ) -> InternalValue<'a> {
        match &sizeof_expr.expr.kind {
            TypedExpressionKind::ConcreteType(concrete_type) => self.build_sizeof_type(local_scope_opt, concrete_type),
            TypedExpressionKind::Symbol(symbol_id, _) => {
                let local_or_global_symbol = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt.clone(), *symbol_id)
                    .unwrap();

                if local_or_global_symbol.as_struct().is_some()
                    || local_or_global_symbol.as_enum().is_some()
                    || local_or_global_symbol.as_union().is_some()
                {
                    return self.build_sizeof_type(
                        local_scope_opt,
                        &ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(*symbol_id)),
                    );
                }

                self.build_sizeof_expr(local_scope_opt, &sizeof_expr.expr)
            }
            _ => self.build_sizeof_expr(local_scope_opt, &sizeof_expr.expr),
        }
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
        }
    }

    fn get_or_declare_lvalue(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> LocalIRValue<'a> {
        let local_or_global_symbol = match self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)
        {
            Some(local_or_global_symbol) => local_or_global_symbol,
            None => {
                panic!("Undefined type symbol.")
            }
        };

        match &local_or_global_symbol {
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::GlobalVar(resolved_global_var) => {
                    let concrete_type = resolved_global_var.global_var_sig.ty.clone().unwrap();
                    let global_value = self.get_or_declare_global_var(resolved_global_var.global_var_sig.clone());

                    LocalIRValue::GlobalValue(global_value, concrete_type)
                }
                _ => {
                    if let Some(resolved_func) = local_or_global_symbol.as_func() {
                        return LocalIRValue::Func(
                            self.get_or_declare_func(resolved_func.symbol_id, resolved_func.func_sig.clone()),
                            ConcreteType::FuncType(typed_func_type_from_func_sig(&resolved_func.func_sig)),
                        );
                    }

                    unreachable!()
                }
            },
            LocalOrGlobalSymbol::LocalSymbol(..) => unreachable!(),
        }
    }

    fn build_lvalue_with_symbol_id(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
    ) -> InternalValue<'a> {
        let local_ir_value_opt = self.get_ir_value(symbol_id);

        let local_ir_value = match local_ir_value_opt {
            Some(local_ir_value) => local_ir_value,
            None => self.get_or_declare_lvalue(local_scope_opt, symbol_id),
        };

        let (pointer, concrete_type) = match local_ir_value.as_lvalue() {
            Some((pointer, concrete_type)) => (pointer.clone(), concrete_type.clone()),
            None => match local_ir_value.as_rvalue() {
                Some((basic_value, concrete_type)) => {
                    return InternalValue::new(concrete_type.clone(), InternalValueKind::RValue(basic_value.clone()));
                }
                None => match local_ir_value.as_global_value() {
                    Some((global_value, concrete_type)) => {
                        (global_value.as_pointer_value().clone(), concrete_type.clone())
                    }
                    None => match local_ir_value.as_func() {
                        Some((fn_value, concrete_type)) => {
                            return InternalValue::new(concrete_type.clone(), InternalValueKind::FuncValue(*fn_value));
                        }
                        None => {
                            panic!("Couldn't find any lvalue with this symbol id.")
                        }
                    },
                },
            },
        };

        let internal_value = InternalValue::new(concrete_type, InternalValueKind::LValue(pointer));
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
                let signed = literal
                    .ty
                    .clone()
                    .unwrap()
                    .get_const_inner()
                    .as_basic_type()
                    .unwrap()
                    .is_signed();

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
                        StringPrefix::C => self.build_c_style_string(value.clone(), literal.loc.clone()),
                        StringPrefix::B => self.build_byte_string(value.clone(), literal.loc.clone()),
                    }
                } else {
                    self.build_string_literal(value.clone(), literal.loc.clone())
                }
            }
        };

        InternalValue::new(literal.ty.clone().unwrap(), InternalValueKind::RValue(basic_value))
    }
}
