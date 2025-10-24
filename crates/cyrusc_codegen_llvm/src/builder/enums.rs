use crate::builder::{
    abi::{generate_enum_abi_name, generate_enum_variant_abi_name},
    module::{CodeGenBuilder, LocalIRValue},
    values::{InternalValue, InternalValueKind},
};
use inkwell::{
    module::Linkage,
    types::{AnyType, ArrayType, BasicTypeEnum, StructType},
    values::{ArrayValue, BasicValue, BasicValueEnum, IntValue, StructValue},
};
use resolver::{
    scope::{LocalScopeRef, ResolvedEnum},
    sigs::EnumSig,
    typed_enum_as_enum_sig,
};
use tast::{
    TypedEnumStmt, TypedEnumValuedField, TypedEnumVariant, TypedExprStmt, TypedTypeArgs,
    types::{BasicType, SemanticType, ResolvedSymbol},
};

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_enum_extract_index(&self, struct_value: StructValue<'a>) -> IntValue<'a> {
        self.llvmbuilder
            .build_extract_value(struct_value, 0, "extract")
            .unwrap()
            .into_int_value()
    }

    pub(crate) fn build_enum_extract_payload(&self, struct_value: StructValue<'a>) -> ArrayValue<'a> {
        self.llvmbuilder
            .build_extract_value(struct_value, 1, "extract")
            .unwrap()
            .into_array_value()
    }

    pub(crate) fn build_compare_enum_variants(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        lhs: InternalValue<'a>,
        rhs: InternalValue<'a>,
        cmp_eq: bool,
    ) -> InternalValue<'a> {
        let struct_value1 = lhs.as_basic_value().into_struct_value();
        let struct_value2 = rhs.as_basic_value().into_struct_value();

        let tag1 = self.build_enum_extract_index(struct_value1);
        let tag2 = self.build_enum_extract_index(struct_value2);

        let tag_concrete_type = SemanticType::BasicType(BasicType::UInt32);
        let tag_cmp_result = if cmp_eq {
            self.build_cmp_eq(
                local_scope_opt,
                InternalValue::new(
                    tag_concrete_type.clone(),
                    InternalValueKind::RValue(tag1.as_basic_value_enum()),
                ),
                InternalValue::new(tag_concrete_type, InternalValueKind::RValue(tag2.as_basic_value_enum())),
            )
        } else {
            self.build_cmp_neq(
                local_scope_opt,
                InternalValue::new(
                    tag_concrete_type.clone(),
                    InternalValueKind::RValue(tag1.as_basic_value_enum()),
                ),
                InternalValue::new(tag_concrete_type, InternalValueKind::RValue(tag2.as_basic_value_enum())),
            )
        };

        let current_func = self.blockreg.current_func_ref.unwrap();
        let payload_block = self.llvmctx.append_basic_block(current_func, "enum_cmp.payload");
        let exit_block = self.llvmctx.append_basic_block(current_func, "enum_cmp.exit");

        let entry_block = self.blockreg.current_block_ref.unwrap();

        self.llvmbuilder
            .build_conditional_branch(
                tag_cmp_result.as_basic_value().into_int_value(),
                payload_block,
                exit_block,
            )
            .unwrap();

        self.blockreg.current_block_ref = Some(payload_block);
        self.llvmbuilder.position_at_end(payload_block);

        let payload1 = self.build_enum_extract_payload(struct_value1);
        let payload2 = self.build_enum_extract_payload(struct_value2);

        let memcmp_result = self.intrinsic_array_memcmp(payload1, payload2);

        let i32_zero = self.llvmctx.i32_type().const_zero();
        let payload_eq = self
            .llvmbuilder
            .build_int_compare(inkwell::IntPredicate::EQ, memcmp_result, i32_zero, "payload_eq")
            .unwrap();

        self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();

        let payload_cmp_block = self.blockreg.current_block_ref.unwrap();

        self.blockreg.current_block_ref = Some(exit_block);
        self.llvmbuilder.position_at_end(exit_block);

        let phi = self
            .llvmbuilder
            .build_phi(self.llvmctx.bool_type(), "enum_cmp_phi")
            .unwrap();

        phi.add_incoming(&[(&self.llvmctx.bool_type().const_zero(), entry_block)]);
        phi.add_incoming(&[(&payload_eq, payload_cmp_block)]);

        InternalValue::new(
            SemanticType::BasicType(BasicType::Bool),
            InternalValueKind::RValue(phi.as_basic_value()),
        )
    }

    pub(crate) fn build_enum_valued_field_variant_struct_type(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        enum_valued_fields: Vec<TypedEnumValuedField>,
    ) -> StructType<'a> {
        let fields: Vec<BasicTypeEnum<'a>> = enum_valued_fields
            .iter()
            .map(|valued_field| {
                self.build_concrete_type(local_scope_opt.clone(), valued_field.field_type.clone())
                    .try_into()
                    .unwrap()
            })
            .collect();

        self.llvmctx.struct_type(&fields, false)
    }

    pub(crate) fn build_construct_enum_variant(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        resolved_enum: &ResolvedEnum,
        variant_name: String,
        args: &Vec<TypedExprStmt>,
        type_args: &Option<TypedTypeArgs>,
    ) -> InternalValue<'a> {
        let (enum_struct_type, enum_payload_type) = self.get_or_declare_enum_monomorph(resolved_enum, type_args);
        let enum_sig = if let Some(type_args) = type_args {
            let normalized_args = self.get_normalized_type_args(type_args);
            self.substitute_enum_sig_generic_params(resolved_enum.enum_sig.clone(), &normalized_args)
        } else {
            resolved_enum.enum_sig.clone()
        };

        let enum_variant_idx = enum_sig
            .variants
            .iter()
            .position(|variant| variant.get_identifier().as_string() == variant_name)
            .unwrap();
        let enum_variant = &enum_sig.variants[enum_variant_idx];

        let mut enum_struct_value = enum_struct_type.const_zero();

        let variant_number = self
            .llvmctx
            .i32_type()
            .const_int(enum_variant_idx.try_into().unwrap(), false);

        enum_struct_value = self
            .llvmbuilder
            .build_insert_value(enum_struct_value, variant_number, 0, "set")
            .unwrap()
            .into_struct_value();

        if let TypedEnumVariant::Variant(_, enum_valued_fields) = enum_variant {
            let fields: Vec<BasicValueEnum<'a>> = enum_valued_fields
                .iter()
                .enumerate()
                .map(|(idx, enum_valued_field)| {
                    let arg = &args[idx];
                    let lvalue = self.build_expr(local_scope_opt.clone(), arg);
                    let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
                    let casted_rvalue =
                        self.build_implicit_cast(local_scope_opt.clone(), enum_valued_field.field_type.clone(), rvalue);

                    casted_rvalue.as_basic_value()
                })
                .collect();

            let payload_basic_value = self.llvmctx.const_struct(&fields, false);

            let copied_payload =
                self.intrinsic_copy_payload_to_buffer(BasicValueEnum::StructValue(payload_basic_value), enum_payload_type);

            enum_struct_value = self
                .llvmbuilder
                .build_insert_value(enum_struct_value, copied_payload, 1, "set")
                .unwrap()
                .into_struct_value();
        }

        InternalValue::new(
            SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(resolved_enum.symbol_id)),
            InternalValueKind::RValue(enum_struct_value.as_basic_value_enum()),
        )
    }

    pub(crate) fn build_construct_enum_variant_no_field(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        resolved_enum: &ResolvedEnum,
        type_args: &Option<TypedTypeArgs>,
        variant_name: String,
    ) -> InternalValue<'a> {
        let (enum_struct_type, enum_payload_type) = self.get_or_declare_enum_monomorph(resolved_enum, type_args);

        let enum_variant_idx = resolved_enum
            .enum_sig
            .variants
            .iter()
            .position(|variant| variant.get_identifier().as_string() == variant_name)
            .unwrap();
        let enum_variant = &resolved_enum.enum_sig.variants[enum_variant_idx];

        let mut enum_struct_value = enum_struct_type.const_zero();

        let variant_number = self
            .llvmctx
            .i32_type()
            .const_int(enum_variant_idx.try_into().unwrap(), false);

        enum_struct_value = self
            .llvmbuilder
            .build_insert_value(enum_struct_value, variant_number, 0, "set")
            .unwrap()
            .into_struct_value();

        match enum_variant {
            TypedEnumVariant::Identifier(..) => {}
            TypedEnumVariant::Valued(identifier, typed_expr) => {
                let abi_name = generate_enum_variant_abi_name(
                    &self.get_module_name(resolved_enum.module_id),
                    &resolved_enum.enum_sig.name,
                    &identifier.as_string(),
                );

                let module = self.llvmmodule.borrow();
                let global_value_opt = module.get_global(&abi_name);
                drop(module);

                let payload_value = if let Some(global_value) = global_value_opt {
                    global_value.as_basic_value_enum()
                } else {
                    let lvalue = self.build_expr(local_scope_opt.clone(), typed_expr);
                    let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt, lvalue);
                    rvalue.as_basic_value()
                };

                let copied_payload = self.intrinsic_copy_payload_to_buffer(payload_value, enum_payload_type);

                enum_struct_value = self
                    .llvmbuilder
                    .build_insert_value(enum_struct_value, copied_payload, 1, "set")
                    .unwrap()
                    .into_struct_value();
            }
            TypedEnumVariant::Variant(..) => unreachable!(),
        }

        InternalValue::new(
            SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(resolved_enum.symbol_id)),
            InternalValueKind::RValue(enum_struct_value.as_basic_value_enum()),
        )
    }

    pub(crate) fn build_enum_type(
        &mut self,
        enum_sig: &EnumSig,
        custom_name: Option<String>,
    ) -> (StructType<'a>, ArrayType<'a>) {
        let llvm_struct_name = custom_name.unwrap_or(generate_enum_abi_name(
            &self.get_module_name(self.module_id),
            &enum_sig.name,
        ));
        let enum_opaque_struct = self.llvmctx.opaque_struct_type(&llvm_struct_name);

        let mut payload_size = 0;

        // traverse variants only to find max payload size.
        enum_sig.variants.iter().for_each(|variant| {
            match variant {
                TypedEnumVariant::Identifier(_) => {
                    // no payload
                }
                TypedEnumVariant::Valued(_, typed_expr) => {
                    let lvalue = self.build_expr(None, &**typed_expr);
                    let rvalue = self.build_load_lvalue_to_rvalue(None, lvalue);
                    let basic_rvalue = rvalue.as_basic_value();

                    let store_size = self
                        .llvmtm
                        .get_target_data()
                        .get_store_size(&basic_rvalue.get_type().as_any_type_enum());

                    payload_size = payload_size.max(store_size);
                }
                TypedEnumVariant::Variant(_, enum_valued_fields) => {
                    let variant_fields: Vec<BasicTypeEnum<'a>> = enum_valued_fields
                        .iter()
                        .map(|f| {
                            TryInto::<BasicTypeEnum<'a>>::try_into(self.build_concrete_type(None, f.field_type.clone()))
                                .unwrap()
                        })
                        .collect();

                    let struct_type = self.llvmctx.struct_type(&variant_fields, false);
                    let store_size = self.llvmtm.get_target_data().get_store_size(&struct_type);
                    payload_size = payload_size.max(store_size);
                }
            }
        });

        let payload_type = self.llvmctx.i8_type().array_type(payload_size.try_into().unwrap());
        let field_types: &[BasicTypeEnum<'a>; 2] = &[
            BasicTypeEnum::IntType(self.llvmctx.i32_type()), // tag
            BasicTypeEnum::ArrayType(payload_type.clone()),  // payload
        ];

        enum_opaque_struct.set_body(field_types, false);
        (enum_opaque_struct, payload_type)
    }

    pub(crate) fn build_enum_def(&mut self, typed_enum: &TypedEnumStmt) {
        if typed_enum.generic_params.is_none() {
            let enum_name = typed_enum.name.clone();

            let (enum_struct_type, payload_type) = self.build_enum_type(&typed_enum_as_enum_sig(typed_enum), None);
            self.insert_ir_value(
                typed_enum.symbol_id,
                LocalIRValue::Enum((enum_struct_type, payload_type)),
            );

            typed_enum.variants.iter().for_each(|variant| {
                if let TypedEnumVariant::Valued(identifier, typed_expr) = variant {
                    let lvalue = self.build_expr(None, &**typed_expr);
                    let rvalue = self.build_load_lvalue_to_rvalue(None, lvalue);
                    let basic_rvalue = rvalue.as_basic_value();

                    let module = self.llvmmodule.borrow_mut();
                    let global_value = module.add_global(
                        basic_rvalue.get_type(),
                        None,
                        &generate_enum_variant_abi_name(
                            &self.get_module_name(self.module_id),
                            &enum_name,
                            &identifier.name,
                        ),
                    );
                    global_value.set_linkage(Linkage::External);
                    global_value.set_initializer(&basic_rvalue);
                    global_value.set_constant(true);
                }
            });

            self.build_methods(typed_enum.module_id, &typed_enum.methods);
        } else {
            // generic enum is generated at use time
        }
    }
}
