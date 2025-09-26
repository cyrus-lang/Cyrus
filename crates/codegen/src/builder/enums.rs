use crate::builder::{
    abi::generate_enum_variant_abi_name,
    module::{CodeGenBuilder, LocalIRValue},
    values::{InternalValue, InternalValueKind},
};
use ast::source_loc::SourceLoc;
use inkwell::{
    AddressSpace,
    module::Linkage,
    types::{AnyType, ArrayType, BasicTypeEnum, StructType},
    values::{ArrayValue, BasicValue, BasicValueEnum, IntValue, StructValue},
};
use resolver::{
    declsign::EnumSig,
    scope::{LocalScopeRef, ResolvedEnum},
};
use typed_ast::{
    TypedEnum, TypedEnumValuedField, TypedEnumVariant, TypedExpression, TypedUnnamedStructValue,
    TypedUnnamedStructValueField,
    types::{BasicConcreteType, ConcreteType, ResolvedSymbol, TypedUnnamedStructType, TypedUnnamedStructTypeField},
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

        let tag_concrete_type = ConcreteType::BasicType(BasicConcreteType::UInt32);
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

        let memcmp_result = self.build_memcmp_for_arrays(payload1, payload2);

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
            ConcreteType::BasicType(BasicConcreteType::Bool),
            InternalValueKind::RValue(phi.as_basic_value()),
        )
    }

    fn build_memcmp_for_arrays(&self, lhs_arr: ArrayValue<'a>, rhs_arr: ArrayValue<'a>) -> IntValue<'a> {
        let i32_type = self.llvmctx.i32_type();
        let i8_ptr_type = self.llvmctx.ptr_type(AddressSpace::default());
        let target_data = self.llvmtm.get_target_data();
        let ptr_sized_int_type = self.llvmctx.ptr_sized_int_type(&target_data, None);

        let module = self.llvmmodule.borrow();
        let memcmp = match module.get_function("memcmp") {
            Some(func) => func,
            None => {
                let fn_type = i32_type.fn_type(
                    &[
                        i8_ptr_type.into(),        // const void* lhs
                        i8_ptr_type.into(),        // const void* rhs
                        ptr_sized_int_type.into(), // size_t len
                    ],
                    false,
                );
                module.add_function("memcmp", fn_type, None)
            }
        };

        let lhs_alloca = self.llvmbuilder.build_alloca(lhs_arr.get_type(), "lhs_alloca").unwrap();
        let rhs_alloca = self.llvmbuilder.build_alloca(rhs_arr.get_type(), "rhs_alloca").unwrap();

        self.llvmbuilder.build_store(lhs_alloca, lhs_arr).unwrap();
        self.llvmbuilder.build_store(rhs_alloca, rhs_arr).unwrap();

        let zero = self.llvmctx.i32_type().const_zero();
        let gep_idx = &[zero, zero];
        let lhs_ptr = unsafe {
            self.llvmbuilder
                .build_in_bounds_gep(lhs_arr.get_type(), lhs_alloca, gep_idx, "lhs_gep")
                .unwrap()
        };
        let rhs_ptr = unsafe {
            self.llvmbuilder
                .build_in_bounds_gep(rhs_arr.get_type(), rhs_alloca, gep_idx, "rhs_gep")
                .unwrap()
        };

        let lhs_cast = self
            .llvmbuilder
            .build_pointer_cast(lhs_ptr, i8_ptr_type, "lhs_cast")
            .unwrap();
        let rhs_cast = self
            .llvmbuilder
            .build_pointer_cast(rhs_ptr, i8_ptr_type, "rhs_cast")
            .unwrap();

        let byte_size = target_data.get_bit_size(&lhs_arr.get_type()) / 8;
        let len_val = ptr_sized_int_type.const_int(byte_size as u64, false);

        let cmp = self
            .llvmbuilder
            .build_call(
                memcmp,
                &[lhs_cast.into(), rhs_cast.into(), len_val.into()],
                "memcmp_call",
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap();

        drop(module);
        cmp.into_int_value()
    }

    pub(crate) fn copy_buffer_to_struct(&self, buffer: ArrayValue<'a>, struct_type: StructType<'a>) -> StructValue<'a> {
        let struct_alloca = self.llvmbuilder.build_alloca(struct_type, "struct_alloca").unwrap();

        let buffer_alloca = self
            .llvmbuilder
            .build_alloca(buffer.get_type(), "buffer_alloca")
            .unwrap();
        self.llvmbuilder.build_store(buffer_alloca, buffer).unwrap();

        let i8_ptr_type = self.llvmctx.ptr_type(AddressSpace::default());
        let dest_i8_ptr = self
            .llvmbuilder
            .build_pointer_cast(struct_alloca, i8_ptr_type, "dest_i8")
            .unwrap();
        let src_i8_ptr = self
            .llvmbuilder
            .build_pointer_cast(buffer_alloca, i8_ptr_type, "src_i8")
            .unwrap();

        let struct_size = struct_type.size_of().unwrap();
        self.llvmbuilder
            .build_memcpy(dest_i8_ptr, 1, src_i8_ptr, 1, struct_size)
            .unwrap();

        self.llvmbuilder
            .build_load(struct_type, struct_alloca, "load_struct")
            .unwrap()
            .into_struct_value()
    }

    fn copy_payload_to_buffer(&self, src_value: BasicValueEnum<'a>, dest_array_type: ArrayType<'a>) -> ArrayValue<'a> {
        let array_alloca = self
            .llvmbuilder
            .build_alloca(dest_array_type, &format!("alloca"))
            .unwrap();

        let src_ptr = match src_value {
            BasicValueEnum::PointerValue(ptr) => ptr,
            _ => {
                let tmp_alloca = self.llvmbuilder.build_alloca(src_value.get_type(), "tmp").unwrap();
                self.llvmbuilder.build_store(tmp_alloca, src_value).unwrap();
                tmp_alloca
            }
        };

        let i8_ptr_type = self.llvmctx.ptr_type(AddressSpace::default());
        let dest_i8_ptr = self
            .llvmbuilder
            .build_pointer_cast(array_alloca, i8_ptr_type, "dest_i8")
            .unwrap();
        let src_i8_ptr = self
            .llvmbuilder
            .build_pointer_cast(src_ptr, i8_ptr_type, "src_i8")
            .unwrap();

        let array_size = dest_array_type.size_of().unwrap();

        self.llvmbuilder
            .build_memcpy(dest_i8_ptr, 1, src_i8_ptr, 1, array_size)
            .unwrap();

        self.llvmbuilder
            .build_load(dest_array_type, array_alloca, &format!("load"))
            .unwrap()
            .into_array_value()
    }

    pub(crate) fn build_enum_valued_field_variant_struct_type(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        enum_valued_fields: Vec<TypedEnumValuedField>,
    ) -> StructType<'a> {
        let mut fields: Vec<TypedUnnamedStructTypeField> = Vec::new();

        for (idx, valued_field) in enum_valued_fields.iter().enumerate() {
            fields.push(TypedUnnamedStructTypeField {
                field_name: format!("field{}", idx),
                field_type: Box::new(valued_field.field_type.clone()),
                loc: valued_field.loc.clone(),
            });
        }

        let unnamed_struct_type = TypedUnnamedStructType {
            fields,
            packed: false,
            loc: SourceLoc::default(),
        };

        self.build_unnamed_struct_type(local_scope_opt, &unnamed_struct_type)
            .try_into()
            .unwrap()
    }

    pub(crate) fn build_construct_enum_variant(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        resolved_enum: &ResolvedEnum,
        variant_name: String,
        args: &Vec<TypedExpression>,
    ) -> InternalValue<'a> {
        let (enum_struct_type, enum_payload_type) = {
            let irreg = self.irreg.borrow();
            let local_ir_value = irreg.get(&resolved_enum.symbol_id).unwrap();
            let enum_ir_value = local_ir_value.as_enum().unwrap().clone();
            let (struct_type, payload_type) = (enum_ir_value.0.clone(), enum_ir_value.1.clone());
            drop(irreg);
            (struct_type, payload_type)
        };

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
            TypedEnumVariant::Identifier(..) => unreachable!(),
            TypedEnumVariant::Valued(..) => unreachable!(),
            TypedEnumVariant::Variant(_, enum_valued_fields) => {
                let mut fields: Vec<TypedUnnamedStructValueField> = Vec::new();

                for (idx, valued_field) in enum_valued_fields.iter().enumerate() {
                    let arg = &args[idx];

                    fields.push(TypedUnnamedStructValueField {
                        field_name: format!("field{}", idx),
                        field_type: Some(valued_field.field_type.clone()),
                        field_value: Box::new(arg.clone()),
                        loc: valued_field.loc.clone(),
                    });
                }

                let unnamed_struct_type = TypedUnnamedStructType {
                    fields: fields
                        .iter()
                        .map(|field| TypedUnnamedStructTypeField {
                            field_name: field.field_name.clone(),
                            field_type: Box::new(field.field_type.clone().unwrap()),
                            loc: field.loc.clone(),
                        })
                        .collect(),
                    packed: false,
                    loc: SourceLoc::default(),
                };

                let unnamed_struct_value = TypedUnnamedStructValue {
                    unnamed_struct_type: Some(unnamed_struct_type),
                    fields,
                    packed: false,
                    is_const: true,
                    loc: SourceLoc::default(),
                };

                let payload_internal_value =
                    self.build_unnamed_struct_value(local_scope_opt.clone(), &unnamed_struct_value);
                let payload_basic_value = payload_internal_value.as_basic_value();

                let copied_payload = self.copy_payload_to_buffer(payload_basic_value, enum_payload_type);

                enum_struct_value = self
                    .llvmbuilder
                    .build_insert_value(enum_struct_value, copied_payload, 1, "set")
                    .unwrap()
                    .into_struct_value();
            }
        }

        InternalValue::new(
            ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(resolved_enum.symbol_id)),
            InternalValueKind::RValue(enum_struct_value.as_basic_value_enum()),
        )
    }

    pub(crate) fn build_construct_enum_variant_no_field(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        resolved_enum: &ResolvedEnum,
        variant_name: String,
    ) -> InternalValue<'a> {
        let (enum_struct_type, enum_payload_type) = {
            let irreg = self.irreg.borrow();
            let local_ir_value = irreg.get(&resolved_enum.symbol_id).unwrap();
            let enum_ir_value = local_ir_value.as_enum().unwrap().clone();
            let (struct_type, payload_type) = (enum_ir_value.0.clone(), enum_ir_value.1.clone());
            drop(irreg);
            (struct_type, payload_type)
        };

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
                    self.get_module_name(resolved_enum.module_id),
                    resolved_enum.enum_sig.name.clone(),
                    identifier.as_string(),
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

                let copied_payload = self.copy_payload_to_buffer(payload_value, enum_payload_type);

                enum_struct_value = self
                    .llvmbuilder
                    .build_insert_value(enum_struct_value, copied_payload, 1, "set")
                    .unwrap()
                    .into_struct_value();
            }
            TypedEnumVariant::Variant(..) => unreachable!(),
        }

        InternalValue::new(
            ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(resolved_enum.symbol_id)),
            InternalValueKind::RValue(enum_struct_value.as_basic_value_enum()),
        )
    }

    pub(crate) fn build_enum_struct_type(&mut self, enum_sig: &EnumSig) -> (StructType<'a>, ArrayType<'a>) {
        let enum_opaque_struct = self.llvmctx.opaque_struct_type(&enum_sig.name);

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

    pub(crate) fn build_enum_def(&mut self, typed_enum: &TypedEnum) {
        let enum_name = typed_enum.name.clone();

        let (enum_struct_type, payload_type) = self.build_enum_struct_type(&typed_enum_as_enum_sig(typed_enum));
        self.insert_forward_decl_to_registry(
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
                        self.get_module_name(self.module_id),
                        enum_name.clone(),
                        identifier.name.clone(),
                    ),
                );
                global_value.set_linkage(Linkage::External);
                global_value.set_initializer(&basic_rvalue);
                global_value.set_constant(true);
            }
        });

        self.build_methods(typed_enum.module_id, &typed_enum.methods);
    }

    pub(crate) fn build_local_enum_def(&self, _typed_enum: &TypedEnum) {
        todo!();
    }
}

fn typed_enum_as_enum_sig(typed_enum: &TypedEnum) -> EnumSig {
    EnumSig {
        symbol_id: typed_enum.symbol_id,
        name: typed_enum.name.clone(),
        methods: typed_enum.methods.clone(),
        variants: typed_enum.variants.clone(),
        vis: typed_enum.vis.clone(),
        loc: typed_enum.loc.clone(),
    }
}
