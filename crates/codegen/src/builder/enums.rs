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
    values::{ArrayValue, BasicValue, BasicValueEnum, StructValue},
};
use resolver::scope::{LocalScopeRef, ResolvedEnum};
use typed_ast::{
    TypedEnum, TypedEnumValuedField, TypedEnumVariant, TypedExpression, TypedUnnamedStructValue,
    TypedUnnamedStructValueField,
    types::{ConcreteType, ResolvedSymbol, TypedUnnamedStructType, TypedUnnamedStructTypeField},
};

impl<'a> CodeGenBuilder<'a> {
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

    pub(crate) fn build_enum_struct_type(
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

    pub(crate) fn build_enum_def(&mut self, typed_enum: &TypedEnum) {
        let local_scope_opt = None;

        let enum_name = typed_enum.name.clone();
        let enum_opaque_struct = {
            let irreg = self.irreg.borrow();
            let local_ir_value = irreg.get(&typed_enum.symbol_id).unwrap();
            let struct_type = local_ir_value.as_enum().unwrap().0.clone();
            drop(irreg);
            struct_type
        };

        let mut enum_iota = 0;
        let mut payload_size = 0;

        typed_enum.variants.iter().for_each(|variant| {
            match variant {
                TypedEnumVariant::Identifier(_) => {
                    // no payload
                }
                TypedEnumVariant::Valued(identifier, typed_expr) => {
                    let lvalue = self.build_expr(local_scope_opt.clone(), &**typed_expr);
                    let rvalue = self.build_load_lvalue_to_rvalue(local_scope_opt.clone(), lvalue);
                    let basic_rvalue = rvalue.as_basic_value();

                    let store_size = {
                        self.llvmtm
                            .get_target_data()
                            .get_store_size(&basic_rvalue.get_type().as_any_type_enum())
                    };

                    if store_size > payload_size {
                        payload_size = store_size;
                    }

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
                    drop(module);
                }
                TypedEnumVariant::Variant(_, enum_valued_fields) => {
                    let variant_fields: Vec<BasicTypeEnum<'a>> = enum_valued_fields
                        .iter()
                        .map(|enum_valued_field| {
                            TryInto::<BasicTypeEnum<'a>>::try_into(
                                self.build_concrete_type(local_scope_opt.clone(), enum_valued_field.field_type.clone()),
                            )
                            .unwrap()
                        })
                        .collect();

                    let struct_type = self.llvmctx.struct_type(&variant_fields, false);
                    let store_size = { self.llvmtm.get_target_data().get_store_size(&struct_type) };
                    if store_size > payload_size {
                        payload_size = store_size;
                    }
                }
            }

            enum_iota += 1;
        });

        let payload_type = self.llvmctx.i8_type().array_type(payload_size.try_into().unwrap());
        let field_types: &[BasicTypeEnum<'a>; 2] = &[
            BasicTypeEnum::IntType(self.llvmctx.i32_type()),
            BasicTypeEnum::ArrayType(payload_type.clone()),
        ];
        enum_opaque_struct.set_body(field_types, false);

        // update irreg because payload_type is evaluated now and previous one is fake (zero cap).
        self.insert_forward_decl_to_registry(
            typed_enum.symbol_id,
            LocalIRValue::Enum((enum_opaque_struct, payload_type)),
        );
        self.build_methods(typed_enum.module_id, &typed_enum.methods);
    }

    pub(crate) fn build_local_enum_def(&self, _typed_enum: &TypedEnum) {
        todo!();
    }
}
