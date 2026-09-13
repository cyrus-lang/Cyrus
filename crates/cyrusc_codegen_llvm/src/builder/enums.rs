// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_internal::cir::{
    cir::{CIREnumInitExpr, CIREnumInitVariant, CIREnumVariant},
    types::{CIREnumType, CIRType},
};
use cyrusc_typed_ast::types::PlainType;
use inkwell::{
    IntPredicate,
    types::{ArrayType, BasicType, BasicTypeEnum, StructType},
    values::{ArrayValue, BasicValue, BasicValueEnum, IntValue, StructValue},
};

use crate::builder::{
    builder::CodeGenIRBuilder,
    values::{InternalValue, InternalValueKind},
};

impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_enum_init(&mut self, enum_init_expr: &CIREnumInitExpr) -> InternalValue<'ll> {
        let ty = &enum_init_expr.ty;
        let type_id = enum_init_expr.ty.as_type_id().unwrap();
        let enum_type = enum_init_expr.ty.as_enum(&self.tctx).unwrap();

        if enum_type.is_scalar_optimizable() {
            return self.emit_repr_c_enum_init(enum_init_expr, &enum_type);
        }

        let llvm_enum_type = self.emit_enum_type(type_id).into_struct_type();
        let buffer_type = llvm_enum_type.get_field_type_at_index(1).unwrap().into_array_type();

        let cir_tag_type = enum_type.tag_type_or_infer_or_default();
        let tag_type = self.emit_type(*cir_tag_type.clone()).into_int_type();
        let tag_value = tag_type.const_int(enum_init_expr.tag as u64, false);

        let is_global = self.llvm_builder.get_insert_block().is_none();

        if !is_global {
            let value =
                self.emit_enum_init_with_alloca(enum_init_expr, &enum_type, llvm_enum_type, buffer_type, tag_value);

            InternalValue::new(ty.clone(), InternalValueKind::RValue(value))
        } else {
            panic!("if global var includes enum init in it's initializer expression, it must be initialized lazily");
        }
    }

    fn emit_enum_init_with_alloca(
        &mut self,
        enum_init_expr: &CIREnumInitExpr,
        cir_enum_type: &CIREnumType,
        enum_struct_type: StructType<'ll>,
        buffer_type: ArrayType<'ll>,
        tag_value: IntValue<'ll>,
    ) -> BasicValueEnum<'ll> {
        let enum_alloca = self.alloca_with_scope_lifetime(enum_struct_type.as_basic_type_enum(), "enum.alloca");

        let tag_ptr = self
            .llvm_builder
            .build_struct_gep(enum_struct_type, enum_alloca, 0, "enum.tag.ptr")
            .unwrap();

        let payload_ptr = self
            .llvm_builder
            .build_struct_gep(enum_struct_type, enum_alloca, 1, "enum.payload.ptr")
            .unwrap();

        self.llvm_builder.build_store(tag_ptr, tag_value).unwrap();

        // IMPORTANT: Zero out the entire payload buffer first
        // to ensure padded slots value is consistent.
        // If we don't do this, it may cause UB when
        // comparing two equal enums.

        match &enum_init_expr.variant {
            CIREnumInitVariant::Unit => {
                let payload_size = buffer_type.size_of().unwrap();

                self.llvm_builder
                    .build_memset(payload_ptr, 1, self.llvm_ctx.i8_type().const_zero(), payload_size)
                    .unwrap();
            }
            CIREnumInitVariant::Valued(expr) => {
                let lvalue = self.emit_expr(expr, &None);
                let rvalue = self.load_rvalue(lvalue);

                self.intrinsic_optimized_memcpy(payload_ptr, rvalue.as_basic_value());
            }
            CIREnumInitVariant::Payload(field_exprs) => {
                let field_types: Vec<BasicTypeEnum<'ll>> = field_exprs
                    .iter()
                    .map(|fld| self.emit_type(fld.ty.clone()).try_into().unwrap())
                    .collect();

                let payload_struct_type = self.llvm_ctx.struct_type(&field_types, false);

                let mut payload_struct_value = payload_struct_type.const_zero();

                for (i, field_expr) in field_exprs.iter().enumerate() {
                    let lvalue = self.emit_expr(&field_expr, &None);
                    let mut rvalue = self.load_rvalue(lvalue);

                    let payload_type = match cir_enum_type.lookup_variant(&enum_init_expr.ident).unwrap() {
                        CIREnumVariant::Payload(_, struct_type, _) => struct_type,
                        _ => unreachable!(),
                    };

                    let field_type = payload_type.fields.get(i).unwrap();

                    if !self.llvm_builder.get_insert_block().is_none() {
                        rvalue = self.emit_implicit_cast(field_type, rvalue);
                    }

                    payload_struct_value = self
                        .llvm_builder
                        .build_insert_value(
                            payload_struct_value,
                            rvalue.as_basic_value(),
                            i as u32,
                            "enum.payload.set_field",
                        )
                        .unwrap()
                        .into_struct_value();
                }

                self.intrinsic_optimized_memcpy(payload_ptr, payload_struct_value.as_basic_value_enum());
            }
        }

        self.llvm_builder
            .build_load(enum_struct_type, enum_alloca, "enum.load")
            .unwrap()
    }

    fn emit_repr_c_enum_init(
        &mut self,
        enum_init_expr: &CIREnumInitExpr,
        enum_type: &CIREnumType,
    ) -> InternalValue<'ll> {
        let ty = enum_init_expr.ty.clone();

        let cir_tag_type = enum_type.tag_type_or_infer_or_default();
        let tag_type = self.emit_type(*cir_tag_type.clone()).into_int_type();
        let tag_value = tag_type.const_int(enum_init_expr.tag.try_into().unwrap(), cir_tag_type.is_signed_integer());

        InternalValue::new(ty, InternalValueKind::RValue(tag_value.as_basic_value_enum()))
    }

    #[inline]
    pub(crate) fn extract_enum_tag(&self, struct_value: StructValue<'ll>) -> IntValue<'ll> {
        self.llvm_builder
            .build_extract_value(struct_value, 0, "extract")
            .unwrap()
            .into_int_value()
    }

    #[inline]
    pub(crate) fn extract_enum_payload(&self, struct_value: StructValue<'ll>) -> ArrayValue<'ll> {
        self.llvm_builder
            .build_extract_value(struct_value, 1, "extract")
            .unwrap()
            .into_array_value()
    }

    pub(crate) fn emit_compare_enum_variants(
        &mut self,
        lhs: InternalValue<'ll>,
        rhs: InternalValue<'ll>,
        tag_type: &CIRType,
        cmp_eq: bool,
    ) -> InternalValue<'ll> {
        let struct_value1 = lhs.as_basic_value().into_struct_value();
        let struct_value2 = rhs.as_basic_value().into_struct_value();

        let tag1 = self.extract_enum_tag(struct_value1);
        let tag2 = self.extract_enum_tag(struct_value2);

        let lhs_tag = InternalValue::new(tag_type.clone(), InternalValueKind::RValue(tag1.into()));
        let rhs_tag = InternalValue::new(tag_type.clone(), InternalValueKind::RValue(tag2.into()));

        let tag_result = if cmp_eq {
            self.emit_cmp_eq(lhs_tag, rhs_tag)
        } else {
            self.emit_cmp_neq(lhs_tag, rhs_tag)
        };

        let tag_result_int_value = tag_result.as_basic_value().into_int_value();

        let current_func = self.cur_func.unwrap();
        let payload_block = self.llvm_ctx.append_basic_block(current_func, "compare.enum.payload");
        let exit_block = self.llvm_ctx.append_basic_block(current_func, "compare.enum.exit");

        let (branch_true, branch_false) = if cmp_eq {
            (payload_block, exit_block)
        } else {
            (exit_block, payload_block)
        };

        let entry_block = self.block_reg.cur_block.unwrap();

        self.llvm_builder
            .build_conditional_branch(tag_result.as_basic_value().into_int_value(), branch_true, branch_false)
            .unwrap();

        self.emit_basic_block(payload_block);

        let payload1 = self.extract_enum_payload(struct_value1);
        let payload2 = self.extract_enum_payload(struct_value2);

        let memcmp_result = self.intrinsic_compare_array_values(payload1, payload2);

        let zero_int = self.llvm_ctx.i32_type().const_zero();

        let predicate = if cmp_eq { IntPredicate::EQ } else { IntPredicate::NE };

        // comparison result
        let payload_result = self
            .llvm_builder
            .build_int_compare(predicate, memcmp_result, zero_int, "compare.payload.is_zero")
            .unwrap();

        let payload_result_bool = self.int_value_as_bool_i1(payload_result);

        self.llvm_builder.build_unconditional_branch(exit_block).unwrap();

        self.emit_basic_block(exit_block);

        let phi = self
            .llvm_builder
            .build_phi(self.llvm_ctx.bool_type(), "compare.enum")
            .unwrap();

        phi.add_incoming(&[(&tag_result_int_value, entry_block)]);
        phi.add_incoming(&[(&payload_result_bool, payload_block)]);

        InternalValue::new(
            CIRType::Plain(PlainType::Bool),
            InternalValueKind::RValue(phi.as_basic_value()),
        )
    }
}
