use crate::builder::{builder::IRBuilderCtx, values::InternalValue};

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn build_compare_enum_variants(
        &mut self,
        lhs: InternalValue<'ll>,
        rhs: InternalValue<'ll>,
        cmp_eq: bool,
    ) -> InternalValue<'ll> {
        todo!();

        // let struct_value1 = lhs.as_basic_value().into_struct_value();
        // let struct_value2 = rhs.as_basic_value().into_struct_value();

        // let tag1 = self.build_enum_extract_index(struct_value1);
        // let tag2 = self.build_enum_extract_index(struct_value2);

        // let tag_concrete_type = SemanticType::BasicType(BasicType::UInt32);
        // let tag_cmp_result = if cmp_eq {
        //     self.build_cmp_eq(
        //         local_scope_opt,
        //         InternalValue::new(
        //             tag_concrete_type.clone(),
        //             InternalValueKind::RValue(tag1.as_basic_value_enum()),
        //         ),
        //         InternalValue::new(tag_concrete_type, InternalValueKind::RValue(tag2.as_basic_value_enum())),
        //     )
        // } else {
        //     self.build_cmp_neq(
        //         local_scope_opt,
        //         InternalValue::new(
        //             tag_concrete_type.clone(),
        //             InternalValueKind::RValue(tag1.as_basic_value_enum()),
        //         ),
        //         InternalValue::new(tag_concrete_type, InternalValueKind::RValue(tag2.as_basic_value_enum())),
        //     )
        // };

        // let current_func = self.blockreg.current_func_ref.unwrap();
        // let payload_block = self.llvmctx.append_basic_block(current_func, "enum_cmp.payload");
        // let exit_block = self.llvmctx.append_basic_block(current_func, "enum_cmp.exit");

        // let entry_block = self.blockreg.current_block_ref.unwrap();

        // self.llvmbuilder
        //     .build_conditional_branch(
        //         tag_cmp_result.as_basic_value().into_int_value(),
        //         payload_block,
        //         exit_block,
        //     )
        //     .unwrap();

        // self.blockreg.current_block_ref = Some(payload_block);
        // self.llvmbuilder.position_at_end(payload_block);

        // let payload1 = self.build_enum_extract_payload(struct_value1);
        // let payload2 = self.build_enum_extract_payload(struct_value2);

        // let memcmp_result = self.intrinsic_array_memcmp(payload1, payload2);

        // let i32_zero = self.llvmctx.i32_type().const_zero();
        // let payload_eq = self
        //     .llvmbuilder
        //     .build_int_compare(inkwell::IntPredicate::EQ, memcmp_result, i32_zero, "payload_eq")
        //     .unwrap();

        // self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();

        // let payload_cmp_block = self.blockreg.current_block_ref.unwrap();

        // self.blockreg.current_block_ref = Some(exit_block);
        // self.llvmbuilder.position_at_end(exit_block);

        // let phi = self
        //     .llvmbuilder
        //     .build_phi(self.llvmctx.bool_type(), "enum_cmp_phi")
        //     .unwrap();

        // phi.add_incoming(&[(&self.llvmctx.bool_type().const_zero(), entry_block)]);
        // phi.add_incoming(&[(&payload_eq, payload_cmp_block)]);

        // InternalValue::new(
        //     SemanticType::BasicType(BasicType::Bool),
        //     InternalValueKind::RValue(phi.as_basic_value()),
        // )
    }
}
