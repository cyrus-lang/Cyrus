// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                                                         │
// │  Cyrus Programming Language                                             │
// │  https://github.com/cyrus-lang/Cyrus                                    │
// │                                                                         │
// │  A general-purpose, statically-typed, manually memory-managed           │
// │  programming language designed for performance-critical applications.   │
// │                                                                         │
// │  Copyright (c) 2026 The Cyrus Programming Language Project              │
// │                                                                         │
// │  This program is free software: you can redistribute it and/or modify   │
// │  it under the terms of the GNU General Public License as published by   │
// │  the Free Software Foundation, either version 3 of the License, or      │
// │  (at your option) any later version.                                    │
// │                                                                         │
// │  This program is distributed in the hope that it will be useful,        │
// │  but WITHOUT ANY WARRANTY; without even the implied warranty of         │
// │  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the           │
// │  GNU General Public License for more details.                           │
// │                                                                         │
// │  You should have received a copy of the GNU General Public License      │
// │  along with this program. If not, see <https://www.gnu.org/licenses/>.  │
// │                                                                         │
// └─────────────────────────────────────────────────────────────────────────┘

/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
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
use crate::builder::{
    builder::IRBuilderCtx,
    values::{InternalValue, InternalValueKind},
};
use cyrusc_cir::types::CIRTy;
use cyrusc_tast::types::PlainType;
use inkwell::values::{ArrayValue, BasicValue, IntValue, StructValue};

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn extract_enum_idx(&self, struct_value: StructValue<'ll>) -> IntValue<'ll> {
        self.llvmbuilder
            .build_extract_value(struct_value, 0, "extract")
            .unwrap()
            .into_int_value()
    }

    pub(crate) fn extract_enum_payload(&self, struct_value: StructValue<'ll>) -> ArrayValue<'ll> {
        self.llvmbuilder
            .build_extract_value(struct_value, 1, "extract")
            .unwrap()
            .into_array_value()
    }

    pub(crate) fn emit_compare_enum_variants(
        &mut self,
        lhs: InternalValue<'ll>,
        rhs: InternalValue<'ll>,
        cmp_eq: bool,
    ) -> InternalValue<'ll> {
        let struct_value1 = lhs.as_basic_value().into_struct_value();
        let struct_value2 = rhs.as_basic_value().into_struct_value();

        let tag1 = self.extract_enum_idx(struct_value1);
        let tag2 = self.extract_enum_idx(struct_value2);

        let tag_concrete_type = CIRTy::PlainType(PlainType::UInt32);
        let tag_cmp_result = if cmp_eq {
            self.emit_cmp_eq(
                InternalValue::new(
                    tag_concrete_type.clone(),
                    InternalValueKind::RValue(tag1.as_basic_value_enum()),
                ),
                InternalValue::new(tag_concrete_type, InternalValueKind::RValue(tag2.as_basic_value_enum())),
            )
        } else {
            self.emit_cmp_neq(
                InternalValue::new(
                    tag_concrete_type.clone(),
                    InternalValueKind::RValue(tag1.as_basic_value_enum()),
                ),
                InternalValue::new(tag_concrete_type, InternalValueKind::RValue(tag2.as_basic_value_enum())),
            )
        };

        let current_func = self.cur_fn.unwrap();
        let payload_block = self.llvmctx.append_basic_block(current_func, "enum_cmp.payload");
        let exit_block = self.llvmctx.append_basic_block(current_func, "enum_cmp.exit");

        let entry_block = self.blockreg.cur_block.unwrap();

        self.llvmbuilder
            .build_conditional_branch(
                tag_cmp_result.as_basic_value().into_int_value(),
                payload_block,
                exit_block,
            )
            .unwrap();

        self.emit_block(payload_block);

        let payload1 = self.extract_enum_payload(struct_value1);
        let payload2 = self.extract_enum_payload(struct_value2);

        let memcmp_result = self.intrinsic_array_memcmp(payload1, payload2);

        let i32_zero = self.llvmctx.i32_type().const_zero();
        let payload_eq = self
            .llvmbuilder
            .build_int_compare(inkwell::IntPredicate::EQ, memcmp_result, i32_zero, "payload_eq")
            .unwrap();

        self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();

        let payload_cmp_block = self.blockreg.cur_block.unwrap();

        self.emit_block(exit_block);

        let phi = self
            .llvmbuilder
            .build_phi(self.llvmctx.bool_type(), "enum_cmp_phi")
            .unwrap();

        phi.add_incoming(&[(&self.llvmctx.bool_type().const_zero(), entry_block)]);
        phi.add_incoming(&[(&payload_eq, payload_cmp_block)]);

        InternalValue::new(
            CIRTy::PlainType(PlainType::Bool),
            InternalValueKind::RValue(phi.as_basic_value()),
        )
    }
}
