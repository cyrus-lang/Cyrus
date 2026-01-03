// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

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
use crate::builder::builder::IRBuilderCtx;
use inkwell::values::{BasicValueEnum, PointerValue};

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_memcpy(&self, dest: PointerValue<'ll>, rvalue: BasicValueEnum<'ll>) {
        let target_data = self.llvmtm.get_target_data();
        let temp = self.llvmbuilder.build_alloca(rvalue.get_type(), "memcpy.temp").unwrap();

        self.llvmbuilder.build_store(temp, rvalue).unwrap();

        let size_in_bytes = target_data.get_store_size(&rvalue.get_type());
        let size_val = self.llvmctx.i64_type().const_int(size_in_bytes, false);

        let src_align = target_data.get_abi_alignment(&rvalue.get_type());
        let dest_align = target_data.get_abi_alignment(&rvalue.get_type());

        self.llvmbuilder
            .build_memcpy(dest, dest_align, temp, src_align, size_val)
            .expect("failed to emit memcpy");
    }
}
