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
