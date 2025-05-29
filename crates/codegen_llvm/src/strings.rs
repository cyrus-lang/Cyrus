use inkwell::values::BasicValueEnum;

use crate::{CodeGenLLVM, InternalType, InternalValue, StringValue, values::TypedPointerValue};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_string(&self, buffer_ptr: BasicValueEnum<'ctx>, len: u64) -> InternalValue<'ctx> {
        InternalValue::StringValue(StringValue {
            struct_value: self.string_type.struct_type.const_named_struct(&[
                buffer_ptr,
                BasicValueEnum::IntValue(self.context.i64_type().const_int(len, false)),
            ]),
        })
    }

    pub(crate) fn build_load_string(&self, string_value: StringValue<'ctx>) -> InternalValue<'ctx> {
        let data_str = self
            .builder
            .build_extract_value(string_value.struct_value, 0, "exv")
            .unwrap();

        InternalValue::PointerValue(TypedPointerValue {
            ptr: data_str.into_pointer_value(),
            pointee_ty: InternalType::VoidType(self.context.void_type()),
        })
    }
}
