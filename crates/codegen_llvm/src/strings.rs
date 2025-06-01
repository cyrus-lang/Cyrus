use inkwell::{AddressSpace, context::Context};

use crate::{CodeGenLLVM, InternalType, InternalValue, StringType, StringValue, values::TypedPointerValue};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_string_type(context: &'ctx Context) -> StringType<'ctx> {
        let i8_ptr_type = context.ptr_type(AddressSpace::default());
        let i64_type = context.i64_type();

        let struct_type = context.opaque_struct_type("str");
        struct_type.set_body(&[i8_ptr_type.into(), i64_type.into()], false);

        StringType { struct_type }
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
