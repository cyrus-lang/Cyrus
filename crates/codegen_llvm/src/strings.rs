use inkwell::{AddressSpace, values::PointerValue};
use crate::{AnyValue, CodeGenLLVM, StringValue};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn string_from_struct_value(&self, struct_ptr: PointerValue<'ctx>) -> StringValue<'ctx> {
        let struct_type = self.string_type.struct_type;
        let mut struct_value = struct_type.get_undef();

        let data_pointee_ty = self.context.ptr_type(AddressSpace::default());
        let data_ptr = self
            .builder
            .build_struct_gep(struct_type, struct_ptr, 0, "data_gep")
            .expect("Failed to get GEP for data_ptr");
        let loaded_data_ptr = self
            .builder
            .build_load(data_pointee_ty, data_ptr, "load_data_ptr")
            .unwrap();

        let len_pointee_ty = self.context.i64_type();
        let len_ptr = self
            .builder
            .build_struct_gep(struct_type, struct_ptr, 1, "len_gep")
            .expect("Failed to get GEP for len");
        let loaded_len = self.builder.build_load(len_pointee_ty, len_ptr, "load_len").unwrap();

        struct_value = self
            .builder
            .build_insert_value(struct_value, loaded_data_ptr, 0, "insert_data")
            .unwrap()
            .into_struct_value();

        struct_value = self
            .builder
            .build_insert_value(struct_value, loaded_len, 1, "insert_len")
            .unwrap()
            .into_struct_value();

        StringValue { struct_value } 
    }

    pub(crate) fn build_load_string(&self, string_value: StringValue<'ctx>) -> AnyValue<'ctx> {
        let data_str = self
            .builder
            .build_extract_value(string_value.struct_value, 0, "load_string")
            .unwrap();

        AnyValue::OpaquePointer(data_str.into_pointer_value())
    }
}
