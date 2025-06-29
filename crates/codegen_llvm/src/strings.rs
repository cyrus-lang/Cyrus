use crate::{
    CodeGenLLVM, InternalType, InternalValue, StringValue,
    types::{InternalArrayType, InternalIntType, InternalStringType, InternalVoidType},
    values::TypedPointerValue,
};
use inkwell::{
    AddressSpace,
    context::Context,
    values::{AnyValue, IntValue, PointerValue},
};
use std::ops::DerefMut;
use utils::purify_string::unescape_string;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_string_literal(&self, value: String) -> InternalValue<'ctx> {
        let mut bytes = unescape_string(value).into_bytes();
        bytes.push(0); // null terminator

        let i8_array_type = self.context.i8_type().array_type(bytes.len() as u32);

        let string_global =
            self.module
                .borrow_mut()
                .deref_mut()
                .add_global(i8_array_type, Some(AddressSpace::default()), ".str");

        let const_string = self.context.const_string(&bytes, false);
        string_global.set_initializer(&const_string);
        string_global.set_constant(true);
        string_global.set_linkage(inkwell::module::Linkage::Private);

        InternalValue::StrValue(
            string_global.as_any_value_enum().into_pointer_value(),
            InternalType::ArrayType(InternalArrayType {
                type_str: "string".to_string(),
                inner_type: Box::new(InternalType::IntType(InternalIntType {
                    type_str: "char".to_string(),
                    int_type: self.context.i8_type(),
                })),
                array_type: i8_array_type,
            }),
        )
    }

    pub(crate) fn build_string_type(context: &'ctx Context) -> InternalStringType<'ctx> {
        let i8_ptr_type = context.ptr_type(AddressSpace::default());
        let i64_type = context.i64_type();

        let struct_type = context.opaque_struct_type("str");
        struct_type.set_body(&[i8_ptr_type.into(), i64_type.into()], false);

        InternalStringType {
            struct_type,
            type_str: "string".to_string(),
        }
    }

    pub(crate) fn build_load_string(&self, string_value: StringValue<'ctx>) -> InternalValue<'ctx> {
        let data_str = self
            .builder
            .build_extract_value(string_value.struct_value, 0, "exv")
            .unwrap();

        InternalValue::PointerValue(TypedPointerValue {
            type_str: "char*".to_string(),
            ptr: data_str.into_pointer_value(),
            pointee_ty: InternalType::VoidType(InternalVoidType {
                type_str: "char".to_string(),
                void_type: self.context.void_type(),
            }),
        })
    }

    pub(crate) fn build_construct_string_value(
        &self,
        buffer: PointerValue<'ctx>,
        buffer_size: IntValue<'ctx>,
    ) -> InternalValue<'ctx> {
        let undef = self.string_type.struct_type.get_undef();

        let str_val = self
            .builder
            .build_insert_value(undef, buffer, 0, "string.buffer")
            .unwrap();
        let str_val2 = self
            .builder
            .build_insert_value(str_val, buffer_size, 1, "string.length")
            .unwrap()
            .into_struct_value();

        InternalValue::StringValue(StringValue { struct_value: str_val2 })
    }

    pub(crate) fn build_zeroinit_string(&self) -> InternalValue<'ctx> {
        let const_str = self.context.const_string(b"", true);
        let global_str = self
            .module
            .borrow_mut()
            .add_global(const_str.get_type(), None, ".string.empty");
        global_str.set_initializer(&const_str);
        global_str.set_constant(true);

        let buffer_size = self
            .context
            .i64_type()
            .const_int(const_str.get_type().len().into(), false);
        self.build_construct_string_value(global_str.as_pointer_value(), buffer_size)
    }
}
