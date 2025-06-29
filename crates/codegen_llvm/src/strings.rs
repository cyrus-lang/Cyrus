use std::process::exit;

use crate::{
    CodeGenLLVM, InternalType, InternalValue, StringValue,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    types::{InternalStringType, InternalVoidType},
    values::TypedPointerValue,
};
use ast::token::Location;
use inkwell::{
    AddressSpace,
    context::Context,
    values::{IntValue, PointerValue},
};
use utils::escaping::unescape_string;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_string_literal(&self, value: String, loc: Location, span_end: usize) -> InternalValue<'ctx> {
        let make_unescaped_string =
            || -> Result<String, utils::escaping::UnescapeError> { unescape_string(&unescape_string(&value)?) };

        let unescaped_string = match make_unescaped_string() {
            Ok(v) => v,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(err.to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        };

        let const_str = self.context.const_string(unescaped_string.as_bytes(), true);
        let global_str = self.module.borrow_mut().add_global(const_str.get_type(), None, ".str");
        global_str.set_initializer(&const_str);
        global_str.set_constant(true);

        let buffer_size = self
            .context
            .i64_type()
            .const_int(const_str.get_type().len().into(), false);

        self.build_create_string_value(global_str.as_pointer_value(), buffer_size)
    }

    pub(crate) fn build_create_string_value(
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

    pub(crate) fn build_string_type(context: &'ctx Context) -> InternalStringType<'ctx> {
        let i8_ptr_type = context.ptr_type(AddressSpace::default());
        let i64_type = context.i64_type();

        let struct_type = context.opaque_struct_type("string");
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

    pub(crate) fn build_empty_string(&self, loc: Location, span_end: usize) -> InternalValue<'ctx> {
        self.build_string_literal("".to_string(), loc, span_end)
    }
}
