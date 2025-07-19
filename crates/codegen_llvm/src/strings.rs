use crate::{
    CodeGenLLVM, InternalType, InternalValue,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    types::{InternalArrayType, InternalConstType, InternalIntType, InternalVoidType},
    values::TypedPointerValue,
};
use ast::token::{Location, TokenKind};
use inkwell::{
    AddressSpace,
    context::Context,
    module::Linkage,
    types::ArrayType,
    values::{GlobalValue, IntValue, PointerValue},
};
use std::process::exit;
use utils::escaping::unescape_string;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_byte_string(&self, value: String, loc: Location, span_end: usize) -> InternalValue<'ctx> {
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
        InternalValue::ArrayValue(
            const_str,
            InternalType::ArrayType(InternalArrayType {
                type_str: format!("char[{}]", unescaped_string.len()),
                inner_type: Box::new(InternalType::IntType(InternalIntType {
                    type_str: "char".to_string(),
                    int_kind: TokenKind::Char,
                    int_type: self.context.i8_type(),
                })),
                array_type: const_str.get_type(),
            }),
        )
    }

    pub(crate) fn build_c_style_string(&self, value: String, loc: Location, span_end: usize) -> InternalValue<'ctx> {
        self.build_string_literal(value, loc, span_end)
    }

    pub(crate) fn build_global_str(
        &self,
        value: String,
        loc: Location,
        span_end: usize,
    ) -> (GlobalValue<'ctx>, ArrayType<'ctx>) {
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
        let module_ref = self.module.borrow_mut(); // Borrow mutably
        let global_str = module_ref.add_global(const_str.get_type(), None, ".str");
        global_str.set_initializer(&const_str);
        global_str.set_constant(true);
        global_str.set_unnamed_addr(true);
        global_str.set_linkage(Linkage::Private);
        global_str.set_alignment(1);
        (global_str, const_str.get_type())
    }

    pub(crate) fn build_string_literal(&self, value: String, loc: Location, span_end: usize) -> InternalValue<'ctx> {
        let (global_str, array_type) = self.build_global_str(value, loc, span_end);
        InternalValue::PointerValue(TypedPointerValue {
            type_str: format!("char*"),
            ptr: global_str.as_pointer_value(),
            pointee_ty: InternalType::IntType(InternalIntType {
                type_str: "char".to_string(),
                int_kind: TokenKind::Char,
                int_type: self.context.i8_type(),
            }),
        })
    }

    // FIXME Consider to move the String type into std::core::intrinsics instead of building it in the compiler itself.

    // pub(crate) fn build_create_string_value(
    //     &self,
    //     buffer: PointerValue<'ctx>,
    //     buffer_size: IntValue<'ctx>,
    // ) -> InternalValue<'ctx> {
    //     let undef = self.string_type.struct_type.get_undef();

    //     let str_val = self
    //         .builder
    //         .build_insert_value(undef, buffer, 0, "string.buffer")
    //         .unwrap();
    //     let str_val2 = self
    //         .builder
    //         .build_insert_value(str_val, buffer_size, 1, "string.length")
    //         .unwrap()
    //         .into_struct_value();

    //     InternalValue::StringValue(StringValue { struct_value: str_val2 })
    // }

    // pub(crate) fn build_string_type(context: &'ctx Context) -> InternalStringType<'ctx> {
    //     let i8_ptr_type = context.ptr_type(AddressSpace::default());
    //     let i64_type = context.i64_type();

    //     let struct_type = context.opaque_struct_type("string");
    //     struct_type.set_body(&[i8_ptr_type.into(), i64_type.into()], false);

    //     InternalStringType {
    //         struct_type,
    //         type_str: "string".to_string(),
    //     }
    // }

    // pub(crate) fn build_load_string(&self, string_value: StringValue<'ctx>) -> InternalValue<'ctx> {
    //     let data_str = self
    //         .builder
    //         .build_extract_value(string_value.struct_value, 0, "exv")
    //         .unwrap();

    //     InternalValue::PointerValue(TypedPointerValue {
    //         type_str: "char*".to_string(),
    //         ptr: data_str.into_pointer_value(),
    //         pointee_ty: InternalType::VoidType(InternalVoidType {
    //             type_str: "char".to_string(),
    //             void_type: self.context.void_type(),
    //         }),
    //     })
    // }

    // pub(crate) fn build_empty_string(&self, loc: Location, span_end: usize) -> InternalValue<'ctx> {
    //     self.build_string_literal("".to_string(), loc, span_end)
    // }
}
