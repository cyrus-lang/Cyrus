use crate::{
    context::CodeGenLLVM,
    diag::{display_single_diag, Diag, DiagKind, DiagLevel, DiagLoc},
    types::{InternalArrayType, InternalIntType, InternalType},
    values::{InternalValue, TypedPointerValue},
};
use ast::token::{Location, TokenKind};
use inkwell::{module::Linkage, types::ArrayType, values::GlobalValue};
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
        let (global_str, _) = self.build_global_str(value, loc, span_end);
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
}
