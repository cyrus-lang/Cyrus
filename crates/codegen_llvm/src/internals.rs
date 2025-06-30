use std::process::exit;

use ast::token::Location;
use inkwell::values::IntValue;

use crate::{
    CodeGenLLVM,
    diag::{Diag, DiagKind, DiagLevel, DiagLoc, display_single_diag},
    values::InternalValue,
};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_internal_methods(
        &self,
        method_name: String,
        internal_value: InternalValue<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        match internal_value {
            InternalValue::BoolValue(int_value, _) => match &*method_name {
                "to_string" => self.internal_bool_to_string(int_value, loc, span_end),
                _ => {
                    self.undefined_internal_method_error(method_name, "bool", loc, span_end);
                    exit(1);
                }
            },
            InternalValue::IntValue(int_value, internal_type) => todo!(),
            InternalValue::FloatValue(float_value, internal_type) => todo!(),
            InternalValue::ArrayValue(array_value, internal_type) => todo!(),
            InternalValue::VectorValue(vector_value, internal_type) => todo!(),
            InternalValue::StringValue(string_value) => todo!(),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "No any internal method found for value of type '{}'.",
                        internal_value.get_type(self.string_type.clone())
                    )),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        }
    }

    fn internal_bool_to_string(
        &self,
        int_value: IntValue<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        if int_value.is_const() {
            let bool_value = int_value.get_zero_extended_constant().unwrap();
            let bool_value_str = if bool_value == 1 { "true" } else { "false" };
            self.build_string_literal(bool_value_str.to_string(), loc, span_end)
        } else {
            unimplemented!();
        }
    }

    fn undefined_internal_method_error(
        &self,
        method_name: String,
        value_type: &'ctx str,
        loc: Location,
        span_end: usize,
    ) {
        display_single_diag(Diag {
            level: DiagLevel::Error,
            kind: DiagKind::Custom(format!(
                "Method '{}' not defined for value of type '{}'.",
                method_name, value_type
            )),
            location: Some(DiagLoc {
                file: self.file_path.clone(),
                line: loc.line,
                column: loc.column,
                length: span_end,
            }),
        });
        exit(1);
    }
}
