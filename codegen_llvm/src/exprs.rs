use std::process::exit;

use crate::{
    CodeGenLLVM,
    diag::{Diag, DiagKind, DiagLevel, display_single_diag},
};
use ast::{ast::*, token::TokenKind};
use inkwell::{
    AddressSpace,
    types::AnyTypeEnum,
    values::{AnyValueEnum, ArrayValue, FloatValue, IntValue, PointerValue},
};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_expr(&self, expr: Expression) -> AnyValueEnum {
        match expr {
            Expression::Identifier(identifier) => todo!(),
            Expression::Assignment(assignment) => {
                self.build_assignment(assignment);
                inkwell::values::AnyValueEnum::PointerValue(self.build_null_literal())
            }
            Expression::Literal(literal) => self.build_literal(literal),
            Expression::Prefix(unary_expression) => self.build_prefix_expr(unary_expression),
            Expression::Infix(binary_expression) => todo!(),
            Expression::UnaryOperator(unary_operator) => todo!(),
            Expression::AddressOf(expression) => todo!(),
            Expression::Dereference(expression) => todo!(),
            Expression::CastAs(cast_as) => todo!(),
            Expression::StructInit(struct_init) => todo!(),
            Expression::StructFieldAccess(struct_field_access) => todo!(),
            Expression::FieldAccessOrMethodCall(field_access_or_method_calls) => todo!(),
            Expression::Array(array) => todo!(),
            Expression::ArrayIndex(array_index) => todo!(),
            Expression::ArrayIndexAssign(array_index_assign) => todo!(),
            Expression::FromPackage(_) => todo!(),
        }
    }

    pub(crate) fn build_literal(&self, literal: Literal) -> AnyValueEnum {
        match literal {
            Literal::Integer(integer_literal) => AnyValueEnum::IntValue(self.build_integer_literal(integer_literal)),
            Literal::Float(float_literal) => AnyValueEnum::FloatValue(self.build_float_literal(float_literal)),
            Literal::Bool(bool_literal) => AnyValueEnum::IntValue(self.build_bool_literal(bool_literal)),
            Literal::String(string_literal) => AnyValueEnum::ArrayValue(self.build_string_literal(string_literal)),
            Literal::Char(char_literal) => AnyValueEnum::IntValue(self.build_char_literal(char_literal)),
            Literal::Null => AnyValueEnum::PointerValue(self.build_null_literal()),
        }
    }

    pub(crate) fn build_integer_literal(&self, integer_literal: IntegerLiteral) -> IntValue {
        match integer_literal {
            IntegerLiteral::I8(val) => self.context.i8_type().const_int(val.try_into().unwrap(), true),
            IntegerLiteral::I16(val) => self.context.i16_type().const_int(val.try_into().unwrap(), true),
            IntegerLiteral::I32(val) => self.context.i32_type().const_int(val.try_into().unwrap(), true),
            IntegerLiteral::I64(val) => self.context.i64_type().const_int(val.try_into().unwrap(), true),
            IntegerLiteral::I128(val) => self.context.i128_type().const_int(val.try_into().unwrap(), true),
            IntegerLiteral::U8(val) => self.context.i8_type().const_int(val.try_into().unwrap(), false),
            IntegerLiteral::U16(val) => self.context.i16_type().const_int(val.try_into().unwrap(), false),
            IntegerLiteral::U32(val) => self.context.i32_type().const_int(val.try_into().unwrap(), false),
            IntegerLiteral::U64(val) => self.context.i64_type().const_int(val.try_into().unwrap(), false),
            IntegerLiteral::U128(val) => self.context.i128_type().const_int(val.try_into().unwrap(), false),
            IntegerLiteral::SizeT(val) => todo!(),
        }
    }

    pub(crate) fn build_float_literal(&self, float_literal: FloatLiteral) -> FloatValue {
        match float_literal {
            FloatLiteral::Float(val) => self.context.f32_type().const_float(val.into()),
            FloatLiteral::Double(val) => self.context.f64_type().const_float(val),
        }
    }

    pub(crate) fn build_string_literal(&self, string_literal: StringLiteral) -> ArrayValue {
        self.context.const_string(string_literal.raw.as_bytes(), true)
    }

    pub(crate) fn build_char_literal(&self, char_literal: CharLiteral) -> IntValue {
        self.context.i8_type().const_int(char_literal.raw as u8 as u64, false)
    }

    pub(crate) fn build_null_literal(&self) -> PointerValue {
        self.context.ptr_type(AddressSpace::default()).const_null()
    }

    pub(crate) fn build_bool_literal(&self, bool_literal: BoolLiteral) -> IntValue {
        self.context
            .bool_type()
            .const_int(if bool_literal.raw { 1 } else { 0 }, false)
    }

    pub(crate) fn build_assignment(&self, assignment: Box<Assignment>) {
        // assignment.
        // self.builder.build_store(ptr, value)
    }

    pub(crate) fn build_prefix_expr(&self, unary_expression: UnaryExpression) -> AnyValueEnum {
        let operand = self.build_expr(*unary_expression.operand.clone());
        match unary_expression.operator.kind {
            TokenKind::Minus => match operand {
                AnyValueEnum::IntValue(int_value) => {
                    AnyValueEnum::IntValue(self.builder.build_int_neg(int_value, "prefix_iminus").unwrap())
                }
                AnyValueEnum::FloatValue(float_value) => {
                    AnyValueEnum::FloatValue(self.builder.build_float_neg(float_value, "prefix_fminus").unwrap())
                }
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("Cannot build minus for non-basic value.")),
                        location: None,
                    });
                    exit(1);
                }
            },
            TokenKind::Bang => match operand {
                AnyValueEnum::IntValue(int_value) => {
                    let zero = int_value.get_type().const_int(0, false);
                    let one = int_value.get_type().const_int(1, false);

                    let is_zero = self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, int_value, zero, "prefix_ineg")
                        .unwrap();
                    
                    if is_zero.get_zero_extended_constant().unwrap() == 0 {
                        AnyValueEnum::IntValue(zero)
                    } else {
                        AnyValueEnum::IntValue(one)
                    }
                }
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(String::from("Cannot build neg for non-integer value.")),
                        location: None,
                    });
                    exit(1);
                }
            },
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(String::from("Invalid prefix token.")),
                    location: None,
                });
                exit(1);
            }
        }
    }
}
