use crate::CodeGenLLVM;
use ast::ast::*;
use inkwell::{values::{AnyValueEnum, ArrayValue, FloatValue, IntValue, PointerValue}, AddressSpace};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_expr(&mut self, expr: Expression) -> AnyValueEnum {
        match expr {
            Expression::Identifier(identifier) => todo!(),
            Expression::Assignment(assignment) => todo!(),
            Expression::Literal(literal) => self.build_literal(literal),
            Expression::Prefix(unary_expression) => todo!(),
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

    pub(crate) fn build_literal(&mut self, literal: Literal) -> AnyValueEnum {
        match literal {
            Literal::Integer(integer_literal) => AnyValueEnum::IntValue(self.build_integer_literal(integer_literal)),
            Literal::Float(float_literal) => AnyValueEnum::FloatValue(self.build_float_literal(float_literal)),
            Literal::Bool(bool_literal) => AnyValueEnum::IntValue(self.build_bool_literal(bool_literal)),
            Literal::String(string_literal) => {
                AnyValueEnum::ArrayValue(self.build_string_literal(string_literal))
            },
            Literal::Char(char_literal) => {
                AnyValueEnum::IntValue(self.build_char_literal(char_literal))
            },
            Literal::Null => {
                AnyValueEnum::PointerValue(self.build_null_literal())
            },
        }
    }

    pub(crate) fn build_integer_literal(&mut self, integer_literal: IntegerLiteral) -> IntValue {
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

    pub(crate) fn build_float_literal(&mut self, float_literal: FloatLiteral) -> FloatValue {
        match float_literal {
            FloatLiteral::Float(val) => self.context.f32_type().const_float(val.into()),
            FloatLiteral::Double(val) => self.context.f64_type().const_float(val),
        }
    }

    pub(crate) fn build_string_literal(&mut self, string_literal: StringLiteral) -> ArrayValue {
        self.context.const_string(string_literal.raw.as_bytes(), true)
    }
    
    pub(crate) fn build_char_literal(&mut self, char_literal: CharLiteral) -> IntValue {
        self.context.i8_type().const_int(char_literal.raw as u8 as u64, false)
    }

    pub(crate) fn build_null_literal(&mut self) -> PointerValue {
        self.context.ptr_type(AddressSpace::default()).const_null()
    }

    pub(crate) fn build_bool_literal(&mut self, bool_literal: BoolLiteral) -> IntValue {
        self.context.bool_type().const_int(if bool_literal.raw { 1 } else { 0 }, false)
    }
}
