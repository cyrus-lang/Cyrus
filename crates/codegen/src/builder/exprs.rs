use crate::builder::module::CodeGenBuilder;
use ast::{Literal, LiteralKind, token::TokenKind};
use inkwell::{AddressSpace, types::BasicTypeEnum, values::AnyValueEnum};
use resolver::scope::LocalScopeRef;
use typed_ast::{TypedExpression, TypedExpressionKind, TypedLiteral, types::ConcreteType};

#[derive(Debug, Clone)]
pub struct CodeGenExprValue<'a> {
    concrete_type: ConcreteType,
    value: AnyValueEnum<'a>,
}

impl<'a> CodeGenExprValue<'a> {
    pub fn lvalue(&self) -> AnyValueEnum<'a> {
        self.value
    }

    pub fn rvalue(&self) -> AnyValueEnum<'a> {
        match &self.concrete_type {
            ConcreteType::Symbol(_) => todo!(),
            ConcreteType::Pointer(concrete_type) => {
                todo!()
            }
            ConcreteType::Const(_) => self.value,
            ConcreteType::Array(_) => self.value,
            ConcreteType::BasicType(_) => self.value,
            ConcreteType::UnnamedStruct(_) => self.value,
        }
    }
}

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_expr(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        typed_expr: &TypedExpression,
    ) -> AnyValueEnum<'a> {
        match &typed_expr.kind {
            TypedExpressionKind::Symbol(_) => todo!(),
            TypedExpressionKind::Literal(typed_literal) => self.build_literal(typed_literal),
            TypedExpressionKind::Prefix(typed_prefix_expression) => todo!(),
            TypedExpressionKind::Infix(typed_infix_expression) => todo!(),
            TypedExpressionKind::Unary(typed_unary_expression) => todo!(),
            TypedExpressionKind::Assignment(typed_assignment) => todo!(),
            TypedExpressionKind::Cast(typed_cast) => todo!(),
            TypedExpressionKind::Array(typed_array) => todo!(),
            TypedExpressionKind::ArrayIndex(typed_array_index) => todo!(),
            TypedExpressionKind::AddressOf(typed_address_of) => todo!(),
            TypedExpressionKind::Dereference(typed_dereference) => todo!(),
            TypedExpressionKind::StructInit(typed_struct_init) => todo!(),
            TypedExpressionKind::FuncCall(typed_func_call) => todo!(),
            TypedExpressionKind::FieldAccess(typed_field_access) => todo!(),
            TypedExpressionKind::MethodCall(typed_method_call) => todo!(),
            TypedExpressionKind::UnnamedStructValue(typed_unnamed_struct_value) => todo!(),
        }
    }

    fn build_literal(&self, literal: &TypedLiteral) -> AnyValueEnum<'a> {
        let basic_type_enum: BasicTypeEnum<'a> = self.build_basic_concrete_type(literal.ty.clone()).try_into().unwrap();

        match &literal.kind {
            LiteralKind::Bool(value) => {
                AnyValueEnum::IntValue(self.llvmctx.bool_type().const_int(*value as u64, false))
            }
            LiteralKind::Integer(value, _) => AnyValueEnum::IntValue(
                basic_type_enum
                    .into_int_type()
                    .const_int((*value).try_into().unwrap(), literal.ty.is_signed()),
            ),
            LiteralKind::Float(value, _) => {
                AnyValueEnum::FloatValue(basic_type_enum.into_float_type().const_float(*value))
            }
            LiteralKind::Char(value) => AnyValueEnum::IntValue(self.llvmctx.i8_type().const_int(*value as u64, false)),
            LiteralKind::Null => {
                AnyValueEnum::PointerValue(self.llvmctx.ptr_type(AddressSpace::default()).const_null())
            }
            LiteralKind::String(_, string_prefix) => todo!(),
        }
    }
}
