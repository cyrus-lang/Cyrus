use crate::builder::module::CodeGenBuilder;
use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, PointerValue},
};
use resolver::scope::LocalScopeRef;
use typed_ast::types::ConcreteType;

impl<'a> CodeGenBuilder<'a> {
    pub(crate) fn build_load_lvalue_to_rvalue(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        internal_value: InternalValue<'a>,
    ) -> InternalValue<'a> {
        let pointer = match &internal_value.kind {
            InternalValueKind::LValue(pointer_value) => pointer_value,
            InternalValueKind::RValue(..) => return internal_value,
        };

        let pointee_ty: BasicTypeEnum<'a> = self
            .build_concrete_type(local_scope_opt, internal_value.value_type.clone())
            .try_into()
            .unwrap();
        let basic_value = self.llvmbuilder.build_load(pointee_ty, *pointer, "load").unwrap();
        InternalValue::new(internal_value.value_type, InternalValueKind::RValue(basic_value))
    }
}

#[derive(Debug, Clone)]
pub struct InternalValue<'a> {
    pub value_type: ConcreteType,
    pub kind: InternalValueKind<'a>,
}

#[derive(Debug, Clone)]
pub enum InternalValueKind<'a> {
    LValue(PointerValue<'a>),
    RValue(BasicValueEnum<'a>),
}

impl<'a> InternalValue<'a> {
    pub fn new(value_type: ConcreteType, kind: InternalValueKind<'a>) -> Self {
        InternalValue { value_type, kind }
    }

    pub fn as_basic_value(&self) -> BasicValueEnum<'a> {
        match &self.kind {
            InternalValueKind::LValue(pointer) => BasicValueEnum::PointerValue(*pointer),
            InternalValueKind::RValue(value) => value.clone(),
        }
    }
}
