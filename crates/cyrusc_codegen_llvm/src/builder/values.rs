use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};

#[derive(Debug, Clone)]
pub struct InternalValue<'a> {
    pub kind: InternalValueKind<'a>,
}

#[derive(Debug, Clone)]
pub enum InternalValueKind<'a> {
    LValue(PointerValue<'a>),
    RValue(BasicValueEnum<'a>),
    FuncValue(FunctionValue<'a>),
}
