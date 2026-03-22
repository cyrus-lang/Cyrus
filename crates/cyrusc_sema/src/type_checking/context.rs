use cyrusc_typed_ast::{
    SymbolID,
    types::{SemanticType, TypedFuncType},
};

#[derive(Debug, Clone)]
pub(crate) struct TypeCheckContext {
    pub(crate) current_func: Option<TypedFuncType>,
    pub(crate) current_self: Option<SemanticType>,
    pub(crate) current_obj_operand_ty: Option<SemanticType>,
    pub(crate) current_method_symbol_id: Option<SymbolID>,
}

impl TypeCheckContext {
    pub fn new() -> Self {
        Self {
            current_func: None,
            current_self: None,
            current_obj_operand_ty: None,
            current_method_symbol_id: None,
        }
    }
}
