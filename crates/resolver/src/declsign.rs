use ast::{AccessSpecifier, token::Location};
use typed_ast::{TypedEnumVariant, TypedFuncDef, TypedFuncParams, TypedStructField, types::ConcreteType};

#[derive(Debug, Clone)]
pub struct StructSig {
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub methods: Vec<TypedFuncDef>,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub name: String,
    pub params: TypedFuncParams,
    pub return_type: ConcreteType,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct EnumSig {
    pub name: String,
    pub variants: Vec<TypedEnumVariant>,
    pub vis: AccessSpecifier,
    pub loc: Location,
}
