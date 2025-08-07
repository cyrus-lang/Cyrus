use std::collections::HashMap;

use ast::{AccessSpecifier, token::Location};
use typed_ast::{SymbolID, TypedEnumVariant, TypedFuncParams, TypedStructField, types::ConcreteType};

#[derive(Debug, Clone)]
pub struct StructSig {
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub methods: HashMap<String, SymbolID>,
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

#[derive(Debug, Clone)]
pub struct TypedefSig {
    pub name: String,
    pub ty: ConcreteType,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct GlobalVarSig {
    pub name: String,
    pub ty: Option<ConcreteType>,
    pub vis: AccessSpecifier,
    pub loc: Location,
}
