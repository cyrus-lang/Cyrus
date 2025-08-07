use crate::scope::LocalOrGlobalSymbol;
use ast::{AccessSpecifier, token::Location};
use std::collections::HashMap;
use typed_ast::{SymbolID, TypedEnumVariant, TypedFuncDecl, TypedFuncParams, TypedStructField, types::ConcreteType};

#[derive(Debug, Clone)]
pub struct StructSig {
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub impls: Vec<LocalOrGlobalSymbol>,
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
pub struct InterfaceSig {
    pub name: String,
    pub methods: Vec<TypedFuncDecl>,
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
