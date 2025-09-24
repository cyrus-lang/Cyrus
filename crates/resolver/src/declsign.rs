use crate::scope::LocalOrGlobalSymbol;
use ast::{source_loc::SourceLoc, AccessSpecifier};
use std::collections::HashMap;
use typed_ast::{
    ModuleID, SymbolID, TypedEnumVariant, TypedExpression, TypedFuncDecl, TypedFuncParams, TypedStructField,
    TypedUnionField, types::ConcreteType,
};

#[derive(Debug, Clone)]
pub struct StructSig {
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub impls: Vec<LocalOrGlobalSymbol>,
    pub methods: HashMap<String, SymbolID>,
    pub packed: bool,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct UnionSig {
    pub name: String,
    pub fields: Vec<TypedUnionField>,
    pub methods: HashMap<String, SymbolID>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub module_id: ModuleID,
    pub name: String,
    pub params: TypedFuncParams,
    pub return_type: ConcreteType,
    pub is_func_decl: bool,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct EnumSig {
    pub name: String,
    pub methods: HashMap<String, SymbolID>,
    pub variants: Vec<TypedEnumVariant>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedefSig {
    pub name: String,
    pub ty: ConcreteType,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct InterfaceSig {
    pub name: String,
    pub methods: Vec<TypedFuncDecl>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct GlobalVarSig {
    pub module_id: ModuleID,
    pub name: String,
    pub ty: Option<ConcreteType>,
    pub rhs: Option<TypedExpression>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}
