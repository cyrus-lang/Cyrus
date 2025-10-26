use ast::{AccessSpecifier, source_loc::SourceLoc};
use std::collections::HashMap;
use tast::{
    ModuleID, SymbolID,
    exprs::{TypedExprStmt, TypedIdentifier},
    stmts::{
        TypedEnumVariant, TypedFuncDeclStmt, TypedFuncParamKind, TypedFuncParams, TypedGenericParamsList,
        TypedStructField, TypedUnionField,
    },
    types::SemanticType,
};

#[derive(Debug, Clone)]
pub struct StructSig {
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub impls: Vec<TypedIdentifier>,
    pub methods: HashMap<String, SymbolID>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub packed: bool,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct UnionSig {
    pub symbol_id: SymbolID,
    pub name: String,
    pub fields: Vec<TypedUnionField>,
    pub methods: HashMap<String, SymbolID>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub module_id: ModuleID,
    pub name: String,
    pub params: TypedFuncParams,
    pub return_type: SemanticType,
    pub is_func_decl: bool,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct EnumSig {
    pub symbol_id: SymbolID,
    pub name: String,
    pub methods: HashMap<String, SymbolID>,
    pub variants: Vec<TypedEnumVariant>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedefSig {
    pub name: String,
    pub ty: SemanticType,
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct InterfaceSig {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub methods: Vec<TypedFuncDeclStmt>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct GlobalVarSig {
    pub module_id: ModuleID,
    pub name: String,
    pub ty: Option<SemanticType>,
    pub rhs: Option<TypedExprStmt>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

impl PartialEq for FuncSig {
    fn eq(&self, other: &Self) -> bool {
        let self_params: Vec<_> = self
            .params
            .list
            .iter()
            .filter(|p| matches!(p, TypedFuncParamKind::FuncParam(_)))
            .collect();

        let other_params: Vec<_> = other
            .params
            .list
            .iter()
            .filter(|p| matches!(p, TypedFuncParamKind::FuncParam(_)))
            .collect();

        self.name == other.name && self_params == other_params && self.return_type == other.return_type
    }
}
