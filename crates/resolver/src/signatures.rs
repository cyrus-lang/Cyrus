use ast::{AccessSpecifier, source_loc::SourceLoc};
use std::collections::HashMap;
use typed_ast::{
    ModuleID, SymbolID, TypedEnumVariant, TypedExpression, TypedFuncDecl, TypedFuncParamKind, TypedFuncParams, TypedGenericParamsList, TypedIdentifier, TypedStructField, TypedUnionField, types::ConcreteType
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
    pub symbol_id: SymbolID,
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
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
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
