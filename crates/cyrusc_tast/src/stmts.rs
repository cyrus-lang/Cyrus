/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::{
    LabelID, ModuleID, ScopeID, SymbolID,
    exprs::{TypedExprStmt, TypedIdentifier, TypedLambdaExpr, TypedTupleAccessExpr, TypedTupleExpr},
    types::SemanticType,
};
use cyrusc_ast::{Ident, SelfModifierKind, abi::Visibility, modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers}};
use cyrusc_diagcentral::source_loc::SourceLoc;
use std::{collections::HashMap, hash::Hash};

#[derive(Debug, Clone)]
pub enum TypedStmt {
    Variable(TypedVarStmt),
    Typedef(TypedTypedefStmt),
    GlobalVar(TypedGlobalVarStmt),
    FuncDef(TypedFuncDefStmt),
    FuncDecl(TypedFuncDeclStmt),
    BlockStmt(TypedBlockStmt),
    If(TypedIfStmt),
    Return(TypedReturnStmt),
    Break(TypedBreakStmt),
    Continue(TypedContinueStmt),
    For(TypedForStmt),
    While(TypedWhileStmt),
    Switch(TypedSwitchStmt),
    Struct(TypedStructStmt),
    Enum(TypedEnumStmt),
    Union(TypedUnionStmt),
    Interface(TypedInterfaceStmt),
    Expr(TypedExprStmt),
    Defer(TypedDeferStmt),
    ExportTuple(TypedExportTupleStmt),
    Label(TypedLabelStmt),
    Goto(TypedGotoStmt),
}

#[derive(Debug, Clone)]
pub struct TypedLabelStmt {
    pub name: String,
    pub label_id: LabelID,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedGotoStmt {
    pub name: String,
    pub label_id: Option<LabelID>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedExportTupleStmt {
    pub pattern: TypedExportPattern,
    pub ty: Option<SemanticType>,
    pub rhs: Option<TypedExprStmt>,
    pub is_const: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub enum TypedExportPattern {
    Ident(SymbolID),
    Tuple(Vec<TypedExportPattern>),
}

#[derive(Debug, Clone)]
pub struct TypedDeferStmt {
    pub operand: Box<TypedStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedImplementInterface {
    pub symbol_id: SymbolID,
    pub type_args: Option<TypedTypeArgs>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedInterfaceStmt {
    pub name: String,
    pub symbol_id: SymbolID,
    pub methods: Vec<TypedFuncDeclStmt>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: Visibility,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedEnumStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub is_local: Option<ScopeID>,
    pub name: String,
    pub variants: Vec<TypedEnumVariant>,
    pub methods: HashMap<String, SymbolID>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub impls: Vec<TypedImplementInterface>,
    pub modifiers: EnumModifiers,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub enum TypedEnumVariant {
    Ident(Ident),
    Valued(Ident, Box<TypedExprStmt>),
    Variant(Ident, Vec<TypedEnumValuedField>),
}

#[derive(Debug, Clone)]
pub struct TypedEnumValuedField {
    pub ty: SemanticType,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedStructStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub is_local: Option<ScopeID>,
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub methods: HashMap<String, SymbolID>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub impls: Vec<TypedImplementInterface>,
    pub modifiers: StructModifiers,
    pub is_packed: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedUnionStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub is_local: Option<ScopeID>,
    pub name: String,
    pub fields: Vec<TypedUnionField>,
    pub methods: HashMap<String, SymbolID>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub impls: Vec<TypedImplementInterface>,
    pub modifiers: UnionModifiers,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedUnionField {
    pub name: String,
    pub ty: SemanticType,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedStructField {
    pub name: String,
    pub ty: SemanticType,
    pub vis: Visibility,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedReturnStmt {
    pub arg: Option<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedGlobalVarStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: Option<SemanticType>,
    pub expr: Option<TypedExprStmt>,
    pub is_const: bool,
    pub modifiers: GlobalVarModifiers,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedTypedefStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: SemanticType,
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: Visibility,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedBlockStmt {
    pub scope_id: ScopeID,
    pub stmts: Vec<TypedStmt>,
    pub defers: Vec<TypedDeferStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedVarStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: Option<SemanticType>,
    pub rhs: Option<TypedExprStmt>,
    pub is_const: bool,
    pub analyzed: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedIfStmt {
    pub cond: TypedExprStmt,
    pub then_block: Box<TypedBlockStmt>,
    pub branches: Vec<TypedIfStmt>,
    pub else_block: Option<Box<TypedBlockStmt>>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDefStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub params: TypedFuncParams,
    pub generic_params: Option<TypedGenericParamsList>,
    pub body: Box<TypedBlockStmt>,
    pub return_type: SemanticType,
    pub modifiers: FuncModifiers,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDeclStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub generic_params: Option<TypedGenericParamsList>,
    pub params: TypedFuncParams,
    pub return_type: SemanticType,
    pub modifiers: FuncModifiers,
    pub renamed_as: Option<String>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedFuncParams {
    pub list: Vec<TypedFuncParamKind>,
    pub variadic: Option<TypedFuncVariadicParams>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedFuncVariadicParams {
    UntypedCStyle,
    Typed(TypedIdentifier, SemanticType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedFuncTypeParams {
    pub list: Vec<SemanticType>,
    pub variadic: Option<Box<TypedFuncTypeVariadicParams>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypedFuncTypeVariadicParams {
    UntypedCStyle,
    Typed(SemanticType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedFuncParamKind {
    FuncParam(TypedFuncParam),
    SelfModifier(TypedSelfModifier),
}

#[derive(Debug, Clone, Eq)]
pub struct TypedSelfModifier {
    pub symbol_id: Option<SymbolID>,
    pub self_symbol_id: Option<SymbolID>,
    pub ty: Option<SemanticType>,
    pub kind: SelfModifierKind,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, Eq)]
pub struct TypedFuncParam {
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: SemanticType,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedForStmt {
    pub initializer: Option<TypedVarStmt>,
    pub cond: Option<TypedExprStmt>,
    pub increment: Option<TypedExprStmt>,
    pub body: Box<TypedBlockStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedWhileStmt {
    pub cond: TypedExprStmt,
    pub body: Box<TypedBlockStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedSwitchStmt {
    pub operand: TypedExprStmt,
    pub cases: Vec<TypedSwitchCase>,
    pub default_case: Option<TypedBlockStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedSwitchCase {
    pub patterns: Vec<TypedSwitchCasePattern>,
    pub body: Box<TypedBlockStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub enum TypedSwitchCasePattern {
    Range(TypedRange),
    Expr(TypedExprStmt, SourceLoc),
    Ident(String, SourceLoc),
    EnumVariant(String, Vec<TypedIdentifier>, SourceLoc),
}

#[derive(Debug, Clone)]
pub struct TypedRange {
    pub lower: TypedExprStmt,
    pub upper: TypedExprStmt,
    pub inclusive_upper: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedBreakStmt {
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedContinueStmt {
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypedGenericParamsList {
    pub list: Vec<TypedGenericParam>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypedGenericParam {
    pub param_name: TypedIdentifier,
    pub bounds: Option<Vec<TypedBound>>,
    pub default: Option<Box<SemanticType>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TypedBound {
    pub symbol: Ident,
    pub type_args: TypedTypeArgs,
}

#[derive(Debug, Clone, Eq)]
pub enum TypedTypeArg {
    Positional {
        idx: usize,
        ty: SemanticType,
        loc: SourceLoc,
    },
    Named {
        key: String,
        ty: SemanticType,
        loc: SourceLoc,
    },
}

impl TypedStmt {
    pub fn loc(&self) -> SourceLoc {
        match self {
            TypedStmt::Variable(typed_variable) => typed_variable.loc.clone(),
            TypedStmt::Typedef(typed_typedef) => typed_typedef.loc.clone(),
            TypedStmt::GlobalVar(typed_global_variable) => typed_global_variable.loc.clone(),
            TypedStmt::FuncDef(typed_func_def) => typed_func_def.loc.clone(),
            TypedStmt::FuncDecl(typed_func_decl) => typed_func_decl.loc.clone(),
            TypedStmt::BlockStmt(typed_block_statement) => typed_block_statement.loc.clone(),
            TypedStmt::If(typed_if) => typed_if.loc.clone(),
            TypedStmt::Return(typed_return) => typed_return.loc.clone(),
            TypedStmt::Break(typed_break) => typed_break.loc.clone(),
            TypedStmt::Continue(typed_continue) => typed_continue.loc.clone(),
            TypedStmt::For(typed_for) => typed_for.loc.clone(),
            TypedStmt::Switch(typed_switch) => typed_switch.loc.clone(),
            TypedStmt::Struct(typed_struct) => typed_struct.loc.clone(),
            TypedStmt::Enum(typed_enum) => typed_enum.loc.clone(),
            TypedStmt::Interface(typed_interface) => typed_interface.loc.clone(),
            TypedStmt::Expr(typed_expr) => typed_expr.loc.clone(),
            TypedStmt::While(while_stmt) => while_stmt.loc.clone(),
            TypedStmt::Union(union_stmt) => union_stmt.loc.clone(),
            TypedStmt::Defer(typed_defer) => typed_defer.loc.clone(),
            TypedStmt::ExportTuple(export_tuple_values) => export_tuple_values.loc.clone(),
            TypedStmt::Label(typed_label_stmt) => typed_label_stmt.loc.clone(),
            TypedStmt::Goto(typed_goto_stmt) => typed_goto_stmt.loc.clone(),
        }
    }
}

impl TypedExportPattern {
    pub fn into_tuple(&self) -> &Vec<TypedExportPattern> {
        match self {
            TypedExportPattern::Ident(_) => unreachable!(),
            TypedExportPattern::Tuple(patterns) => patterns,
        }
    }
}
impl TypedEnumVariant {
    pub fn ident(&self) -> &Ident {
        match self {
            TypedEnumVariant::Ident(ident) => ident,
            TypedEnumVariant::Valued(ident, ..) => ident,
            TypedEnumVariant::Variant(ident, ..) => ident,
        }
    }

    pub fn as_fielded_variant(&self) -> Option<&Vec<TypedEnumValuedField>> {
        match self {
            TypedEnumVariant::Variant(_, valued_fields) => Some(valued_fields),
            _ => None,
        }
    }
}

impl TypedBlockStmt {
    pub fn new_empty(scope_id: ScopeID, loc: SourceLoc) -> Self {
        Self {
            scope_id,
            stmts: Vec::new(),
            defers: Vec::new(),
            loc,
        }
    }
}

impl TypedFuncParams {
    pub fn is_instance_method(&self) -> bool {
        if let Some(param) = self.list.first() {
            param.as_self_modifier().is_some()
        } else {
            false
        }
    }
}

impl TypedFuncTypeParams {
    pub fn as_typed_variadic(&self) -> Option<SemanticType> {
        self.variadic.clone().and_then(|variadic| match *variadic {
            TypedFuncTypeVariadicParams::Typed(sema_ty) => Some(sema_ty),
            TypedFuncTypeVariadicParams::UntypedCStyle => None,
        })
    }
}

impl TypedFuncParamKind {
    pub fn loc(&self) -> SourceLoc {
        match self {
            TypedFuncParamKind::FuncParam(typed_func_param) => typed_func_param.loc.clone(),
            TypedFuncParamKind::SelfModifier(typed_self_modifier) => typed_self_modifier.loc.clone(),
        }
    }

    pub fn param_type(&self) -> Option<SemanticType> {
        match self {
            TypedFuncParamKind::FuncParam(func_param) => Some(func_param.ty.clone()),
            TypedFuncParamKind::SelfModifier(self_modifier) => self_modifier.ty.clone(),
        }
    }

    pub fn as_self_modifier(&self) -> Option<&TypedSelfModifier> {
        match self {
            TypedFuncParamKind::SelfModifier(self_modifier) => Some(self_modifier),
            TypedFuncParamKind::FuncParam(_) => None,
        }
    }

    pub fn as_self_modifier_mut(&mut self) -> Option<&mut TypedSelfModifier> {
        match self {
            TypedFuncParamKind::SelfModifier(self_modifier) => Some(self_modifier),
            TypedFuncParamKind::FuncParam(_) => None,
        }
    }
}

impl TypedSwitchStmt {
    pub fn includes_any_range(&self) -> bool {
        self.cases.iter().any(|case| {
            case.patterns
                .iter()
                .any(|p| matches!(p, TypedSwitchCasePattern::Range(_)))
        })
    }

    pub fn includes_only_integer(&self) -> bool {
        self.cases.iter().any(|case| {
            case.patterns.iter().any(|p| match p {
                TypedSwitchCasePattern::Expr(expr, _) => {
                    let sema_ty = expr.sema_ty.as_ref().unwrap();
                    sema_ty.is_char() || sema_ty.is_integer()
                }
                _ => false,
            })
        })
    }
}

impl Hash for TypedGenericParam {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.param_name.hash(state);
        if let Some(bounds) = &self.bounds {
            bounds.hash(state);
        }
    }
}

impl TypedGenericParamsList {
    pub fn new() -> Self {
        Self { list: Vec::new() }
    }

    pub fn push(&mut self, gp: TypedGenericParam) {
        self.list.push(gp);
    }

    pub fn resolve_symbol_id(&self, name: &String) -> Option<SymbolID> {
        self.lookup_named(name).map(|p| p.param_name.symbol_id)
    }

    pub fn lookup_named(&self, name: &String) -> Option<&TypedGenericParam> {
        self.list.iter().find(|p| &p.param_name.name == name)
    }

    pub fn lookup_positional(&self, idx: usize) -> Option<&TypedGenericParam> {
        self.list.get(idx)
    }
}

impl TypedTypeArg {
    pub fn as_named(&self) -> Option<(&String, &SemanticType)> {
        match self {
            TypedTypeArg::Named { key, ty, .. } => Some((key, ty)),
            _ => None,
        }
    }

    pub fn ty(&self) -> &SemanticType {
        match self {
            TypedTypeArg::Named { ty, .. } => ty,
            TypedTypeArg::Positional { ty, .. } => ty,
        }
    }

    pub fn ty_mut(&mut self) -> &mut SemanticType {
        match self {
            TypedTypeArg::Named { ty, .. } => ty,
            TypedTypeArg::Positional { ty, .. } => ty,
        }
    }
}

pub type TypedTypeArgs = Vec<TypedTypeArg>;

impl Hash for TypedTypeArg {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TypedTypeArg::Positional { idx, ty, .. } => {
                idx.hash(state);
                ty.hash(state);
            }
            TypedTypeArg::Named { key, ty, .. } => {
                key.hash(state);
                ty.hash(state);
            }
        }
    }
}

impl PartialEq for TypedTypeArg {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Positional {
                    idx: l_idx, ty: l_ty, ..
                },
                Self::Positional {
                    idx: r_idx, ty: r_ty, ..
                },
            ) => l_idx == r_idx && l_ty == r_ty,

            (
                Self::Named {
                    key: l_key, ty: l_ty, ..
                },
                Self::Named {
                    key: r_key, ty: r_ty, ..
                },
            ) => l_key == r_key && l_ty == r_ty,

            _ => false,
        }
    }
}

impl PartialEq for TypedFuncParam {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

impl PartialEq for TypedSelfModifier {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl PartialEq for TypedLambdaExpr {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.return_type == other.return_type
    }
}

impl PartialEq for TypedTupleExpr {
    fn eq(&self, other: &Self) -> bool {
        self.expr_list == other.expr_list
    }
}

impl PartialEq for TypedTupleAccessExpr {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand && self.index == other.index
    }
}

impl PartialEq for TypedIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.symbol_id == other.symbol_id
    }
}

impl Hash for TypedIdentifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.symbol_id.hash(state);
    }
}

pub fn lookup_symbol_from_generic_params(
    generic_params: &TypedGenericParamsList,
    symbol_id: SymbolID,
) -> Option<TypedGenericParam> {
    generic_params
        .list
        .iter()
        .find(|generic_param| generic_param.param_name.symbol_id == symbol_id)
        .cloned()
}
