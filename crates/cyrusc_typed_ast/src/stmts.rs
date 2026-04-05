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
    LabelID, SymbolID,
    decls::{FuncDecl, MethodDecls, VarDeclID},
    exprs::{TypedExprStmt, TypedIdent, TypedLambdaExpr, TypedTupleAccessExpr, TypedTupleExpr},
    types::SemanticType,
};
use cyrusc_ast::{
    Ident, Mutability, SelfModifierKind,
    abi::{ReprKind, Visibility},
    modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers},
};
use cyrusc_source_loc::{FileID, Loc};
use std::hash::Hash;

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
    Builtin(TypedBuiltin),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedBuiltin {
    BuiltinFunc(TypedBuiltinFunc),
    BuiltinScope(TypedBuiltinScope),
}

#[derive(Debug, Clone)]
pub struct TypedBuiltinFunc {
    pub name: Ident,
    pub args: Vec<TypedExprStmt>,
    pub child_stmt: Option<Box<TypedStmt>>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedBuiltinScope {
    pub name: Ident,
    pub args: Vec<TypedExprStmt>,
    pub block: Box<TypedBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedLabelStmt {
    pub name: String,
    pub label_id: LabelID,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedGotoStmt {
    pub name: String,
    pub label_id: Option<LabelID>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedExportTupleStmt {
    pub pattern: TypedExportPattern,
    pub rhs: Option<TypedExprStmt>,
    pub is_const: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedExportPattern {
    pub kind: TypedExportPatternKind,
    pub ty: Option<SemanticType>,
    pub mutability: Option<Mutability>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum TypedExportPatternKind {
    Ident(SymbolID),
    Tuple(Vec<TypedExportPattern>),
    Ignore,
}

#[derive(Debug, Clone)]
pub struct TypedDeferStmt {
    pub operand: Box<TypedStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedImplementInterface {
    pub symbol_id: SymbolID,
    pub type_args: Option<TypedTypeArgs>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedInterfaceStmt {
    pub name: String,
    pub symbol_id: SymbolID,
    pub methods: Vec<TypedFuncDeclStmt>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedEnumStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub variants: Vec<TypedEnumVariant>,
    pub methods: MethodDecls,
    pub generic_params: Option<TypedGenericParamsList>,
    pub impls: Vec<TypedImplementInterface>,
    pub modifiers: EnumModifiers,
    pub tag_type: Option<SemanticType>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum TypedEnumVariant {
    Ident(Ident),
    Valued {
        ident: Ident,
        value: Box<TypedExprStmt>,
    },
    Tuple {
        ident: Ident,
        fields: Vec<TypedEnumVariantTupleField>,
    },
    Struct {
        ident: Ident,
        fields: Vec<TypedEnumVariantStructField>,
    },
}

#[derive(Debug, Clone)]
pub struct TypedEnumVariantTupleField {
    pub ty: SemanticType,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedEnumVariantStructField {
    pub name: Ident,
    pub ty: SemanticType,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedStructStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub methods: MethodDecls,
    pub generic_params: Option<TypedGenericParamsList>,
    pub impls: Vec<TypedImplementInterface>,
    pub modifiers: StructModifiers,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedUnionStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub fields: Vec<TypedUnionField>,
    pub methods: MethodDecls,
    pub generic_params: Option<TypedGenericParamsList>,
    pub impls: Vec<TypedImplementInterface>,
    pub align: Option<usize>,
    pub modifiers: UnionModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedUnionField {
    pub name: String,
    pub ty: SemanticType,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedStructField {
    pub name: String,
    pub ty: SemanticType,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedReturnStmt {
    pub arg: Option<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedGlobalVarStmt {
    pub file_id: FileID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: Option<SemanticType>,
    pub expr: Option<TypedExprStmt>,
    pub is_const: bool,
    pub modifiers: GlobalVarModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedTypedefStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: SemanticType,
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedBlockStmt {
    pub stmts: Vec<TypedStmt>,
    pub defers: Vec<TypedDeferStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedVarStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: Option<SemanticType>,
    pub rhs: Option<TypedExprStmt>,
    pub is_const: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedIfStmt {
    pub cond: TypedExprStmt,
    pub then_block: Box<TypedBlockStmt>,
    pub branches: Vec<TypedIfStmt>,
    pub else_block: Option<Box<TypedBlockStmt>>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDefStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub params: TypedFuncParams,
    pub generic_params: Option<TypedGenericParamsList>,
    pub body: Box<TypedBlockStmt>,
    pub ret_type: SemanticType,
    pub modifiers: FuncModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDeclStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub generic_params: Option<TypedGenericParamsList>,
    pub params: TypedFuncParams,
    pub ret_type: SemanticType,
    pub modifiers: FuncModifiers,
    pub renamed_as: Option<String>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedFuncParams {
    pub list: Vec<TypedFuncParamKind>,
    pub variadic: Option<TypedFuncVariadicParams>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedFuncVariadicParams {
    UntypedCStyle,
    Typed(TypedIdent, SemanticType),
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
    pub var_decl_id: Option<VarDeclID>,
    pub ty: Option<SemanticType>,
    pub kind: SelfModifierKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq)]
pub struct TypedFuncParam {
    pub var_decl_id: Option<VarDeclID>, // none if used in func decl
    pub name: String,
    pub ty: SemanticType,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedForStmt {
    pub initializer: Option<TypedVarStmt>,
    pub cond: Option<TypedExprStmt>,
    pub increment: Option<TypedExprStmt>,
    pub body: Box<TypedBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedWhileStmt {
    pub cond: TypedExprStmt,
    pub body: Box<TypedBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedSwitchStmt {
    pub operand: TypedExprStmt,
    pub cases: Vec<TypedSwitchCase>,
    pub default_case: Option<TypedBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedSwitchCase {
    pub patterns: Vec<TypedSwitchCasePattern>,
    pub body: Box<TypedBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum TypedSwitchCasePattern {
    /// `_`
    Wildcard,

    Binding {
        name: Ident,
        symbol_id: SymbolID,
    },

    /// `1..10` or `1..=10`
    Range(TypedRange),

    /// literal / constant expression
    Expr(TypedExprStmt),

    /// .Variant
    EnumUnit(Ident),

    /// .Variant(a, b, _)
    EnumTupleVariant {
        variant: Ident,
        items: Vec<TypedSwitchCasePattern>,
    },

    /// .Variant { a, b: x, c: _, .. }
    EnumStructVariant {
        variant: Ident,
        items: Vec<TypedSwitchCaseEnumStructPatternField>,
        has_rest: bool,
    },
}

#[derive(Debug, Clone)]
pub struct TypedSwitchCaseEnumStructPatternField {
    pub name: Ident,
    pub pattern: TypedSwitchCasePattern,
}

#[derive(Debug, Clone)]
pub struct TypedRange {
    pub lower: TypedExprStmt,
    pub upper: TypedExprStmt,
    pub inclusive_upper: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedBreakStmt {
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedContinueStmt {
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypedGenericParamsList {
    pub list: Vec<TypedGenericParam>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypedGenericParam {
    pub name: Ident,
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
    Positional { i: usize, ty: SemanticType, loc: Loc },
    Named { key: String, ty: SemanticType, loc: Loc },
}

impl TypedExportPattern {
    #[inline]
    pub fn as_tuple(&self) -> Option<&Vec<TypedExportPattern>> {
        match &self.kind {
            TypedExportPatternKind::Tuple(patterns) => Some(patterns),
            _ => None,
        }
    }
}

impl TypedFuncParams {
    pub fn as_func_type_params(&self) -> TypedFuncTypeParams {
        let list = self
            .list
            .iter()
            .map(|param_kind| match param_kind {
                TypedFuncParamKind::FuncParam(param) => param.ty.clone(),
                TypedFuncParamKind::SelfModifier(self_modifier) => self_modifier.ty.clone().unwrap(),
            })
            .collect();

        let variadic = match &self.variadic {
            Some(variadic) => match variadic {
                TypedFuncVariadicParams::UntypedCStyle => Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle)),
                TypedFuncVariadicParams::Typed(_, sema_type) => {
                    Some(Box::new(TypedFuncTypeVariadicParams::Typed(sema_type.clone())))
                }
            },
            None => None,
        };

        TypedFuncTypeParams { list, variadic }
    }
}

impl TypedStmt {
    pub fn loc(&self) -> Loc {
        match self {
            TypedStmt::Variable(typed_variable) => typed_variable.loc,
            TypedStmt::Typedef(typed_typedef) => typed_typedef.loc,
            TypedStmt::GlobalVar(typed_global_variable) => typed_global_variable.loc,
            TypedStmt::FuncDef(typed_func_def) => typed_func_def.loc,
            TypedStmt::FuncDecl(typed_func_decl) => typed_func_decl.loc,
            TypedStmt::BlockStmt(typed_block_statement) => typed_block_statement.loc,
            TypedStmt::If(typed_if) => typed_if.loc,
            TypedStmt::Return(typed_return) => typed_return.loc,
            TypedStmt::Break(typed_break) => typed_break.loc,
            TypedStmt::Continue(typed_continue) => typed_continue.loc,
            TypedStmt::For(typed_for) => typed_for.loc,
            TypedStmt::Switch(typed_switch) => typed_switch.loc,
            TypedStmt::Struct(struct_stmt) => struct_stmt.loc,
            TypedStmt::Enum(typed_enum) => typed_enum.loc,
            TypedStmt::Interface(typed_interface) => typed_interface.loc,
            TypedStmt::Expr(typed_expr) => typed_expr.loc,
            TypedStmt::While(while_stmt) => while_stmt.loc,
            TypedStmt::Union(union_stmt) => union_stmt.loc,
            TypedStmt::Defer(typed_defer) => typed_defer.loc,
            TypedStmt::ExportTuple(export_tuple_values) => export_tuple_values.loc,
            TypedStmt::Label(typed_label_stmt) => typed_label_stmt.loc,
            TypedStmt::Goto(typed_goto_stmt) => typed_goto_stmt.loc,
            TypedStmt::Builtin(typed_builtin) => typed_builtin.loc(),
        }
    }
}

impl TypedBuiltin {
    pub fn loc(&self) -> Loc {
        match self {
            TypedBuiltin::BuiltinFunc(builtin_func) => builtin_func.loc,
            TypedBuiltin::BuiltinScope(builtin_scope) => builtin_scope.loc,
        }
    }
}

impl TypedEnumStmt {
    pub fn is_repr_c(&self) -> bool {
        if let Some(repr_attr) = &self.modifiers.repr_attr {
            if let Some(kind) = repr_attr.kind() {
                return match kind {
                    ReprKind::C => true,
                    ReprKind::Cyrus => false,
                    ReprKind::Transparent => false,
                };
            }
        }
        false
    }
}

impl TypedBlockStmt {
    pub fn new_empty(loc: Loc) -> Self {
        Self {
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
            TypedFuncTypeVariadicParams::Typed(sema_type) => Some(sema_type),
            TypedFuncTypeVariadicParams::UntypedCStyle => None,
        })
    }
}

impl TypedFuncParamKind {
    pub fn name(&self) -> String {
        match self {
            TypedFuncParamKind::FuncParam(func_param) => func_param.name.clone(),
            TypedFuncParamKind::SelfModifier(_) => "self".to_string(),
        }
    }

    pub fn loc(&self) -> Loc {
        match self {
            TypedFuncParamKind::FuncParam(typed_func_param) => typed_func_param.loc,
            TypedFuncParamKind::SelfModifier(typed_self_modifier) => typed_self_modifier.loc,
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

impl TypedFuncDeclStmt {
    pub fn as_func_decl(&self) -> FuncDecl {
        FuncDecl {
            symbol_id: Some(self.symbol_id),
            name: self.usable_name(),
            params: self.params.clone(),
            generic_params: self.generic_params.clone(),
            ret_type: self.ret_type.clone(),
            modifiers: self.modifiers.clone(),
            loc: self.loc,
            is_func_decl: true,
        }
    }

    /// Returns the function's effective name, preferring the renamed version if available.
    pub fn usable_name(&self) -> String {
        match &self.renamed_as {
            Some(name) => name.clone(),
            None => self.name.clone(),
        }
    }
}

impl TypedEnumVariant {
    #[inline]
    pub fn ident(&self) -> &Ident {
        match self {
            TypedEnumVariant::Ident(ident) => ident,
            TypedEnumVariant::Valued { ident, .. } => ident,
            TypedEnumVariant::Tuple { ident, .. } => ident,
            TypedEnumVariant::Struct { ident, .. } => ident,
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
                TypedSwitchCasePattern::Expr(expr, ..) => {
                    let sema_type = expr.sema_type.as_ref().unwrap();
                    sema_type.is_char() || sema_type.is_integer()
                }
                _ => false,
            })
        })
    }
}

impl Hash for TypedGenericParam {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        if let Some(bounds) = &self.bounds {
            bounds.hash(state);
        }
    }
}

impl TypedFuncDefStmt {
    pub fn is_generic(&self) -> bool {
        self.generic_params.is_some()
    }
}

impl TypedGenericParamsList {
    pub fn new() -> Self {
        Self { list: Vec::new() }
    }

    pub fn push(&mut self, gp: TypedGenericParam) {
        self.list.push(gp);
    }

    pub fn lookup_named(&self, name: &String) -> Option<&TypedGenericParam> {
        self.list.iter().find(|generic_param| &generic_param.name.value == name)
    }

    pub fn lookup_positional(&self, i: usize) -> Option<&TypedGenericParam> {
        self.list.get(i)
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
            TypedTypeArg::Positional { i, ty, .. } => {
                i.hash(state);
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
            (Self::Positional { i: l_idx, ty: l_ty, .. }, Self::Positional { i: r_idx, ty: r_ty, .. }) => {
                l_idx == r_idx && l_ty == r_ty
            }
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
        self.params == other.params && self.ret_type == other.ret_type
    }
}

impl PartialEq for TypedTupleExpr {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl PartialEq for TypedTupleAccessExpr {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand && self.index == other.index
    }
}

impl PartialEq for TypedIdent {
    fn eq(&self, other: &Self) -> bool {
        self.symbol_id == other.symbol_id
    }
}

impl PartialEq for TypedBuiltinFunc {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args && self.child_stmt.is_some() == other.child_stmt.is_some()
    }
}

impl PartialEq for TypedBuiltinScope {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args
    }
}

impl Hash for TypedIdent {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.symbol_id.hash(state);
    }
}

impl Eq for TypedBuiltinFunc {}
impl Eq for TypedBuiltinScope {}
