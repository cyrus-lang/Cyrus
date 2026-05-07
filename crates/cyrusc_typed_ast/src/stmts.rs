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
    GenericParamID, LabelID,
    builtins::TypedBuiltin,
    decls::{
        EnumDeclID, FuncDecl, FuncDeclID, GlobalVarDeclID, InterfaceDeclID, MethodDecls, StructDeclID, TypedefDeclID,
        UnionDeclID, VarDeclID,
    },
    exprs::{TypedExpr, TypedLambdaExpr, TypedTupleAccessExpr, TypedTupleExpr},
    types::SemaType,
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
    Expr(TypedExpr),
    Defer(TypedDeferStmt),
    TupleExport(TypedTupleExportStmt),
    Label(TypedLabelStmt),
    Goto(TypedGotoStmt),
    Builtin(TypedBuiltin),
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
pub struct TypedTupleExportStmt {
    pub pattern: TypedTupleExportPattern,
    pub rhs: Option<TypedExpr>,
    pub is_const: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedTupleExportPattern {
    pub kind: TypedTupleExportPatternKind,
    pub ty: Option<SemaType>,
    pub mutability: Option<Mutability>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum TypedTupleExportPatternKind {
    Ident(VarDeclID),
    Tuple(Vec<TypedTupleExportPattern>),
    Ignore,
}

#[derive(Debug, Clone)]
pub struct TypedDeferStmt {
    pub operand: Box<TypedStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedImplementInterface {
    pub ty: SemaType,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedInterfaceStmt {
    pub name: String,
    pub interface_decl_id: InterfaceDeclID,
    pub methods: MethodDecls,
    pub generic_params: TypedGenericParams,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedEnumStmt {
    pub enum_decl_id: EnumDeclID,
    pub name: String,
    pub variants: Vec<TypedEnumVariant>,
    pub methods: MethodDecls,
    pub generic_params: TypedGenericParams,
    pub impls: Vec<TypedImplementInterface>,
    pub modifiers: EnumModifiers,
    pub tag_type: Option<SemaType>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum TypedEnumVariant {
    Unit(Ident),
    Valued {
        ident: Ident,
        value: Box<TypedExpr>,
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
    pub ty: SemaType,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedEnumVariantStructField {
    pub name: Ident,
    pub ty: SemaType,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedStructStmt {
    pub struct_decl_id: StructDeclID,
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub methods: MethodDecls,
    pub generic_params: TypedGenericParams,
    pub impls: Vec<TypedImplementInterface>,
    pub modifiers: StructModifiers,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedUnionStmt {
    pub union_decl_id: UnionDeclID,
    pub name: String,
    pub fields: Vec<TypedUnionField>,
    pub methods: MethodDecls,
    pub generic_params: TypedGenericParams,
    pub impls: Vec<TypedImplementInterface>,
    pub align: Option<usize>,
    pub modifiers: UnionModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedUnionField {
    pub name: String,
    pub ty: SemaType,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedStructField {
    pub name: String,
    pub ty: SemaType,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedReturnStmt {
    pub arg: Option<TypedExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedGlobalVarStmt {
    pub file_id: FileID,
    pub global_var_decl_id: GlobalVarDeclID,
    pub name: String,
    pub ty: Option<SemaType>,
    pub expr: Option<TypedExpr>,
    pub is_const: bool,
    pub modifiers: GlobalVarModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedTypedefStmt {
    pub typedef_decl_id: TypedefDeclID,
    pub name: String,
    pub ty: SemaType,
    pub generic_params: TypedGenericParams,
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
    pub var_decl_id: VarDeclID,
    pub name: String,
    pub ty: Option<SemaType>,
    pub rhs: Option<TypedExpr>,
    pub is_const: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedIfStmt {
    pub cond: TypedExpr,
    pub then_block: Box<TypedBlockStmt>,
    pub branches: Vec<TypedIfStmt>,
    pub else_block: Option<Box<TypedBlockStmt>>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDefStmt {
    pub func_decl_id: Option<FuncDeclID>,
    pub name: String,
    pub params: TypedFuncParams,
    pub generic_params: TypedGenericParams,
    pub body: Box<TypedBlockStmt>,
    pub ret_type: SemaType,
    pub modifiers: FuncModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDeclStmt {
    pub file_id: FileID,
    pub func_decl_id: FuncDeclID,
    pub name: String,
    pub generic_params: TypedGenericParams,
    pub params: TypedFuncParams,
    pub ret_type: SemaType,
    pub modifiers: FuncModifiers,
    pub renamed_as: Option<String>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedFuncParams {
    pub list: Vec<TypedFuncParamKind>,
    pub variadic: Option<TypedFuncVariadicParam>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedFuncVariadicParam {
    UntypedCStyle,
    Typed {
        var_decl_id: VarDeclID,
        ty: SemaType,
        loc: Loc,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedFuncTypeParams {
    pub list: Vec<SemaType>,
    pub variadic: Option<Box<TypedFuncTypeVariadicParam>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypedFuncTypeVariadicParam {
    UntypedCStyle,
    Typed(SemaType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedFuncParamKind {
    FuncParam(TypedFuncParam),
    SelfModifier(TypedSelfModifier),
}

#[derive(Debug, Clone, Eq)]
pub struct TypedSelfModifier {
    // none if used in func decl (no body)
    pub var_decl_id: Option<VarDeclID>,

    pub ty: SemaType,
    pub kind: SelfModifierKind,
    pub mutability: Mutability,
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq)]
pub struct TypedFuncParam {
    // none if used in func decl (no body)
    pub var_decl_id: Option<VarDeclID>,

    pub ident: Ident,
    pub ty: SemaType,
    pub mutability: Mutability,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedForStmt {
    pub initializer: Option<TypedVarStmt>,
    pub cond: Option<TypedExpr>,
    pub increment: Option<TypedExpr>,
    pub body: Box<TypedBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedWhileStmt {
    pub cond: TypedExpr,
    pub body: Box<TypedBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedSwitchStmt {
    pub operand: TypedExpr,
    pub cases: Vec<TypedSwitchCase>,
    pub default_case: Option<TypedBlockStmt>,
    pub all_cases_covered: Option<bool>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedSwitchCase {
    pub patterns: Vec<TypedSwitchCasePattern>,
    pub body: Box<TypedBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedSwitchCasePattern {
    pub kind: TypedSwitchCasePatternKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum TypedSwitchCasePatternKind {
    /// `_`
    Wildcard,

    Binding {
        name: Ident,
        var_decl_id: VarDeclID,
    },

    /// `1..10` or `1..=10`
    Range(TypedRange),

    /// literal / constant expression
    Expr(TypedExpr),

    /// .Variant
    EnumUnit(Ident),

    /// .Variant(a, b, _)
    EnumTupleVariant {
        ident: Ident,
        items: Vec<TypedSwitchCasePattern>,
    },

    /// .Variant { a, b: x, c: _, .. }
    EnumStructVariant {
        ident: Ident,
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
    pub lower: TypedExpr,
    pub upper: TypedExpr,
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
pub struct TypedGenericParams(pub Vec<GenericParamID>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypedGenericParam {
    pub name: Ident,
    pub bounds: Vec<TypedBound>,
    pub default: Option<Box<SemaType>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TypedTypeArgs(pub Vec<TypedTypeArg>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypedTypeArg {
    Type(SemaType, Loc),
    Infer,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TypedBound {
    pub ty: SemaType,
    pub loc: Loc,
}

impl TypedGenericParams {
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &GenericParamID> {
        self.0.iter()
    }
}

impl TypedTypeArgs {
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&TypedTypeArg> {
        self.0.get(index)
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &TypedTypeArg> {
        self.0.iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut TypedTypeArg> {
        self.0.iter_mut()
    }

    #[inline]
    pub fn push(&mut self, ty: TypedTypeArg) {
        self.0.push(ty);
    }
}

impl FromIterator<TypedTypeArg> for TypedTypeArgs {
    fn from_iter<T: IntoIterator<Item = TypedTypeArg>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl TypedTupleExportPattern {
    #[inline]
    pub fn as_tuple(&self) -> Option<&Vec<TypedTupleExportPattern>> {
        match &self.kind {
            TypedTupleExportPatternKind::Tuple(patterns) => Some(patterns),
            _ => None,
        }
    }
}

impl TypedStructStmt {
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }
}

impl TypedEnumStmt {
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }
}

impl TypedUnionStmt {
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }
}

impl TypedFuncParams {
    pub fn lookup_param(&self, name: &str) -> Option<&TypedFuncParamKind> {
        self.list.iter().find(|param_kind| param_kind.name().as_str() == name)
    }

    pub fn as_func_type_params(&self) -> TypedFuncTypeParams {
        let list = self
            .list
            .iter()
            .map(|param_kind| match param_kind {
                TypedFuncParamKind::FuncParam(param) => param.ty.clone(),
                TypedFuncParamKind::SelfModifier(self_modifier) => self_modifier.ty.clone(),
            })
            .collect();

        let variadic = match &self.variadic {
            Some(variadic) => match variadic {
                TypedFuncVariadicParam::UntypedCStyle => Some(Box::new(TypedFuncTypeVariadicParam::UntypedCStyle)),
                TypedFuncVariadicParam::Typed { ty, .. } => {
                    Some(Box::new(TypedFuncTypeVariadicParam::Typed(ty.clone())))
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
            TypedStmt::TupleExport(export_tuple_values) => export_tuple_values.loc,
            TypedStmt::Label(typed_label_stmt) => typed_label_stmt.loc,
            TypedStmt::Goto(typed_goto_stmt) => typed_goto_stmt.loc,
            TypedStmt::Builtin(typed_builtin) => typed_builtin.loc(),
        }
    }
}

impl TypedFuncDeclStmt {
    #[inline]
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }
}

impl TypedBuiltin {
    pub fn loc(&self) -> Loc {
        match self {
            TypedBuiltin::BuiltinFunc(builtin_func) => builtin_func.loc,
            TypedBuiltin::BuiltinBlock(builtin_block) => builtin_block.loc,
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
    #[inline]
    pub fn is_instance_method(&self) -> bool {
        if let Some(param) = self.list.first() {
            param.as_self_modifier().is_some()
        } else {
            false
        }
    }

    #[inline]
    pub fn get_self_modifier(&self) -> Option<&TypedSelfModifier> {
        self.list.first().and_then(|param_kind| match param_kind {
            TypedFuncParamKind::SelfModifier(self_modifier) => Some(self_modifier),
            TypedFuncParamKind::FuncParam(_) => None,
        })
    }

    #[inline]
    pub fn get_self_modifier_mut(&mut self) -> Option<&mut TypedSelfModifier> {
        self.list.first_mut().and_then(|param_kind| match param_kind {
            TypedFuncParamKind::SelfModifier(self_modifier) => Some(self_modifier),
            TypedFuncParamKind::FuncParam(_) => None,
        })
    }
}

impl TypedFuncTypeParams {
    pub fn as_typed_variadic(&self) -> Option<SemaType> {
        self.variadic.clone().and_then(|variadic| match *variadic {
            TypedFuncTypeVariadicParam::Typed(sema_type) => Some(sema_type),
            TypedFuncTypeVariadicParam::UntypedCStyle => None,
        })
    }
}

impl TypedFuncParamKind {
    #[inline]
    pub fn is_const(&self) -> bool {
        match self {
            TypedFuncParamKind::FuncParam(func_param) => func_param.mutability.is_const(),
            TypedFuncParamKind::SelfModifier(self_modifier) => self_modifier.mutability.is_const(),
        }
    }

    #[inline]
    pub fn name(&self) -> String {
        match self {
            TypedFuncParamKind::FuncParam(func_param) => func_param.ident.as_string(),
            TypedFuncParamKind::SelfModifier(_) => "self".to_string(),
        }
    }

    #[inline]
    pub fn loc(&self) -> Loc {
        match self {
            TypedFuncParamKind::FuncParam(typed_func_param) => typed_func_param.loc,
            TypedFuncParamKind::SelfModifier(typed_self_modifier) => typed_self_modifier.loc,
        }
    }

    #[inline]
    pub fn var_decl_id(&self) -> Option<VarDeclID> {
        match self {
            TypedFuncParamKind::FuncParam(func_param) => func_param.var_decl_id,
            TypedFuncParamKind::SelfModifier(self_modifier) => self_modifier.var_decl_id,
        }
    }

    #[inline]
    pub fn param_type(&self) -> SemaType {
        match self {
            TypedFuncParamKind::FuncParam(func_param) => func_param.ty.clone(),
            TypedFuncParamKind::SelfModifier(self_modifier) => self_modifier.ty.clone(),
        }
    }

    #[inline]
    pub fn param_type_mut(&mut self) -> &mut SemaType {
        match self {
            TypedFuncParamKind::FuncParam(func_param) => &mut func_param.ty,
            TypedFuncParamKind::SelfModifier(self_modifier) => &mut self_modifier.ty,
        }
    }

    #[inline]
    pub fn as_self_modifier(&self) -> Option<&TypedSelfModifier> {
        match self {
            TypedFuncParamKind::SelfModifier(self_modifier) => Some(self_modifier),
            TypedFuncParamKind::FuncParam(_) => None,
        }
    }

    #[inline]
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
            file_id: self.file_id,
            name: self.usable_name(),
            params: self.params.clone(),
            generic_params: self.generic_params.clone(),
            ret_type: self.ret_type.clone(),
            modifiers: self.modifiers.clone(),
            loc: self.loc,
            body: None,
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
    pub fn get_struct_variant_field_type(&self, name: &str) -> Option<SemaType> {
        match self {
            TypedEnumVariant::Struct { fields, .. } => fields
                .iter()
                .find(|field| field.name.value == name)
                .map(|field| field.ty.clone()),

            _ => None,
        }
    }

    pub fn get_struct_variant_field_index(&self, name: &str) -> Option<usize> {
        match self {
            TypedEnumVariant::Struct { fields, .. } => fields.iter().position(|field| field.name.value == name),

            _ => None,
        }
    }

    #[inline]
    pub fn ident(&self) -> &Ident {
        match self {
            TypedEnumVariant::Unit(ident) => ident,
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
                .any(|pattern| matches!(&pattern.kind, TypedSwitchCasePatternKind::Range(_)))
        })
    }

    pub fn includes_only_integer(&self) -> bool {
        self.cases.iter().any(|case| {
            case.patterns.iter().any(|pattern| match &pattern.kind {
                TypedSwitchCasePatternKind::Expr(expr, ..) => {
                    let sema_type = expr.ty.as_ref().unwrap();
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
        self.bounds.hash(state);
    }
}

impl TypedFuncDefStmt {
    pub fn is_generic(&self) -> bool {
        !self.generic_params.is_empty()
    }
}

impl PartialEq for TypedFuncParam {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.ty == other.ty
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

impl Hash for TypedTypeArg {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TypedTypeArg::Type(sema_type, _) => {
                0u8.hash(state);
                sema_type.hash(state);
            }
            TypedTypeArg::Infer => {
                1u8.hash(state);
            }
        }
    }
}
