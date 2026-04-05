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

use crate::abi::{ReprAttr, Visibility};
use crate::modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers};
use crate::operators::{InfixOperator, PrefixOperator, UnaryOperator};
use cyrusc_source_loc::Loc;
use cyrusc_tokens::TokenKind;
use cyrusc_tokens::{Token, literals::ASTLiteralExpr};
use std::fmt;
use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

pub mod abi;
pub mod format;
pub mod modifiers;
pub mod operators;

#[derive(Debug)]
pub struct ProgramTree {
    pub body: Rc<Vec<ASTStmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ASTExpr {
    Ident(Ident),
    TypeSpecifier(TypeSpecifier),
    ModuleImport(ASTModuleImport),
    Builtin(Builtin),
    Assign(Box<ASTAssignExpr>),
    Literal(ASTLiteralExpr),
    Prefix(ASTPrefixExpr),
    Infix(ASTInfixExpr),
    Unary(ASTUnaryExpr),
    Array(ASTArrayExpr),
    UntypedArray(ASTUntypedArrayExpr),
    ArrayIndex(ASTArrayIndexExpr),
    AddrOf(ASTAddrOfExpr),
    Deref(ASTDerefExpr),
    StructInit(ASTStructInitExpr),
    FuncCall(ASTFuncCallExpr),
    FieldAccess(ASTFieldAccessExpr),
    MethodCall(ASTMethodCallExpr),
    Lambda(ASTLambdaExpr),
    Tuple(ASTTupleValueExpr),
    TupleAccess(ASTTupleAccessExpr),
    Dynamic(ASTDynamicExpr),
    UnnamedStructValue(ASTUnnamedStructValueExpr),
    UnnamedUnionValue(ASTUnnamedUnionValueExpr),
    UnnamedEnumValue(ASTUnnamedEnumValueExpr),
}

#[derive(Debug, Clone)]
pub struct ASTModuleDecl {
    pub ident: Ident,
    pub vis: Visibility,
    pub stmts: Vec<ASTStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTDynamicExpr {
    pub operand: Box<ASTExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTTupleAccessExpr {
    pub operand: Box<ASTExpr>,
    pub index: usize,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTTupleValueExpr {
    pub elements: Vec<ASTExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTDerefExpr {
    pub expr: Box<ASTExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTAddrOfExpr {
    pub expr: Box<ASTExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTUnnamedStructValueExpr {
    pub fields: Vec<UnnamedStructValueField>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTUnnamedUnionValueExpr {
    pub field_name: Ident,
    pub field_value: Box<ASTExpr>,
    pub is_const: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTUnnamedEnumValueExpr {
    pub ident: Ident,
    pub kind: UnnamedEnumValueKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum UnnamedEnumValueKind {
    Plain,
    Fielded(Vec<ASTExpr>),
}

#[derive(Debug, Clone)]
pub struct UnnamedStructValueField {
    pub name: Ident,
    pub ty: Option<TypeSpecifier>,
    pub value: Box<ASTExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructType {
    pub fields: Vec<UnnamedStructTypeField>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructTypeField {
    pub name: Ident,
    pub ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedUnionType {
    pub fields: Vec<UnnamedUnionTypeField>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedUnionTypeField {
    pub field_name: Ident,
    pub field_ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTUnionStmt {
    pub ident: Ident,
    pub fields: Vec<UnionField>,
    pub generic_params: Option<GenericParamsList>,
    pub methods: Vec<ASTFuncDefStmt>,
    pub impls: Vec<TypeSpecifier>,
    pub modifiers: UnionModifiers,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnionField {
    pub ident: Ident,
    pub ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTEnumStmt {
    pub ident: Ident,
    pub variants: Vec<EnumVariant>,
    pub generic_params: Option<GenericParamsList>,
    pub methods: Vec<ASTFuncDefStmt>,
    pub impls: Vec<TypeSpecifier>,
    pub modifiers: EnumModifiers,
    pub tag_type: Option<TypeSpecifier>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedEnumType {
    pub variants: Vec<EnumVariant>,
    pub tag_type: Option<Box<TypeSpecifier>>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Unit(Ident),
    Valued {
        ident: Ident,
        value: Box<ASTExpr>,
    },
    Tuple {
        ident: Ident,
        fields: Vec<EnumVariantTupleField>,
    },
    Struct {
        ident: Ident,
        fields: Vec<EnumVariantStructField>,
    },
}

#[derive(Debug, Clone)]
pub struct EnumVariantTupleField {
    pub ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct EnumVariantStructField {
    pub name: Ident,
    pub ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTPrefixExpr {
    pub operand: Box<ASTExpr>,
    pub op: PrefixOperator,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Builtin {
    BuiltinFunc(BuiltinFunc),
    BuiltinScope(BuiltinScope),
}

#[derive(Debug, Clone)]
pub struct BuiltinFunc {
    pub name: Ident,
    pub args: Vec<ASTExpr>,
    pub child_stmt: Option<Box<ASTStmt>>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct BuiltinScope {
    pub name: Ident,
    pub args: Vec<ASTExpr>,
    pub block: Box<ASTBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTFuncCallExpr {
    pub operand: Box<ASTExpr>,
    pub args: Vec<ASTExpr>,
    pub type_args: Option<TypeArgs>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTFieldAccessExpr {
    pub is_fat_arrow: bool,
    pub operand: Box<ASTExpr>,
    pub field_name: Ident,
    pub type_args: Option<TypeArgs>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTMethodCallExpr {
    pub is_fat_arrow: bool,
    pub operand: Box<ASTExpr>,
    pub method_name: Ident,
    pub args: Vec<ASTExpr>,
    pub type_args: Option<TypeArgs>,
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq)]
pub struct Ident {
    pub value: String,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTModuleImport {
    pub segments: Vec<ModuleSegment>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSpecifier {
    TypeToken(Token),
    Ident(Ident),
    Const(Box<TypeSpecifier>),
    Array(ArrayType),
    ModuleImport(ASTModuleImport),
    Deref(Box<TypeSpecifier>),
    UnnamedStruct(UnnamedStructType),
    UnnamedUnion(UnnamedUnionType),
    UnnamedEnum(UnnamedEnumType),
    FuncType(Box<FuncType>),
    Tuple(TupleType),
    GenericInst(GenericInst),
    SelfType(SelfType),
}

#[derive(Debug, Clone)]
pub struct SelfType {
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct GenericInst {
    pub base: Box<TypeSpecifier>,
    pub type_args: TypeArgs,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TupleType {
    pub type_list: Vec<TypeSpecifier>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub size: ArrayCapacity,
    pub element_type: Box<TypeSpecifier>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrayCapacity {
    Fixed(Box<ASTExpr>),
    Dynamic,
}

#[derive(Debug, Clone)]
pub struct ASTUnaryExpr {
    pub operand: Box<ASTExpr>,
    pub op: UnaryOperator,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params: FuncTypeParams,
    pub ret_type: Box<TypeSpecifier>,
    pub vis_opt: Option<Visibility>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncTypeParams {
    pub list: Vec<TypeSpecifier>,
    pub variadic: Option<FuncTypeVariadicParams>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuncTypeVariadicParams {
    UntypedCStyle,
    Typed(TypeSpecifier),
}

#[derive(Debug, Clone)]
pub struct ASTInfixExpr {
    pub op: InfixOperator,
    pub lhs: Box<ASTExpr>,
    pub rhs: Box<ASTExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTArrayExpr {
    pub data_type: TypeSpecifier,
    pub elements: Vec<ASTExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTUntypedArrayExpr {
    pub elements: Vec<ASTExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTArrayIndexExpr {
    pub operand: Box<ASTExpr>,
    pub index: Box<ASTExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum ASTStmt {
    ModuleDecl(ASTModuleDecl),
    Builtin(Builtin),
    Import(ASTImportStmt),
    Variable(ASTVarStmt),
    ExportTuple(ASTExportTupleStmt),
    Expr(ASTExpr),
    If(ASTIfStmt),
    Return(ASTReturnStmt),
    FuncDef(ASTFuncDefStmt),
    FuncDecl(ASTFuncDeclStmt),
    For(ASTForStmt),
    While(ASTWhileStmt),
    Foreach(ASTForeachStmt),
    Switch(ASTSwitchStmt),
    BlockStmt(ASTBlockStmt),
    Interface(ASTInterfaceStmt),
    Struct(ASTStructStmt),
    Enum(ASTEnumStmt),
    Union(ASTUnionStmt),
    Break(ASTBreakStmt),
    Continue(ASTContinueStmt),
    Typedef(ASTTypedefStmt),
    GlobalVar(ASTGlobalVarStmt),
    Defer(ASTDeferStmt),
    Label(ASTLabelStmt),
    Goto(ASTGotoStmt),
}

#[derive(Debug, Clone)]
pub struct ASTGotoStmt {
    pub name: Ident,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTLabelStmt {
    pub name: Ident,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTDeferStmt {
    pub operand: Box<ASTStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTInterfaceStmt {
    pub ident: Ident,
    pub methods: Vec<ASTFuncDeclStmt>,
    pub generic_params: Option<GenericParamsList>,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTGlobalVarStmt {
    pub ident: Ident,
    pub type_spec: Option<TypeSpecifier>,
    pub expr: Option<ASTExpr>,
    pub is_const: bool,
    pub modifiers: GlobalVarModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTTypedefStmt {
    pub ident: Ident,
    pub type_spec: TypeSpecifier,
    pub generic_params: Option<GenericParamsList>,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTBreakStmt {
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTContinueStmt {
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTReturnStmt {
    pub argument: Option<ASTExpr>,

    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleSegmentSingle {
    pub ident: Ident,
    pub renamed: Option<Ident>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleSegment {
    SubModule(Ident),
    Single(Vec<ModuleSegmentSingle>),
}

#[derive(Debug, Clone)]
pub struct ModulePath {
    pub alias: Option<String>,
    pub segments: Vec<ModuleSegment>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTImportStmt {
    pub paths: Vec<ModulePath>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTStructStmt {
    pub ident: Ident,
    pub generic_params: Option<GenericParamsList>,
    pub impls: Vec<TypeSpecifier>,
    pub fields: Vec<StructField>,
    pub methods: Vec<ASTFuncDefStmt>,
    pub modifiers: StructModifiers,
    pub align: Option<usize>,
    pub is_packed: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTStructInitExpr {
    pub struct_name: ASTModuleImport,
    pub field_inits: Vec<FieldInit>,
    pub type_args: Option<TypeArgs>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub ident: Ident,
    pub vis: Visibility,
    pub ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct FieldInit {
    pub ident: Ident,
    pub value: ASTExpr,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTWhileStmt {
    pub condition: ASTExpr,
    pub body: Box<ASTBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTForStmt {
    pub initializer: Option<ASTVarStmt>,
    pub condition: Option<ASTExpr>,
    pub increment: Option<ASTExpr>,
    pub body: Box<ASTBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTForeachStmt {
    pub item: Ident,
    pub index: Option<Ident>,
    pub expr: ASTExpr,
    pub body: Box<ASTBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTSwitchStmt {
    pub operand: ASTExpr,
    pub cases: Vec<SwitchCase>,
    pub default_case: Option<ASTBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub patterns: Vec<SwitchCasePattern>,
    pub body: ASTBlockStmt,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum SwitchCasePattern {
    Expr(ASTExpr),
    Range(Range),
    Ident(Ident),
    EnumVariant(Ident, Vec<Ident>),
}

#[derive(Debug, Clone)]
pub struct Range {
    pub lower: ASTExpr,
    pub upper: ASTExpr,
    pub inclusive_upper: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTLambdaExpr {
    pub params: FuncParams,
    pub body: Box<ASTBlockStmt>,
    pub ret_type: TypeSpecifier,
    pub inline: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTFuncDefStmt {
    pub ident: Ident,
    pub generic_params: Option<GenericParamsList>,
    pub params: FuncParams,
    pub body: Box<ASTBlockStmt>,
    pub ret_type: Option<TypeSpecifier>,
    pub modifiers: FuncModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTFuncDeclStmt {
    pub ident: Ident,
    pub generic_params: Option<GenericParamsList>,
    pub params: FuncParams,
    pub ret_type: Option<TypeSpecifier>,
    pub modifiers: FuncModifiers,
    pub renamed_as: Option<Ident>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTBlockStmt {
    pub stmts: Vec<ASTStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTVarStmt {
    pub ident: Ident,
    pub ty: Option<TypeSpecifier>,
    pub rhs: Option<ASTExpr>,
    pub is_const: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ASTExportTupleStmt {
    pub pattern: ExportPattern,
    pub rhs: Option<ASTExpr>,
    pub is_const: bool,
    pub loc: Loc,
}
#[derive(Debug, Clone)]
pub struct ExportPattern {
    pub kind: ExportPatternKind,
    pub ty: Option<TypeSpecifier>,
    pub mutability: Option<Mutability>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum ExportPatternKind {
    Ident(Ident),
    Tuple(Vec<ExportPattern>),
    Ignore,
}

#[derive(Debug, Clone)]
pub struct ASTAssignExpr {
    pub lhs: ASTExpr,
    pub rhs: ASTExpr,
    pub kind: AssignKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignKind {
    Default,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitwiseAndAssign,
    BitwiseXorAssign,
    BitwiseAndNotAssign,
    LeftShiftAssign,
    RightShiftAssign,
}

#[derive(Debug, Clone)]
pub struct SelfModifier {
    pub kind: SelfModifierKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SelfModifierKind {
    Copied,
    Referenced,
}

#[derive(Debug, Clone)]
pub enum FuncParamKind {
    FuncParam(FuncParam),
    SelfModifier(SelfModifier),
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub ident: Ident,
    pub ty: Option<TypeSpecifier>,

    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct FuncParams {
    pub list: Vec<FuncParamKind>,
    pub variadic: Option<FuncVariadicParams>,
}

#[derive(Debug, Clone)]
pub enum FuncVariadicParams {
    UntypedCStyle,
    Typed(Ident, TypeSpecifier),
}

#[derive(Debug, Clone)]
pub struct ASTIfStmt {
    pub condition: ASTExpr,
    pub then_block: Box<ASTBlockStmt>,
    pub branches: Vec<ASTIfStmt>,
    pub else_block: Option<Box<ASTBlockStmt>>,

    pub loc: Loc,
}

pub type GenericParamsList = Vec<GenericParam>;

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub param_name: Ident,
    pub bounds: Option<Vec<Bound>>,
    pub default: Option<TypeSpecifier>,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub symbol: Ident,
    pub type_args: Vec<TypeArg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeArg {
    Positional(TypeSpecifier),
    Named { key: Ident, ty: TypeSpecifier },
}

pub type TypeArgs = Vec<TypeArg>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Const,
    Var,
}

// Returns the provided return type, or constructs a default `void` type.
///
/// This utility is used by the parser when building the raw AST to ensure
/// that functions without an explicit return type are assigned a `void`
/// return type. The resulting `TypeSpecifier` preserves the provided
/// source location and span for accurate diagnostics.
pub fn return_type_or_default_void(ret_type: Option<TypeSpecifier>, loc: Loc) -> TypeSpecifier {
    ret_type.unwrap_or(TypeSpecifier::TypeToken(Token {
        kind: TokenKind::Void,
        loc,
    }))
}

pub fn last_sub_module_index(segments: &Vec<ModuleSegment>) -> usize {
    segments
        .iter()
        .rposition(|seg| matches!(seg, ModuleSegment::SubModule(_)))
        .expect("import must contain at least one module segment")
}

#[inline]
pub fn sub_module_segments(segments: Vec<ModuleSegment>) -> Vec<Ident> {
    segments
        .iter()
        .filter_map(|module_segment| module_segment.as_ident())
        .collect()
}

impl ModuleSegment {
    pub fn loc(&self) -> Loc {
        match self {
            ModuleSegment::SubModule(ident) => ident.loc,
            ModuleSegment::Single(singles) => singles.first().unwrap().ident.loc,
        }
    }

    #[inline]
    pub fn as_ident(&self) -> Option<Ident> {
        match self {
            ModuleSegment::SubModule(ident) => Some(ident.clone()),
            ModuleSegment::Single(_) => None,
        }
    }
}

impl PartialEq for ModulePath {
    fn eq(&self, other: &Self) -> bool {
        let self_submodules: Vec<&String> = self
            .segments
            .iter()
            .filter_map(|segment| {
                if let ModuleSegment::SubModule(ident) = segment {
                    Some(&ident.value)
                } else {
                    None
                }
            })
            .collect();

        let other_submodules: Vec<&String> = other
            .segments
            .iter()
            .filter_map(|segment| {
                if let ModuleSegment::SubModule(ident) = segment {
                    Some(&ident.value)
                } else {
                    None
                }
            })
            .collect();

        self_submodules == other_submodules
    }
}

impl ModulePath {
    pub fn as_module_import(&self) -> ASTModuleImport {
        ASTModuleImport {
            segments: self.segments.clone(),
            loc: self.loc,
        }
    }
}

impl ASTFuncDefStmt {
    pub fn as_func_decl(&self) -> ASTFuncDeclStmt {
        ASTFuncDeclStmt {
            ident: self.ident.clone(),
            generic_params: self.generic_params.clone(),
            params: self.params.clone(),
            ret_type: self.ret_type.clone(),
            modifiers: self.modifiers.clone(),
            renamed_as: None,
            loc: self.loc,
        }
    }
}

impl ASTFuncDeclStmt {
    /// Returns the function's effective name, preferring the renamed version if available.
    pub fn usable_name(&self) -> String {
        match &self.renamed_as {
            Some(ident) => ident.value.clone(),
            None => self.ident.value.clone(),
        }
    }
}

impl ASTStmt {
    pub fn vis(&self) -> Option<Visibility> {
        match self {
            ASTStmt::ModuleDecl(module_decl) => Some(module_decl.vis),
            ASTStmt::FuncDef(func_def) => Some(func_def.modifiers.vis),
            ASTStmt::FuncDecl(func_decl) => Some(func_decl.modifiers.vis),
            ASTStmt::Interface(interface) => Some(interface.vis),
            ASTStmt::Struct(struct_stmt) => Some(struct_stmt.modifiers.vis),
            ASTStmt::Union(union_stmt) => Some(union_stmt.modifiers.vis),
            ASTStmt::Enum(enum_stmt) => Some(enum_stmt.modifiers.vis),
            ASTStmt::GlobalVar(global_var) => Some(global_var.modifiers.vis),

            ASTStmt::Builtin(_)
            | ASTStmt::For(_)
            | ASTStmt::While(_)
            | ASTStmt::Foreach(_)
            | ASTStmt::Switch(_)
            | ASTStmt::BlockStmt(_)
            | ASTStmt::Import(_)
            | ASTStmt::Variable(_)
            | ASTStmt::ExportTuple(_)
            | ASTStmt::Expr(_)
            | ASTStmt::If(_)
            | ASTStmt::Return(_)
            | ASTStmt::Break(_)
            | ASTStmt::Continue(_)
            | ASTStmt::Typedef(_)
            | ASTStmt::Defer(_)
            | ASTStmt::Label(_)
            | ASTStmt::Goto(_) => None,
        }
    }

    pub fn decl_name(&self) -> Option<&Ident> {
        match self {
            ASTStmt::ModuleDecl(module_decl) => Some(&module_decl.ident),
            ASTStmt::Variable(variable) => Some(&variable.ident),
            ASTStmt::FuncDef(func_def) => Some(&func_def.ident),
            ASTStmt::FuncDecl(func_decl) => Some(&func_decl.ident),
            ASTStmt::Interface(interface) => Some(&interface.ident),
            ASTStmt::Struct(struct_) => Some(&struct_.ident),
            ASTStmt::Enum(enum_) => Some(&enum_.ident),
            ASTStmt::Union(union_) => Some(&union_.ident),
            ASTStmt::Typedef(typedef) => Some(&typedef.ident),
            ASTStmt::GlobalVar(global_var) => Some(&global_var.ident),

            ASTStmt::Import(_)
            | ASTStmt::ExportTuple(_)
            | ASTStmt::Expr(_)
            | ASTStmt::If(_)
            | ASTStmt::Return(_)
            | ASTStmt::For(_)
            | ASTStmt::While(_)
            | ASTStmt::Foreach(_)
            | ASTStmt::Switch(_)
            | ASTStmt::BlockStmt(_)
            | ASTStmt::Break(_)
            | ASTStmt::Continue(_)
            | ASTStmt::Defer(_)
            | ASTStmt::Label(_)
            | ASTStmt::Goto(_)
            | ASTStmt::Builtin(_) => None,
        }
    }

    pub fn loc(&self) -> Loc {
        match self {
            ASTStmt::ModuleDecl(module_decl) => module_decl.loc,
            ASTStmt::Interface(interface) => interface.loc,
            ASTStmt::Variable(variable) => variable.loc,
            ASTStmt::ExportTuple(export_tuple_values) => export_tuple_values.loc,
            ASTStmt::If(if_stmt) => if_stmt.loc,
            ASTStmt::Return(ret) => ret.loc,
            ASTStmt::FuncDef(func_def) => func_def.loc,
            ASTStmt::FuncDecl(func_decl) => func_decl.loc,
            ASTStmt::For(for_stmt) => for_stmt.loc,
            ASTStmt::While(while_stmt) => while_stmt.loc,
            ASTStmt::Foreach(foreach) => foreach.loc,
            ASTStmt::Switch(switch) => switch.loc,
            ASTStmt::Struct(struct_stmt) => struct_stmt.loc,
            ASTStmt::Import(import) => import.loc,
            ASTStmt::BlockStmt(block_stmt) => block_stmt.loc,
            ASTStmt::Enum(enum_stmt) => enum_stmt.loc,
            ASTStmt::Union(union) => union.loc,
            ASTStmt::Break(break_stmt) => break_stmt.loc,
            ASTStmt::Continue(continue_stmt) => continue_stmt.loc,
            ASTStmt::Typedef(typedef) => typedef.loc,
            ASTStmt::GlobalVar(global_variable) => global_variable.loc,
            ASTStmt::Defer(defer) => defer.loc,
            ASTStmt::Label(label) => label.loc,
            ASTStmt::Goto(goto) => goto.loc,
            ASTStmt::Builtin(builtin) => match builtin {
                Builtin::BuiltinFunc(builtin_func) => builtin_func.loc,
                Builtin::BuiltinScope(builtin_scope) => builtin_scope.loc,
            },
            ASTStmt::Expr(..) => unreachable!(),
        }
    }
}

impl Default for ProgramTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramTree {
    pub fn new() -> Self {
        Self {
            body: Rc::new(Vec::new()),
        }
    }

    pub fn import_stmts(&self) -> Vec<ASTImportStmt> {
        let mut imports: Vec<ASTImportStmt> = Vec::new();

        self.body.iter().for_each(|stmt| match stmt {
            ASTStmt::Import(import) => {
                imports.push(import.clone());
            }
            _ => {}
        });

        imports
    }
}

impl Ident {
    pub fn new(value: &str, loc: Loc) -> Self {
        Self {
            value: value.to_string(),
            loc,
        }
    }

    #[inline]
    pub fn as_string(&self) -> String {
        self.value.clone()
    }
}

impl ModuleSegmentSingle {
    #[inline]
    pub fn visible_name(&self) -> String {
        self.renamed
            .clone()
            .map(|ident| ident.value)
            .unwrap_or(self.ident.as_string())
    }
}

impl ASTModuleImport {
    pub fn as_ident(&self) -> Option<Ident> {
        if self.segments.len() == 1 {
            match self.segments.last()? {
                ModuleSegment::SubModule(ident) => Some(ident.clone()),
                ModuleSegment::Single(_) => None,
            }
        } else {
            None
        }
    }
}

impl TypeSpecifier {
    pub fn as_type_token(&self) -> Option<&Token> {
        match self {
            TypeSpecifier::TypeToken(token) => Some(token),
            _ => None,
        }
    }

    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const(..))
    }

    pub fn as_module_import(&self) -> Option<ASTModuleImport> {
        match self {
            TypeSpecifier::Ident(ident) => Some(ASTModuleImport {
                segments: vec![ModuleSegment::SubModule(ident.clone())],
                loc: ident.loc,
            }),
            TypeSpecifier::ModuleImport(module_import) => Some(module_import.clone()),
            _ => None,
        }
    }

    pub fn loc(&self) -> Loc {
        match self {
            TypeSpecifier::TypeToken(token) => token.loc,
            TypeSpecifier::Ident(ident) => ident.loc,
            TypeSpecifier::Const(inner) => inner.loc(),
            TypeSpecifier::Array(array) => array.loc,
            TypeSpecifier::ModuleImport(module_import) => module_import.loc,
            TypeSpecifier::Deref(type_spec) => type_spec.loc(),
            TypeSpecifier::UnnamedStruct(unnamed_struct_type) => unnamed_struct_type.loc,
            TypeSpecifier::UnnamedUnion(unnamed_union_type) => unnamed_union_type.loc,
            TypeSpecifier::UnnamedEnum(unnamed_enum_type) => unnamed_enum_type.loc,
            TypeSpecifier::FuncType(func_type) => func_type.loc,
            TypeSpecifier::Tuple(tuple_type) => tuple_type.loc,
            TypeSpecifier::GenericInst(generic_inst) => generic_inst.loc,
            TypeSpecifier::SelfType(self_type) => self_type.loc,
        }
    }
}

impl Builtin {
    pub fn loc(&self) -> Loc {
        match self {
            Builtin::BuiltinFunc(builtin_func) => builtin_func.loc,
            Builtin::BuiltinScope(builtin_scope) => builtin_scope.loc,
        }
    }
}

impl AssignKind {
    pub fn to_infix_operator(&self) -> InfixOperator {
        match self {
            AssignKind::Default => unreachable!(),
            AssignKind::AddAssign => InfixOperator::Add,
            AssignKind::SubAssign => InfixOperator::Sub,
            AssignKind::MulAssign => InfixOperator::Mul,
            AssignKind::DivAssign => InfixOperator::Div,
            AssignKind::ModAssign => InfixOperator::Rem,
            AssignKind::BitwiseAndAssign => InfixOperator::BitwiseAnd,
            AssignKind::BitwiseXorAssign => InfixOperator::BitwiseXor,
            AssignKind::BitwiseAndNotAssign => InfixOperator::BitwiseAndNot,
            AssignKind::LeftShiftAssign => InfixOperator::ShiftLeft,
            AssignKind::RightShiftAssign => InfixOperator::ShiftRight,
        }
    }
}

impl fmt::Display for AssignKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignKind::Default => write!(f, "="),
            AssignKind::AddAssign => write!(f, "+="),
            AssignKind::SubAssign => write!(f, "-="),
            AssignKind::MulAssign => write!(f, "*="),
            AssignKind::DivAssign => write!(f, "/="),
            AssignKind::ModAssign => write!(f, "%="),
            AssignKind::BitwiseAndAssign => write!(f, "&="),
            AssignKind::BitwiseXorAssign => write!(f, "^="),
            AssignKind::BitwiseAndNotAssign => write!(f, "&~="),
            AssignKind::LeftShiftAssign => write!(f, "<<="),
            AssignKind::RightShiftAssign => write!(f, ">>="),
        }
    }
}

impl Hash for ModuleSegmentSingle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
        self.renamed.hash(state);
    }
}

impl Hash for ModulePath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for segment in &self.segments {
            if let ModuleSegment::SubModule(ident) = segment {
                ident.value.hash(state);
            }
        }
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl PartialEq for ASTModuleImport {
    fn eq(&self, other: &Self) -> bool {
        self.segments == other.segments
    }
}

impl PartialEq for UnnamedStructType {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields && self.repr_attr == other.repr_attr
    }
}

impl PartialEq for UnnamedStructTypeField {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

impl PartialEq for UnnamedUnionType {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields
    }
}

impl PartialEq for UnnamedUnionTypeField {
    fn eq(&self, other: &Self) -> bool {
        self.field_name == other.field_name && self.field_ty == other.field_ty
    }
}

impl PartialEq for FuncType {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.ret_type == other.ret_type
    }
}

impl PartialEq for TupleType {
    fn eq(&self, other: &Self) -> bool {
        self.type_list == other.type_list
    }
}

impl PartialEq for UnnamedEnumType {
    fn eq(&self, other: &Self) -> bool {
        self.variants == other.variants
    }
}

impl PartialEq for EnumVariant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit(l0), Self::Unit(r0)) => l0 == r0,
            (
                Self::Valued {
                    ident: l_ident,
                    value: l_value,
                },
                Self::Valued {
                    ident: r_ident,
                    value: r_value,
                },
            ) => l_ident == r_ident && l_value == r_value,
            (
                Self::Tuple {
                    ident: l_ident,
                    fields: l_fields,
                },
                Self::Tuple {
                    ident: r_ident,
                    fields: r_fields,
                },
            ) => l_ident == r_ident && l_fields == r_fields,
            (
                Self::Struct {
                    ident: l_ident,
                    fields: l_fields,
                },
                Self::Struct {
                    ident: r_ident,
                    fields: r_fields,
                },
            ) => l_ident == r_ident && l_fields == r_fields,
            _ => false,
        }
    }
}

impl PartialEq for EnumVariantTupleField {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl PartialEq for EnumVariantStructField {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

impl PartialEq for GenericInst {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base && self.type_args == other.type_args
    }
}

impl PartialEq for SelfType {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl PartialEq for ASTAssignExpr {
    fn eq(&self, other: &Self) -> bool {
        self.lhs == other.lhs && self.rhs == other.rhs && self.kind == other.kind
    }
}

impl PartialEq for ASTInfixExpr {
    fn eq(&self, other: &Self) -> bool {
        self.op == other.op && self.lhs == other.lhs && self.rhs == other.rhs
    }
}

impl PartialEq for ASTUnaryExpr {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand && self.op == other.op
    }
}

impl PartialEq for ASTArrayExpr {
    fn eq(&self, other: &Self) -> bool {
        self.data_type == other.data_type && self.elements == other.elements
    }
}

impl PartialEq for ASTUntypedArrayExpr {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl PartialEq for ASTArrayIndexExpr {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand && self.index == other.index
    }
}

impl PartialEq for ASTAddrOfExpr {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl PartialEq for ASTDerefExpr {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl PartialEq for ASTStructInitExpr {
    fn eq(&self, other: &Self) -> bool {
        self.struct_name == other.struct_name
            && self.field_inits == other.field_inits
            && self.type_args == other.type_args
    }
}

impl PartialEq for FieldInit {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.value == other.value
    }
}

impl PartialEq for ASTTupleValueExpr {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl PartialEq for ASTTupleAccessExpr {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand && self.index == other.index
    }
}

impl PartialEq for ASTDynamicExpr {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand
    }
}

impl PartialEq for ASTUnnamedStructValueExpr {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields && self.repr_attr == other.repr_attr && self.align == other.align
    }
}

impl PartialEq for ASTUnnamedUnionValueExpr {
    fn eq(&self, other: &Self) -> bool {
        self.field_name == other.field_name && self.field_value == other.field_value && self.is_const == other.is_const
    }
}

impl PartialEq for UnnamedStructValueField {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.value == other.value
    }
}

impl PartialEq for ASTUnnamedEnumValueExpr {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.kind == other.kind
    }
}

impl PartialEq for UnnamedEnumValueKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (UnnamedEnumValueKind::Plain, UnnamedEnumValueKind::Plain) => true,
            (UnnamedEnumValueKind::Fielded(exprs1), UnnamedEnumValueKind::Fielded(exprs2)) => exprs1 == exprs2,
            _ => false,
        }
    }
}

impl PartialEq for ASTLambdaExpr {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.ret_type == other.ret_type && self.inline == other.inline
    }
}

impl PartialEq for FuncParam {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.ty == other.ty
    }
}

impl PartialEq for FuncParams {
    fn eq(&self, other: &Self) -> bool {
        self.list == other.list && self.variadic == other.variadic
    }
}

impl PartialEq for FuncParamKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::FuncParam(func_param1), Self::FuncParam(func_param2)) => func_param1 == func_param2,
            (Self::SelfModifier(self_modifier1), Self::SelfModifier(self_modifier2)) => {
                self_modifier1 == self_modifier2
            }
            _ => false,
        }
    }
}

impl PartialEq for SelfModifier {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl PartialEq for FuncVariadicParams {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FuncVariadicParams::UntypedCStyle, FuncVariadicParams::UntypedCStyle) => true,
            (
                FuncVariadicParams::Typed(ident1, type_specifier1),
                FuncVariadicParams::Typed(ident2, type_specifier2),
            ) => ident1 == ident2 && type_specifier1 == type_specifier2,
            _ => false,
        }
    }
}

impl PartialEq for BuiltinScope {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args
    }
}

impl PartialEq for BuiltinFunc {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args && self.child_stmt.is_some() == other.child_stmt.is_some()
    }
}

impl Eq for BuiltinFunc {}
impl Eq for BuiltinScope {}
impl Eq for ASTArrayExpr {}
impl Eq for ASTUntypedArrayExpr {}
impl Eq for ASTArrayIndexExpr {}
impl Eq for ASTAddrOfExpr {}
impl Eq for ASTDerefExpr {}
impl Eq for ASTStructInitExpr {}
impl Eq for ASTLambdaExpr {}
impl Eq for ASTTupleValueExpr {}
impl Eq for ASTTupleAccessExpr {}
impl Eq for ASTDynamicExpr {}
impl Eq for ASTUnnamedStructValueExpr {}
impl Eq for ASTUnnamedUnionValueExpr {}
impl Eq for ASTUnnamedEnumValueExpr {}
impl Eq for ASTUnaryExpr {}
impl Eq for ASTInfixExpr {}
impl Eq for ASTAssignExpr {}
impl Eq for ASTModuleImport {}
impl Eq for ModulePath {}
impl Eq for UnnamedStructType {}
impl Eq for UnnamedStructTypeField {}
impl Eq for UnnamedUnionType {}
impl Eq for UnnamedUnionTypeField {}
impl Eq for FuncType {}
impl Eq for TupleType {}
impl Eq for UnnamedEnumType {}
impl Eq for GenericInst {}
impl Eq for SelfType {}
impl Eq for EnumVariantStructField {}
impl Eq for EnumVariantTupleField {}
