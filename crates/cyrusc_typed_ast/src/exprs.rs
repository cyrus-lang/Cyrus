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
    VTableID,
    decls::{DeclID, EnumDeclID, FuncDeclID, InterfaceDeclID, MonomorphID, StructDeclID, UnionDecl, UnionDeclID},
    stmts::{TypedBlockStmt, TypedBuiltin, TypedFuncParams, TypedTypeArgs},
    types::{SemaType, TypedFuncType},
};
use cyrusc_ast::{
    AssignKind, Ident,
    abi::ReprAttr,
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
};
use cyrusc_source_loc::Loc;
use cyrusc_tokens::literals::LiteralKind;
use std::fmt;

#[derive(Debug, Clone)]
pub struct TypedExprStmt {
    pub kind: TypedExprKind,
    pub sema_type: Option<SemaType>,
    pub mloc: MemoryLocation,
    pub loc: Loc,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MemoryLocation {
    LValue,
    RValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExprKind {
    Symbol(TypedSymbolExpr),
    Literal(TypedLiteralExpr),
    Prefix(TypedPrefixExpr),
    Infix(TypedInfixExpr),
    Unary(TypedUnaryExpr),
    Assign(TypedAssignExpr),
    AddrOf(TypedAddrOfExpr),
    Deref(TypedDerefExpr),
    Array(TypedArrayExpr),
    ArrayIndex(TypedArrayIndexExpr),
    UnnamedStructValue(TypedUnnamedStructValue),
    UnnamedUnionValue(TypedUnnamedUnionValue),
    UnnamedEnumValue(TypedUnnamedEnumValue),
    EnumStructVariantInit(TypedEnumStructVariantInit),
    EnumInit(TypedEnumInit),
    StructInit(TypedStructInitExpr),
    UnionInit(TypedUnionInitExpr),
    FuncCall(TypedFuncCall),
    MethodCall(TypedMethodCall),
    FieldAccess(TypedFieldAccess),
    Lambda(TypedLambdaExpr),
    Tuple(TypedTupleExpr),
    TupleAccess(TypedTupleAccessExpr),
    Dynamic(TypedDynamicExpr),
    SemaType(SemaType),
    Builtin(TypedBuiltin),

    Poisoned, // semantically wrong
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedSymbolExpr {
    pub decl_id: DeclID,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedDynamicExpr {
    pub operand: Box<TypedExprStmt>,
    pub object_name: Option<String>,
    pub vtable_id: Option<VTableID>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedTupleAccessExpr {
    pub operand: Box<TypedExprStmt>,
    pub index: usize,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedTupleExpr {
    pub elements: Vec<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedLambdaExpr {
    pub params: TypedFuncParams,
    pub body: Box<TypedBlockStmt>,
    pub ret_type: SemaType,
    pub inline: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedSizeOfExpr {
    pub operand: Box<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedLiteralExpr {
    pub ty: Option<SemaType>,
    pub kind: LiteralKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypedSelfType {
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedPrefixExpr {
    pub op: PrefixOperator,
    pub operand: Box<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnaryExpr {
    pub operand: Box<TypedExprStmt>,
    pub op: UnaryOperator,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedInfixExpr {
    pub op: InfixOperator,
    pub lhs: Box<TypedExprStmt>,
    pub rhs: Box<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedAssignExpr {
    pub lhs: Box<TypedExprStmt>,
    pub rhs: Box<TypedExprStmt>,
    pub kind: AssignKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedCastExpr {
    pub operand: Box<TypedExprStmt>,
    pub target_type: SemaType,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArrayExpr {
    pub ty: Option<SemaType>,
    pub elements: Vec<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArrayIndexExpr {
    pub operand: Box<TypedExprStmt>,
    pub index: Box<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedAddrOfExpr {
    pub operand: Box<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedDerefExpr {
    pub operand: Box<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructInitExpr {
    pub decl_id: DeclID,
    pub type_args: TypedTypeArgs,
    pub fields: Vec<TypedFieldInit>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnionInitExpr {
    pub decl_id: DeclID,
    pub type_args: TypedTypeArgs,
    pub field: Box<TypedFieldInit>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFieldInit {
    pub name: String,
    pub value: TypedExprStmt,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFuncCall {
    pub operand: Box<TypedExprStmt>,
    pub args: Vec<TypedExprStmt>,
    pub type_args: TypedTypeArgs,
    pub dispatch: TypedFuncCallDispatch,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedFuncCallDispatch {
    Unresolved,

    /// Normal function call
    Normal {
        func_decl_id: FuncDeclID,
    },

    /// Generic function instantiation
    Monomorph {
        monomorph_id: MonomorphID,
    },

    /// Calling a function pointer
    FunctionPointer {
        func_type: TypedFuncType,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFieldAccess {
    pub operand: Box<TypedExprStmt>,
    pub name: String,
    pub dispatch: TypedFieldAccessDispatch,
    pub ty: Option<SemaType>,
    pub is_fat_arrow: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedFieldAccessDispatch {
    Unresolved,

    Struct { struct_decl_id: StructDeclID, index: usize },

    Union { union_decl_id: UnionDeclID },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedEnumInit {
    pub enum_decl_id: EnumDeclID,
    pub name: String,
    pub args: TypedEnumInitArgs,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedEnumInitArgs {
    Unit,
    Tuple(Vec<TypedExprStmt>),
    Struct(Vec<TypedEnumStructVariantFieldInit>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedMethodCall {
    pub operand: Box<TypedExprStmt>,
    pub name: String,
    pub args: Vec<TypedExprStmt>,
    pub type_args: TypedTypeArgs,

    pub dispatch: TypedMethodCallDispatch,

    pub is_fat_arrow: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedMethodCallDispatch {
    Unresolved,
    Direct {
        decl_id: FuncDeclID,
        self_type: SemaType,
    },
    Interface {
        decl_id: InterfaceDeclID,
        self_type: SemaType,
        method_idx: usize,
        methods_len: usize,
    },
    Monomorph {
        decl_id: FuncDeclID,
        monomorph_id: MonomorphID,
        self_type: SemaType,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnnamedStructValue {
    pub struct_decl_id: Option<StructDeclID>,
    pub fields: Vec<TypedUnnamedStructValueField>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedUnnamedUnionValue {
    pub union_decl_id: Option<UnionDeclID>,
    pub name: Ident,
    pub value: Box<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedUnnamedEnumValue {
    pub enum_decl_id: Option<EnumDeclID>,
    pub ident: Ident,
    pub kind: TypedUnnamedEnumValueKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedUnnamedEnumValueKind {
    Unit,
    Tuple(Vec<TypedExprStmt>),
    Struct(Vec<TypedEnumStructVariantFieldInit>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedEnumStructVariantInit {
    pub enum_decl_id: Option<EnumDeclID>,
    pub operand: Box<TypedExprStmt>,
    pub ident: Ident,
    pub field_inits: Vec<TypedEnumStructVariantFieldInit>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedEnumStructVariantFieldInit {
    pub name: Ident,
    pub value: Box<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnnamedStructValueField {
    pub name: String,
    pub value: Box<TypedExprStmt>,
    pub loc: Loc,
}

impl TypedExprStmt {
    pub fn is_lvalue(&self) -> bool {
        self.mloc == MemoryLocation::LValue
    }

    pub fn is_rvalue(&self) -> bool {
        self.mloc == MemoryLocation::RValue
    }
}

impl TypedExprKind {
    #[inline]
    pub fn is_dynamic(&self) -> bool {
        match self {
            TypedExprKind::Dynamic(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn as_decl_id(&self) -> Option<DeclID> {
        match self {
            TypedExprKind::Symbol(TypedSymbolExpr {
                decl_id: symbol_decl_id,
                ..
            }) => Some(*symbol_decl_id),
            _ => None,
        }
    }

    #[inline]
    pub fn is_lvalue(&self) -> bool {
        match self {
            TypedExprKind::Symbol(..) => true,
            TypedExprKind::ArrayIndex(_) => true,
            TypedExprKind::Deref(_) => true,
            TypedExprKind::FieldAccess(_) => true,
            TypedExprKind::TupleAccess(_) => true,
            TypedExprKind::MethodCall(_)
            | TypedExprKind::FuncCall(_)
            | TypedExprKind::StructInit(_)
            | TypedExprKind::UnnamedStructValue(_)
            | TypedExprKind::UnnamedEnumValue(_)
            | TypedExprKind::Literal(_)
            | TypedExprKind::Prefix(_)
            | TypedExprKind::Infix(_)
            | TypedExprKind::Unary(_)
            | TypedExprKind::Assign(_)
            | TypedExprKind::AddrOf(_)
            | TypedExprKind::Array(_)
            | TypedExprKind::SemaType(_)
            | TypedExprKind::Lambda(_)
            | TypedExprKind::Tuple(_)
            | TypedExprKind::Dynamic(_)
            | TypedExprKind::UnnamedUnionValue(_)
            | TypedExprKind::Builtin(_)
            | TypedExprKind::EnumStructVariantInit(_)
            | TypedExprKind::UnionInit(_)
            | TypedExprKind::EnumInit(_) => false,

            TypedExprKind::Poisoned => unreachable!(),
        }
    }
}

impl fmt::Display for TypedLiteralExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl TypedLiteralExpr {
    #[inline]
    pub fn format_kind(&self) -> String {
        match &self.kind {
            LiteralKind::Integer { .. } => "integer",
            LiteralKind::Float(..) => "float",
            LiteralKind::String(..) => "string",
            LiteralKind::Bool(..) => "bool",
            LiteralKind::Char(..) => "char",
            LiteralKind::Null => "null",
        }
        .to_string()
    }
}

impl TypedUnnamedEnumValueKind {
    #[inline]
    pub fn as_fielded(&self) -> Option<&Vec<TypedExprStmt>> {
        match self {
            TypedUnnamedEnumValueKind::Unit => None,
            TypedUnnamedEnumValueKind::Struct(_) => None,
            TypedUnnamedEnumValueKind::Tuple(exprs) => Some(exprs),
        }
    }
}

impl PartialEq for TypedUnnamedEnumValue {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.kind == other.kind
    }
}

impl PartialEq for TypedExprStmt {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.sema_type == other.sema_type
    }
}

impl std::hash::Hash for TypedSelfType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // fixed tag
        0xDEAD_BEEF_u64.hash(state);
    }
}

impl PartialEq for TypedUnnamedUnionValue {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.value == other.value
    }
}

impl TypedSymbolExpr {
    pub fn new(decl_id: DeclID, loc: Loc) -> Self {
        Self { decl_id, loc }
    }
}

impl TypedExprStmt {
    pub fn literal_const_int_value(&self) -> Option<i128> {
        match &self.kind {
            TypedExprKind::Literal(lit) => match &lit.kind {
                LiteralKind::Integer(v, ..) => Some(*v),
                LiteralKind::Bool(v) => Some(if *v { 1 } else { 0 }),
                _ => None,
            },
            _ => None,
        }
    }
}

pub fn literal_expr_from_const_int(value: i128, loc: Loc) -> TypedExprStmt {
    TypedExprStmt {
        kind: TypedExprKind::Literal(TypedLiteralExpr {
            kind: LiteralKind::Integer(value, None),
            ty: None,
            loc: loc,
        }),
        sema_type: None,
        mloc: MemoryLocation::RValue,
        loc,
    }
}
