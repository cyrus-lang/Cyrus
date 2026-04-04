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
    SymbolID, VTableID,
    decls::{FuncDeclID, InterfaceDeclID, MonomorphID, StructDeclID},
    stmts::{TypedBlockStmt, TypedBuiltin, TypedFuncParams, TypedTypeArgs},
    types::SemanticType,
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
    pub sema_type: Option<SemanticType>,
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
    StructInit(TypedStructInitExpr),
    UnnamedStructValue(TypedUnnamedStructValue),
    UnnamedEnumValue(TypedUnnamedEnumValue),
    UnnamedUnionValue(TypedUnnamedUnionValue),
    FuncCall(TypedFuncCall),
    MethodCall(TypedMethodCall),
    FieldAccess(TypedFieldAccess),
    Lambda(TypedLambdaExpr),
    Tuple(TypedTupleExpr),
    TupleAccess(TypedTupleAccessExpr),
    Dynamic(TypedDynamicExpr),
    SemanticType(SemanticType),
    Builtin(TypedBuiltin),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedSymbolExpr {
    pub symbol_id: SymbolID,
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
    pub ret_type: SemanticType,
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
    pub ty: Option<SemanticType>,
    pub kind: LiteralKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypedSelfType {
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq)]
pub struct TypedIdent {
    pub name: String,
    pub symbol_id: SymbolID,
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
    pub target_type: SemanticType,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArrayExpr {
    pub ty: Option<SemanticType>,
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
    pub symbol_id: Option<SymbolID>,
    pub struct_decl_id: Option<StructDeclID>,
    pub type_args: Option<TypedTypeArgs>,
    pub fields: Vec<TypedStructFieldInit>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructFieldInit {
    pub name: String,
    pub value: TypedExprStmt,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFuncCall {
    pub operand: Box<TypedExprStmt>,
    pub args: Vec<TypedExprStmt>,
    pub type_args: Option<TypedTypeArgs>,
    pub ret_type: Option<SemanticType>,
    pub monomorph_id: Option<MonomorphID>, // only set when calling a generic func
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFieldAccess {
    pub operand: Box<TypedExprStmt>,
    pub object_symbol_id: Option<SymbolID>,
    pub field_name: String,
    pub field_index: Option<usize>,
    pub field_ty: Option<SemanticType>,
    pub type_args: Option<TypedTypeArgs>,
    pub is_fat_arrow: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedInterfaceMethodCallMetadata {
    pub method_idx: usize,
    pub methods_len: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedMethodCall {
    pub operand: Box<TypedExprStmt>,
    pub method_name: String,
    pub args: Vec<TypedExprStmt>,
    pub type_args: Option<TypedTypeArgs>,

    pub dispatch: TypedMethodCallDispatch,

    pub is_fat_arrow: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedMethodCallDispatch {
    Unresolved,
    Direct {
        decl_id: FuncDeclID,
        self_type: SemanticType,
    },
    Interface {
        decl_id: InterfaceDeclID,
        self_type: SemanticType,
    },
    Monomorph {
        decl_id: FuncDeclID,
        monomorph_id: MonomorphID,
        self_type: SemanticType,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnnamedStructValue {
    pub fields: Vec<TypedUnnamedStructValueField>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedUnnamedUnionValue {
    pub name: Ident,
    pub value: Box<TypedExprStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedUnnamedEnumValue {
    pub ident: Ident,
    pub kind: TypedUnnamedEnumValueKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedUnnamedEnumValueKind {
    Plain,
    Fielded(Vec<TypedExprStmt>),
}
#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnnamedStructValueField {
    pub name: String,
    pub ty: Option<SemanticType>,
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
    pub fn is_dynamic_expr(&self) -> bool {
        match self {
            TypedExprKind::Dynamic(_) => true,
            _ => false,
        }
    }

    pub fn as_symbol_id(&self) -> Option<SymbolID> {
        match self {
            TypedExprKind::Symbol(TypedSymbolExpr { symbol_id, .. }) => Some(*symbol_id),
            _ => None,
        }
    }

    pub fn is_lvalue(&self) -> bool {
        match self {
            TypedExprKind::Symbol(..) => true,
            TypedExprKind::ArrayIndex(_) => true,
            TypedExprKind::Deref(_) => true,
            TypedExprKind::FieldAccess(_) => true,
            TypedExprKind::TupleAccess(_) => true,
            TypedExprKind::MethodCall(_) => false,
            TypedExprKind::FuncCall(_) => false,
            TypedExprKind::StructInit(_) => false,
            TypedExprKind::UnnamedStructValue(_) => false,
            TypedExprKind::UnnamedEnumValue(_) => false,
            TypedExprKind::Literal(_) => false,
            TypedExprKind::Prefix(_) => false,
            TypedExprKind::Infix(_) => false,
            TypedExprKind::Unary(_) => false,
            TypedExprKind::Assign(_) => false,
            TypedExprKind::AddrOf(_) => false,
            TypedExprKind::Array(_) => false,
            TypedExprKind::SemanticType(_) => false,
            TypedExprKind::Lambda(_) => false,
            TypedExprKind::Tuple(_) => false,
            TypedExprKind::Dynamic(_) => false,
            TypedExprKind::UnnamedUnionValue(_) => false,
            TypedExprKind::Builtin(_) => false,
        }
    }
}

impl fmt::Display for TypedLiteralExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl TypedLiteralExpr {
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
    pub fn as_fielded(&self) -> Option<&Vec<TypedExprStmt>> {
        match self {
            TypedUnnamedEnumValueKind::Plain => None,
            TypedUnnamedEnumValueKind::Fielded(exprs) => Some(exprs),
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
    pub fn new(symbol_id: SymbolID, loc: Loc) -> Self {
        Self { symbol_id, loc }
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
