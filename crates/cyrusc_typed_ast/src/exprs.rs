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
    decls::{DeclID, EnumDeclID, FuncDeclID, InterfaceDeclID, MethodDeclID, MonomorphID, StructDeclID, UnionDeclID},
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
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Option<SemaType>,
    pub val_cat: ValueCategory,
    pub loc: Loc,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueCategory {
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
    pub operand: Box<TypedExpr>,
    pub object_name: Option<String>,
    pub ty: Option<SemaType>,
    pub concrete_type: Option<SemaType>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedTupleAccessExpr {
    pub operand: Box<TypedExpr>,
    pub index: usize,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedTupleExpr {
    pub elements: Vec<TypedExpr>,
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
    pub operand: Box<TypedExpr>,
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
    pub operand: Box<TypedExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnaryExpr {
    pub operand: Box<TypedExpr>,
    pub op: UnaryOperator,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedInfixExpr {
    pub op: InfixOperator,
    pub lhs: Box<TypedExpr>,
    pub rhs: Box<TypedExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedAssignExpr {
    pub lhs: Box<TypedExpr>,
    pub rhs: Box<TypedExpr>,
    pub kind: AssignKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedCastExpr {
    pub operand: Box<TypedExpr>,
    pub target_type: SemaType,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArrayExpr {
    pub ty: Option<SemaType>,
    pub elements: Vec<TypedExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArrayIndexExpr {
    pub operand: Box<TypedExpr>,
    pub index: Box<TypedExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedAddrOfExpr {
    pub operand: Box<TypedExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedDerefExpr {
    pub operand: Box<TypedExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructInitExpr {
    pub operand: SemaType,
    pub fields: Vec<TypedFieldInit>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnionInitExpr {
    pub operand: SemaType,
    pub field: Box<TypedFieldInit>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFieldInit {
    pub name: String,
    pub value: TypedExpr,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFuncCall {
    pub operand: Box<TypedExpr>,
    pub args: Vec<TypedExpr>,
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
    pub operand: Box<TypedExpr>,
    pub name: String,
    pub dispatch: TypedFieldAccessDispatch,
    pub ty: Option<SemaType>,
    pub is_thin_arrow: bool,
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
    pub operand: SemaType,
    pub name: String,
    pub arg: TypedEnumInitArgs,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedEnumInitArgs {
    Unit,
    Tuple(Vec<TypedExpr>),
    Struct(Vec<TypedEnumStructVariantFieldInit>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedMethodCall {
    pub operand: Box<TypedExpr>,
    pub name: String,
    pub args: Vec<TypedExpr>,
    pub type_args: TypedTypeArgs,

    pub dispatch: TypedMethodCallDispatch,

    pub is_thin_arrow: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedMethodCallDispatch {
    Unresolved,

    Normal {
        method_decl_id: MethodDeclID,
    },

    Interface {
        vtable_id: VTableID,
        interface_decl_id: InterfaceDeclID,

        index: usize,
        methods_len: usize,
        method_self_type: SemaType,
    },

    Monomorph {
        monomorph_id: MonomorphID,
        is_instance_method: bool,
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
    pub value: Box<TypedExpr>,
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
    Tuple(Vec<TypedExpr>),
    Struct(Vec<TypedEnumStructVariantFieldInit>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedEnumStructVariantInit {
    pub enum_decl_id: Option<EnumDeclID>,
    pub operand: Box<TypedExpr>,
    pub ident: Ident,
    pub field_inits: Vec<TypedEnumStructVariantFieldInit>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedEnumStructVariantFieldInit {
    pub name: Ident,
    pub value: Box<TypedExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnnamedStructValueField {
    pub name: String,
    pub value: Box<TypedExpr>,
    pub loc: Loc,
}

impl TypedExpr {
    pub fn is_lvalue(&self) -> bool {
        self.val_cat == ValueCategory::LValue
    }

    pub fn is_rvalue(&self) -> bool {
        self.val_cat == ValueCategory::RValue
    }
}

impl TypedExpr {
    pub fn extract_dynamic_expr_concrete_type(&self) -> Option<&SemaType> {
        match &self.kind {
            TypedExprKind::Dynamic(dynamic) => dynamic.concrete_type.as_ref(),
            _ => None,
        }
    }
}

impl TypedExprKind {
    #[inline]
    pub fn as_type_expr(&self) -> Option<SemaType> {
        match self {
            TypedExprKind::SemaType(sema_type) => Some(sema_type.clone()),
            _ => None,
        }
    }

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
    pub fn as_fielded(&self) -> Option<&Vec<TypedExpr>> {
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

impl PartialEq for TypedExpr {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.ty == other.ty
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

impl TypedExpr {
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

pub fn literal_expr_from_const_int(value: i128, loc: Loc) -> TypedExpr {
    TypedExpr {
        kind: TypedExprKind::Literal(TypedLiteralExpr {
            kind: LiteralKind::Integer(value, None),
            ty: None,
            loc: loc,
        }),
        ty: None,
        val_cat: ValueCategory::RValue,
        loc,
    }
}
