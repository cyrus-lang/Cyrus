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
use crate::types::{CIREnumTy, CIRFuncTy, CIRStructTy, CIRTy, CIRUnionTy};
use cyrusc_abi::modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers};
use cyrusc_ast::operators::{InfixOperator, PrefixOperator, UnaryOperator};
use cyrusc_tast::{LabelID, exprs::TypedIdentifier, generics::monomorph::MonomorphKey, sigs::StructSig};

pub mod monomorph;
pub mod types;
pub mod walk;

pub type IRValueID = u32;
pub type CIRBlockID = u32;

#[derive(Debug)]
pub struct CIRProgramTree {
    pub body: Vec<CIRStmt>,
    pub file_path: String,
}

#[derive(Debug, Clone)]
pub enum CIRStmt {
    Variable(CIRVarStmt),
    GlobalVar(CIRGlobalVarStmt),
    FuncDef(CIRFuncDefStmt),
    FuncDecl(CIRFuncDeclStmt),
    Block(CIRBlockStmt),
    Struct(CIRStructStmt),
    Enum(CIREnumStmt),
    Union(CIRUnionStmt),
    Expr(CIRExpr),
    If(CIRIfStmt),
    For(CIRForStmt),
    While(CIRWhileStmt),
    Switch(CIRSwitchStmt),
    SwitchOnEnum(CIRSwitchOnEnumStmt),
    Return(CIRReturnStmt),
    Label(CIRLabelStmt),
    Goto(CIRGotoStmt),
    Continue,
    Break,
}

#[derive(Debug, Clone)]
pub struct CIRExpr {
    pub kind: CIRExprKind,
    pub ty: CIRTy,
}

#[derive(Debug, Clone)]
pub enum CIRExprKind {
    Load(CIRValue),
    Literal(CIRLiteral),
    Prefix(CIRPrefixExpr),
    Infix(CIRInfixExpr),
    Unary(CIRUnaryExpr),
    SizeOf(CIRSizeOfExpr),
    Assign(CIRAssignExpr),
    Cast(CIRCastExpr),
    AddrOf(CIRAddrOfExpr),
    Deref(CIRDerefExpr),
    Array(CIRArrayExpr),
    ArrayIndex(CIRArrayIndexExpr),
    Tuple(CIRTupleExpr),
    TupleAccess(CIRTupleAccessExpr),
    StructInit(CIRStructInitExpr),
    UnionInit(CIRUnionInitExpr),
    EnumInit(CIREnumInitExpr),
    StructFieldAccess(CIRStructFieldAccessExpr),
    UnionFieldAccess(CIRUnionFieldAccessExpr),
    Lambda(CIRLambda),
    FuncCall(CIRFuncCall),
    MonomorphFuncInstanceCall(CIRMonomorphFuncInstanceCall),
}

#[derive(Debug, Clone)]
pub struct CIRLambda {
    pub irv_id: IRValueID,
    pub params: CIRFuncParams,
    pub ret: CIRTy,
    pub inline: bool,
    pub body: Box<CIRBlockStmt>,
}

#[derive(Debug, Clone)]
pub struct CIRFuncCall {
    pub operand: Box<CIRExpr>,
    pub args: Vec<CIRExpr>,
    pub ret_ty: CIRTy,
}

#[derive(Debug, Clone)]
pub struct CIRMonomorphFuncInstanceCall {
    pub monomorph_key: MonomorphKey,
    pub args: Vec<CIRExpr>,
    pub ret_ty: CIRTy,
}

#[derive(Debug, Clone)]
pub struct CIRUnionFieldAccessExpr {
    pub operand: Box<CIRExpr>,
    pub field_ty: CIRTy,
}

#[derive(Debug, Clone)]
pub struct CIRStructFieldAccessExpr {
    pub operand: Box<CIRExpr>,
    pub field_idx: usize,
    pub field_ty: CIRTy,
}

#[derive(Debug, Clone)]
pub struct CIRStructInitExpr {
    pub ty: CIRStructTy,
    pub fields: Vec<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIREnumInitExpr {
    pub tag: usize,
    pub variant: CIREnumInitVariant,
    pub enum_ty: CIREnumTy,
}

#[derive(Debug, Clone)]
pub struct CIRUnionInitExpr {
    pub expr: Box<CIRExpr>,
    pub ty: CIRTy,
}

#[derive(Debug, Clone)]
pub struct CIRTupleAccessExpr {
    pub operand: Box<CIRExpr>,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct CIRTupleExpr {
    pub elms: Vec<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRArrayIndexExpr {
    pub operand: Box<CIRExpr>,
    pub index: Box<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRArrayExpr {
    pub ty: CIRTy,
    pub elms: Vec<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRDerefExpr {
    pub operand: Box<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRAddrOfExpr {
    pub operand: Box<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRCastExpr {
    pub operand: Box<CIRExpr>,
    pub ty: Box<CIRTy>,
}

#[derive(Debug, Clone)]
pub struct CIRAssignExpr {
    pub lhs: Box<CIRExpr>,
    pub rhs: Box<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRSizeOfExpr {
    pub ty: CIRTy,
}

#[derive(Debug, Clone)]
pub struct CIRInfixExpr {
    pub op: InfixOperator,
    pub lhs: Box<CIRExpr>,
    pub rhs: Box<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRPrefixExpr {
    pub op: PrefixOperator,
    pub operand: Box<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRUnaryExpr {
    pub op: UnaryOperator,
    pub operand: Box<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRLiteral {
    pub kind: CIRLiteralKind,
    pub ty: CIRTy,
}

#[derive(Debug, Clone)]
pub enum CIRLiteralKind {
    Integer(i128, bool),
    Float(f64),
    Bool(bool),
    Char(char),
    Null,
    CString(String),
    ByteString(String),
}

#[derive(Debug, Clone)]
pub struct CIRValue {
    pub irv_id: IRValueID,
    pub kind: CIRValueKind,
}

#[derive(Debug, Clone)]
pub enum CIRValueKind {
    Func(Box<CIRFuncDeclStmt>),
    GlobalVar(Box<CIRGlobalVarStmt>),
    LocalVariable,
}

#[derive(Debug, Clone)]
pub struct CIRGlobalVarStmt {
    pub irv_id: IRValueID,
    pub name: String,
    pub ty: CIRTy,
    pub expr: Option<CIRExpr>,
    pub modifiers: GlobalVarModifiers,
}

#[derive(Debug, Clone)]
pub struct CIRVarStmt {
    pub irv_id: IRValueID,
    pub name: String,
    pub ty: CIRTy,
    pub expr: Option<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRFuncDefStmt {
    pub irv_id: IRValueID,
    pub name: String,
    pub params: CIRFuncParams,
    pub body: Box<CIRBlockStmt>,
    pub ret: CIRTy,
    pub modifiers: FuncModifiers,
}

#[derive(Debug, Clone)]
pub struct CIRFuncDeclStmt {
    pub irv_id: IRValueID,
    pub name: String,
    pub params: CIRFuncParams,
    pub ret: CIRTy,
    pub modifiers: FuncModifiers,
}

#[derive(Debug, Clone)]
pub struct CIRFuncParam {
    pub irv_id: IRValueID,
    pub ty: CIRTy,
}

#[derive(Debug, Clone)]
pub struct CIRFuncParams {
    pub list: Vec<CIRFuncParam>,
    pub is_var: bool,
}

#[derive(Debug, Clone)]
pub struct CIRBlockStmt {
    pub stmts: Vec<CIRStmt>,
}

#[derive(Debug, Clone)]
pub struct CIRIfStmt {
    pub cond: CIRExpr,
    pub then_block: Box<CIRBlockStmt>,
    pub else_block: Option<Box<CIRBlockStmt>>,
}

#[derive(Debug, Clone)]
pub struct CIRForStmt {
    pub initializer: Option<CIRVarStmt>,
    pub cond: Option<CIRExpr>,
    pub increment: Option<CIRExpr>,
    pub body: Box<CIRBlockStmt>,
}

#[derive(Debug, Clone)]
pub struct CIRWhileStmt {
    pub cond: Box<CIRExpr>,
    pub body: Box<CIRBlockStmt>,
}

#[derive(Debug, Clone)]
pub struct CIRSwitchStmt {
    pub value: CIRExpr,
    pub cases: Vec<CIRSwitchCase>,
    pub default: Option<CIRBlockStmt>,
}

#[derive(Debug, Clone)]
pub struct CIRSwitchCase {
    pub patterns: Vec<CIRExpr>,
    pub body: CIRBlockStmt,
}

#[derive(Debug, Clone)]
pub struct CIRSwitchOnEnumStmt {
    pub value: CIRExpr,
    pub cases: Vec<CIRSwitchOnEnumCase>,
    pub default: Option<CIRBlockStmt>,
}

#[derive(Debug, Clone)]
pub struct CIRSwitchOnEnumCase {
    pub patterns: Vec<CIRSwitchOnEnumPattern>,
    pub body: CIRBlockStmt,
}

#[derive(Debug, Clone)]
pub enum CIRSwitchOnEnumPattern {
    Ident(usize),
    Valued(usize, (TypedIdentifier, CIRExpr)),
    ExportFields(usize, Vec<(TypedIdentifier, CIRTy)>),
}

impl CIRSwitchOnEnumPattern {
    pub fn variant_idx(&self) -> usize {
        match self {
            CIRSwitchOnEnumPattern::Ident(variant_idx) => *variant_idx,
            CIRSwitchOnEnumPattern::ExportFields(variant_idx, ..) => *variant_idx,
            CIRSwitchOnEnumPattern::Valued(variant_idx, ..) => *variant_idx,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CIRReturnStmt {
    pub arg: Option<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRStructStmt {
    pub name: String,
    pub fields: Vec<CIRTy>,
    pub is_packed: bool,
    pub modifiers: StructModifiers,
}

#[derive(Debug, Clone)]
pub struct CIREnumStmt {
    pub name: String,
    pub variants: Vec<CIREnumTyVariant>,
    pub modifiers: EnumModifiers,
}

#[derive(Debug, Clone)]
pub enum CIREnumTyVariant {
    Ident,
    Valued(Box<CIRExpr>),
    Fielded(Vec<CIRTy>),
}

impl CIREnumTyVariant {
    pub fn as_fielded(&self) -> Option<&Vec<CIRTy>> {
        match self {
            CIREnumTyVariant::Fielded(fields) => Some(fields),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CIREnumInitVariant {
    Ident,
    Valued(Box<CIRExpr>),
    Fielded(Vec<CIRExpr>),
}

#[derive(Debug, Clone)]
pub struct CIRUnionStmt {
    pub name: String,
    pub fields: Vec<CIRTy>,
    pub modifiers: UnionModifiers,
}

#[derive(Debug, Clone)]
pub struct CIRLabelStmt {
    pub name: String,
    pub label_id: LabelID,
}

#[derive(Debug, Clone)]
pub struct CIRGotoStmt {
    pub label_id: LabelID,
}

pub fn cir_func_def_as_decl(func_def: &CIRFuncDefStmt) -> CIRFuncDeclStmt {
    CIRFuncDeclStmt {
        irv_id: func_def.irv_id,
        name: func_def.name.clone(),
        params: func_def.params.clone(),
        ret: func_def.ret.clone(),
        modifiers: func_def.modifiers.clone(),
    }
}

pub fn cir_func_decl_as_func_ty(func_decl: &CIRFuncDeclStmt) -> CIRFuncTy {
    CIRFuncTy {
        params: func_decl.params.list.iter().map(|param| param.ty.clone()).collect(),
        is_var: func_decl.params.is_var,
        ret: Box::new(func_decl.ret.clone()),
    }
}

pub fn cir_struct_as_struct_ty(struct_stmt: &CIRStructStmt) -> CIRStructTy {
    CIRStructTy {
        fields: struct_stmt.fields.clone(),
        is_packed: struct_stmt.is_packed,
    }
}

pub fn cir_enum_as_enum_ty(enum_stmt: &CIREnumStmt) -> CIREnumTy {
    CIREnumTy {
        variants: enum_stmt.variants.clone(),
    }
}

pub fn cir_union_as_union_ty(union_stmt: &CIRUnionStmt) -> CIRUnionTy {
    CIRUnionTy {
        fields: union_stmt.fields.clone(),
    }
}
