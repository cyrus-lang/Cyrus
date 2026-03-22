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
    abi::args::ABIFunctionInfo,
    cir::types::{CIREnumTy, CIRFuncTy, CIRStructTy, CIRTy, CIRUnionTy},
};
use cyrusc_ast::{
    abi::ReprKind,
    modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers},
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{LabelID, exprs::TypedIdentifier, generics::monomorph::MonomorphKey};
use std::fmt::Debug;

pub type IRValueID = u32;
pub type CIRBlockID = u32;

pub struct CIRProgramTree {
    pub body: Vec<CIRStmt>,
    pub file_path: String,
    pub module_name: String,
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
    Defer(CIRDeferStmt),
    Continue(Loc),
    Break(Loc),
}

#[derive(Debug, Clone)]
pub struct CIRExpr {
    pub kind: CIRExprKind,
    pub ty: CIRTy,
    pub loc: Loc,
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
    Dynamic(CIRDynamicExpr),
    MonomorphFuncInstanceCall(CIRMonomorphFuncInstanceCall),
    InterfaceMethodCall(CIRInterfaceMethodCall),
}

#[derive(Debug, Clone)]
pub struct CIRDynamicExpr {
    pub data_expr: Box<CIRExpr>,
    pub method_decls: Vec<CIRFuncDeclStmt>,
    pub global_var_id: IRValueID,
    pub vtable_abi_name: String,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRLambda {
    pub irv_id: IRValueID,
    pub params: CIRFuncParams,
    pub ret: CIRTy,
    pub inline: bool,
    pub body: Box<CIRBlockStmt>,
    pub abi_func_info: ABIFunctionInfo,
    pub loc: Loc,
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
pub struct CIRInterfaceMethodCall {
    pub operand: Box<CIRExpr>,
    pub args: Vec<CIRExpr>,
    pub ret_ty: CIRTy,
    pub func_type: CIRFuncTy,
    pub method_idx: usize,
    pub methods_len: usize,
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
    pub elements: Vec<CIRExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRArrayIndexExpr {
    pub operand: Box<CIRExpr>,
    pub index: Box<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRArrayExpr {
    pub ty: CIRTy,
    pub elements: Vec<CIRExpr>,
    pub loc: Loc,
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
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRVarStmt {
    pub irv_id: IRValueID,
    pub name: String,
    pub ty: CIRTy,
    pub expr: Option<CIRExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRFuncDefStmt {
    pub irv_id: IRValueID,
    pub name: String,
    pub params: CIRFuncParams,
    pub body: Box<CIRBlockStmt>,
    pub ret: CIRTy,
    pub modifiers: FuncModifiers,
    pub abi_func_info: Option<ABIFunctionInfo>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRFuncDeclStmt {
    pub irv_id: IRValueID,
    pub name: String,
    pub params: CIRFuncParams,
    pub ret: CIRTy,
    pub modifiers: FuncModifiers,
    pub abi_func_info: Option<ABIFunctionInfo>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRFuncParam {
    pub name: Option<String>,
    pub irv_id: IRValueID,
    pub ty: CIRTy,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRFuncParams {
    pub list: Vec<CIRFuncParam>,
    pub is_var: bool,
}

#[derive(Debug, Clone)]
pub struct CIRBlockStmt {
    pub stmts: Vec<CIRStmt>,
    pub defers: Vec<CIRStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRIfStmt {
    pub cond: CIRExpr,
    pub then_block: Box<CIRBlockStmt>,
    pub else_block: Option<Box<CIRBlockStmt>>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRForStmt {
    pub initializer: Option<CIRVarStmt>,
    pub cond: Option<CIRExpr>,
    pub increment: Option<CIRExpr>,
    pub body: Box<CIRBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRWhileStmt {
    pub cond: Box<CIRExpr>,
    pub body: Box<CIRBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRSwitchStmt {
    pub value: CIRExpr,
    pub cases: Vec<CIRSwitchCase>,
    pub default: Option<CIRBlockStmt>,
    pub all_cases_covered: bool,
    pub loc: Loc,
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
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRSwitchOnEnumCase {
    pub patterns: Vec<CIRSwitchOnEnumPattern>,
    pub body: CIRBlockStmt,
}

#[derive(Debug, Clone)]
pub enum CIRSwitchOnEnumPattern {
    Ident(String, usize),
    Valued(String, usize, (TypedIdentifier, CIRExpr)),
    ExportFields(String, usize, Vec<(TypedIdentifier, CIRTy)>),
}

impl CIRSwitchOnEnumPattern {
    pub fn variant_idx(&self) -> usize {
        match self {
            CIRSwitchOnEnumPattern::Ident(_, variant_idx) => *variant_idx,
            CIRSwitchOnEnumPattern::ExportFields(_, variant_idx, ..) => *variant_idx,
            CIRSwitchOnEnumPattern::Valued(_, variant_idx, ..) => *variant_idx,
        }
    }

    pub fn variant_name(&self) -> &String {
        match self {
            CIRSwitchOnEnumPattern::Ident(ident, ..) => ident,
            CIRSwitchOnEnumPattern::ExportFields(ident, ..) => ident,
            CIRSwitchOnEnumPattern::Valued(ident, ..) => ident,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CIRReturnStmt {
    pub arg: Option<CIRExpr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRStructStmt {
    pub name: String,
    pub fields: Vec<CIRTy>,
    pub fields_info: Vec<(String, Loc)>,
    pub align: Option<usize>,
    pub modifiers: StructModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIREnumStmt {
    pub name: String,
    pub variants: Vec<CIREnumTyVariant>,
    pub align: Option<usize>,
    pub tag_type: Option<Box<CIRTy>>,
    pub modifiers: EnumModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum CIREnumTyVariant {
    Ident(String),
    Valued(String, Box<CIRExpr>),
    Fielded(String, Vec<CIRTy>),
}

impl PartialEq for CIREnumTyVariant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ident(ident1), Self::Ident(ident2)) => ident1 == ident2,
            (Self::Valued(ident1, expr1), Self::Valued(ident2, expr2)) => ident1 == ident2 && expr1.ty == expr2.ty,
            (Self::Fielded(ident1, fields1), Self::Fielded(ident2, fields2)) => ident1 == ident2 && fields1 == fields2,
            _ => false,
        }
    }
}

impl CIREnumTyVariant {
    pub fn as_fielded(&self) -> Option<&Vec<CIRTy>> {
        match self {
            CIREnumTyVariant::Fielded(_, fields) => Some(fields),
            _ => None,
        }
    }

    pub fn ident(&self) -> &String {
        match self {
            CIREnumTyVariant::Ident(ident) => ident,
            CIREnumTyVariant::Valued(ident, _) => ident,
            CIREnumTyVariant::Fielded(ident, _) => ident,
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
    pub fields_info: Vec<(String, Loc)>,
    pub align: Option<usize>,
    pub modifiers: UnionModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRLabelStmt {
    pub name: String,
    pub label_id: LabelID,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRGotoStmt {
    pub label_id: LabelID,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CIRDeferStmt {
    pub operand: Box<CIRStmt>,
    pub loc: Loc,
}

pub fn cir_expr_as_const_integer_value(expr: &CIRExpr) -> Option<i128> {
    match &expr.kind {
        CIRExprKind::Literal(literal) => match literal.kind {
            CIRLiteralKind::Integer(value, _) => Some(value),
            _ => None,
        },
        _ => None,
    }
}

pub fn cir_func_def_as_decl(func_def: &CIRFuncDefStmt) -> CIRFuncDeclStmt {
    CIRFuncDeclStmt {
        irv_id: func_def.irv_id,
        name: func_def.name.clone(),
        params: func_def.params.clone(),
        ret: func_def.ret.clone(),
        modifiers: func_def.modifiers.clone(),
        abi_func_info: func_def.abi_func_info.clone(),
        loc: func_def.loc,
    }
}

pub fn cir_func_decl_as_func_ty(func_decl: &CIRFuncDeclStmt) -> CIRFuncTy {
    CIRFuncTy {
        params: func_decl.params.list.iter().map(|param| param.ty.clone()).collect(),
        is_var: func_decl.params.is_var,
        ret: Box::new(func_decl.ret.clone()),
        callconv: func_decl.modifiers.callconv.clone().unwrap_or_default(),
        abi_func_info: func_decl.abi_func_info.clone(),
    }
}

pub fn cir_struct_as_struct_ty(struct_stmt: &CIRStructStmt) -> CIRStructTy {
    CIRStructTy {
        name: Some(struct_stmt.name.clone()),
        fields: struct_stmt.fields.clone(),
        fields_info: struct_stmt.fields_info.clone(),
        repr_attr: struct_stmt.modifiers.repr_attr.clone(),
        align: struct_stmt.align.clone(),
        loc: struct_stmt.loc,
    }
}

pub fn cir_enum_as_enum_ty(enum_stmt: &CIREnumStmt) -> CIREnumTy {
    CIREnumTy {
        name: Some(enum_stmt.name.clone()),
        variants: enum_stmt.variants.clone(),
        tag_type: enum_stmt.tag_type.clone(),
        repr_attr: enum_stmt.modifiers.repr_attr.clone(),
        align: enum_stmt.align.clone(),
        loc: enum_stmt.loc,
    }
}

pub fn cir_union_as_union_ty(union_stmt: &CIRUnionStmt) -> CIRUnionTy {
    CIRUnionTy {
        name: Some(union_stmt.name.clone()),
        fields: union_stmt.fields.clone(),
        fields_info: union_stmt.fields_info.clone(),
        repr_attr: union_stmt.modifiers.repr_attr.clone(),
        align: union_stmt.align.clone(),
        loc: union_stmt.loc,
    }
}

impl Debug for CIRProgramTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CIRProgramTree")
            .field("body", &self.body)
            .field("file_path", &self.file_path)
            .finish()
    }
}

impl CIREnumTy {
    pub fn is_repr_c(&self) -> bool {
        if let Some(repr_attr) = &self.repr_attr {
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

impl CIRStmt {
    pub fn loc(&self) -> &Loc {
        match self {
            CIRStmt::Variable(var_stmt) => &var_stmt.loc,
            CIRStmt::GlobalVar(global_var_stmt) => &global_var_stmt.loc,
            CIRStmt::FuncDef(func_def_stmt) => &func_def_stmt.loc,
            CIRStmt::FuncDecl(func_decl_stmt) => &func_decl_stmt.loc,
            CIRStmt::Block(block_stmt) => &block_stmt.loc,
            CIRStmt::Struct(struct_stmt) => &struct_stmt.loc,
            CIRStmt::Enum(enum_stmt) => &enum_stmt.loc,
            CIRStmt::Union(union_stmt) => &union_stmt.loc,
            CIRStmt::Expr(expr) => &expr.loc,
            CIRStmt::If(if_stmt) => &if_stmt.loc,
            CIRStmt::For(for_stmt) => &for_stmt.loc,
            CIRStmt::While(while_stmt) => &while_stmt.loc,
            CIRStmt::Switch(switch_stmt) => &switch_stmt.loc,
            CIRStmt::SwitchOnEnum(switch_on_enum_stmt) => &switch_on_enum_stmt.loc,
            CIRStmt::Return(return_stmt) => &return_stmt.loc,
            CIRStmt::Label(label_stmt) => &label_stmt.loc,
            CIRStmt::Goto(goto_stmt) => &goto_stmt.loc,
            CIRStmt::Defer(defer_stmt) => &defer_stmt.loc,
            CIRStmt::Continue(loc) => loc,
            CIRStmt::Break(loc) => loc,
        }
    }
}
