use crate::types::{CIREnumTy, CIRFuncTy, CIRStructTy, CIRTy, CIRUnionTy};
use cyrusc_ast::{
    AccessSpecifier, StringPrefix,
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
};
use cyrusc_tast::sigs::StructSig;

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
    Switch(CIRSwitchStmt),
    Return(CIRReturnStmt),
    Struct(CIRStructStmt),
    Enum(CIREnumStmt),
    Union(CIRUnionStmt),
    ExportTuple(CIRExportTupleStmt),
    Expr(CIRExpr),
}

#[derive(Debug, Clone)]
pub struct CIRExpr {
    pub kind: CIRExprKind,
    pub ty: CIRTy,
}

#[derive(Debug, Clone)]
pub enum CIRExprKind {
    Load(CIRValueRef),
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
    StructFieldAccess(CIRStructFieldAccessExpr),
    UnionFieldAccess(CIRUnionFieldAccessExpr),
    Lambda(CIRLambda),
    FuncCall(CIRFuncCall),
}

#[derive(Debug, Clone)]
pub struct CIRLambda {
    pub irv_id: IRValueID,
    pub params: CIRFuncParams,
    pub ret: CIRTy,
    pub body: Box<CIRBlockStmt>,
}

#[derive(Debug, Clone)]
pub struct CIRFuncCall {
    pub operand: Box<CIRExpr>,
    pub args: Vec<CIRExpr>,
    pub ret_ty: CIRTy,
}

#[derive(Debug, Clone)]
pub struct CIRUnionFieldAccessExpr {
    pub operand: Box<CIRExpr>,
    pub field_idx: usize,
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
    pub operand: Box<CIRExpr>,
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
    Integer(i64, bool),
    Float(f64),
    Bool(bool),
    Char(char),
    Null,
    CString(String),
    ByteString(String),
}

#[derive(Debug, Clone)]
pub struct CIRValueRef {
    pub irv_id: IRValueID,
}

#[derive(Debug, Clone)]
pub struct CIRGlobalVarStmt {
    pub irv_id: IRValueID,
    pub name: String,
    pub ty: CIRTy,
    pub expr: Option<CIRExpr>,
    pub vis: AccessSpecifier,
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
    pub vis: AccessSpecifier,
}

#[derive(Debug, Clone)]
pub struct CIRFuncDeclStmt {
    pub irv_id: IRValueID,
    pub name: String,
    pub params: CIRFuncParams,
    pub ret: CIRTy,
    pub vis: AccessSpecifier,
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
    pub branches: Vec<CIRIfStmt>,
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
    kind: CIRSwitchKind,
    value: CIRExpr,
    cases: Vec<(CIRBlockID, Box<CIRBlockStmt>)>,
    default: CIRBlockID,
}

#[derive(Debug, Clone)]
pub enum CIRSwitchKind {
    Plain, // integer, scalar
    Enum,
}

#[derive(Debug, Clone)]
pub struct CIRContinueStmt;

#[derive(Debug, Clone)]
pub struct CIRBreakStmt;

#[derive(Debug, Clone)]
pub struct CIRReturnStmt {
    pub arg: Option<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRStructStmt {
    pub name: String,
    pub fields: Vec<CIRTy>,
    pub is_packed: bool,
    pub vis: AccessSpecifier,
}

#[derive(Debug, Clone)]
pub struct CIREnumStmt {
    pub name: String,
    pub variants: Vec<CIREnumVariant>,
    pub vis: AccessSpecifier,
}

#[derive(Debug, Clone)]
pub enum CIREnumVariant {
    Ident,
    Valued(Box<CIRExpr>),
    Fielded(Vec<CIRTy>),
}

#[derive(Debug, Clone)]
pub struct CIRUnionStmt {
    pub name: String,
    pub fields: Vec<CIRTy>,
    pub vis: AccessSpecifier,
}

#[derive(Debug, Clone)]
pub struct CIRExportTupleStmt {}

pub fn cir_func_def_as_decl(func_def: &CIRFuncDefStmt) -> CIRFuncDeclStmt {
    CIRFuncDeclStmt {
        irv_id: func_def.irv_id,
        name: func_def.name.clone(),
        params: func_def.params.clone(),
        ret: func_def.ret.clone(),
        vis: func_def.vis.clone(),
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
