use crate::concrete_type::{CIRStructTy, CIRTy};
use ast::{
    AccessSpecifier, StringPrefix,
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
};
use tast::{TypedExprStmt, types::BasicType};

pub mod concrete_type;
pub mod walk;

pub type IRValueId = u32;

#[derive(Debug)]
pub struct CIRModule {
    pub program: CIRProgramTree,
}

#[derive(Debug)]
pub struct CIRProgramTree {
    pub body: Vec<CIRStmt>,
}

#[derive(Debug, Clone)]
pub enum CIRStmt {
    Variable(CIRVarStmt),
    GlobalVar(CIRGlobalVarStmt),
    FuncDef(CIRFuncDefStmt),
    FuncDecl(CIRFuncDeclStmt),
    Block(CIRBlockStmt),
    If(CIRIfStmt),
    For(CIRForStmt),
    While(CIRWhileStmt),
    SwitchInteger(CIRSwitchIntegerStmt),
    SwitchValue(CIRSwitchValueStmt),
    SwitchEnumVariant(CIRSwitchEnumVariantStmt),
    Return(CIRReturnStmt),
    Break(CIRBreakStmt),
    Continue(CIRContinueStmt),
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
    FuncCall(CIRFuncCall)
}

#[derive(Debug, Clone)]
pub struct CIRFuncCall {
    pub operand: Box<CIRExpr>,
    pub args: Vec<CIRExpr>
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
pub enum CIRLiteral {
    Integer(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Null,
    String(String, Option<StringPrefix>),
}

#[derive(Debug, Clone)]
pub struct CIRValueRef {
    pub irv_id: IRValueId,
}

#[derive(Debug, Clone)]
pub struct CIRGlobalVarStmt {
    pub name: String,
    pub ty: CIRTy,
    pub expr: Option<CIRExpr>,
    pub vis: AccessSpecifier,
}

#[derive(Debug, Clone)]
pub struct CIRVarStmt {
    pub name: String,
    pub ty: CIRTy,
    pub expr: Option<CIRExpr>,
}

#[derive(Debug, Clone)]
pub struct CIRFuncDefStmt {
    pub name: String,
    pub params: Vec<CIRTy>,
    pub is_var: bool,
    pub body: Box<CIRBlockStmt>,
    pub ret: CIRTy,
    pub vis: AccessSpecifier,
}

#[derive(Debug, Clone)]
pub struct CIRFuncDeclStmt {
    pub name: String,
    pub params: Vec<CIRTy>,
    pub is_var: bool,
    pub ret: CIRTy,
    pub vis: AccessSpecifier,
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
pub struct CIRSwitchIntegerStmt {} // TODO

#[derive(Debug, Clone)]
pub struct CIRSwitchValueStmt {} // TODO

#[derive(Debug, Clone)]
pub struct CIRSwitchEnumVariantStmt {} // TODO

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
    pub packed: bool,
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
pub struct CIRExportTupleStmt {} // FIXME
