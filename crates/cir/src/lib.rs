use crate::concrete_type::CIRTy;

pub mod concrete_type;
pub mod walk;

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
    Switch(CIRSwitchStmt),
    Return(CIRReturnStmt),
    Break(CIRBreakStmt),
    Continue(CIRContinueStmt),
    Struct(CIRStructStmt),
    Enum(CIREnumStmt),
    Union(CIRUnionStmt),
    ExportTuple(CIRExportTupleStmt),
    Defer(CIRDeferStmt),
    Expr(CIRExprStmt),
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
    FieldAccess(CIRFieldAccessExpr),
}
