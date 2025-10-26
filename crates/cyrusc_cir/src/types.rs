use crate::CIRExpr;
use cyrusc_tast::types::BasicType;

#[derive(Debug, Clone)]
pub enum CIRTy {
    BasicType(BasicType),
    Const(Box<CIRTy>),
    Pointer(Box<CIRTy>),
    Struct(CIRStructTy),
    Enum(CIREnumTy),
    Union(CIRUnionTy),
    FuncType(CIRFuncTy),
    Tuple(CIRTupleTy),
    Array(CIRArrayTy),
}

#[derive(Debug, Clone)]
pub struct CIRTupleTy {
    pub tys: Vec<CIRTy>,
}

#[derive(Debug, Clone)]
pub struct CIRArrayTy {
    pub ty: Box<CIRTy>,
    pub len: usize,
}

#[derive(Debug, Clone)]
pub struct CIRFuncTy {
    pub params: Vec<CIRTy>,
    pub is_var: bool,
    pub ret: Box<CIRTy>,
}

#[derive(Debug, Clone)]
pub struct CIRStructTy {
    pub tys: Vec<CIRTy>,
}

#[derive(Debug, Clone)]
pub struct CIRUnionTy {
    pub tys: Vec<CIRTy>,
}

#[derive(Debug, Clone)]
pub struct CIREnumTy {
    pub variants: Vec<CIREnumVariantTy>,
}

#[derive(Debug, Clone)]
pub enum CIREnumVariantTy {
    Ident(String),
    Valued(String, Box<CIRExpr>),
    Fielded(String, Vec<CIRTy>),
}
