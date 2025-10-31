use crate::{CIREnumVariant, CIRExpr};
use cyrusc_tast::types::PlainType;

#[derive(Debug, Clone)]
pub enum CIRTy {
    PlainType(PlainType),
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
    pub items: Vec<CIRTy>,
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
    pub fields: Vec<CIRTy>,
    pub is_packed: bool,
}

#[derive(Debug, Clone)]
pub struct CIRUnionTy {
    pub fields: Vec<CIRTy>,
}

#[derive(Debug, Clone)]
pub struct CIREnumTy {
    pub variants: Vec<CIREnumVariant>,
}
