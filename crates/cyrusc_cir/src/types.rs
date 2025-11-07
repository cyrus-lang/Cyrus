use crate::CIREnumTyVariant;
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
    pub variants: Vec<CIREnumTyVariant>,
}

impl CIRTy {
    pub fn as_tuple(&self) -> Option<CIRTupleTy> {
        match self {
            CIRTy::Tuple(tuple) => Some(tuple.clone()),
            _ => None,
        }
    }

    pub fn as_fn(&self) -> Option<CIRFuncTy> {
        match self {
            CIRTy::FuncType(fn_ty) => Some(fn_ty.clone()),
            CIRTy::Const(inner) => inner.as_fn(),
            _ => None,
        }
    }

    pub fn as_plain(&self) -> Option<PlainType> {
        match self {
            CIRTy::PlainType(plain_type) => Some(plain_type.clone()),
            CIRTy::Const(inner) => inner.as_plain(),
            _ => None,
        }
    }

    pub fn as_arr_ty(&self) -> Option<CIRArrayTy> {
        match self {
            CIRTy::Array(arr_ty) => Some(arr_ty.clone()),
            CIRTy::Const(inner) => inner.as_arr_ty(),
            _ => None,
        }
    }

    pub fn is_enum(&self) -> bool {
        match self {
            CIRTy::Enum(..) => true,
            CIRTy::Const(inner) => inner.is_enum(),
            _ => false,
        }
    }

    pub fn is_fn(&self) -> bool {
        match self {
            CIRTy::FuncType(_) => true,
            CIRTy::Const(inner) => inner.is_fn(),
            _ => false,
        }
    }

    pub fn get_pointer_inner(&self) -> Option<&CIRTy> {
        match self {
            CIRTy::Pointer(inner) => Some(&inner),
            _ => None,
        }
    }
}
