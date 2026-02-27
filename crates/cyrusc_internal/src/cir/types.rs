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

use crate::{abi::args::ABIFunctionInfo, cir::cir::CIREnumTyVariant};
use cyrusc_ast::abi::{CallConv, ReprAttr};
use cyrusc_tast::{types::PlainType, vtable::VTableID};

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
    Dynamic(CIRDynamicTy),
}

#[derive(Debug, Clone)]
pub struct CIRTupleTy {
    pub elements: Vec<CIRTy>,
}

#[derive(Debug, Clone)]
pub struct CIRDynamicTy {
    pub vtable_id: VTableID,
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
    pub callconv: CallConv,
    pub abi_func_info: Option<ABIFunctionInfo>,
}

#[derive(Debug, Clone)]
pub struct CIRStructTy {
    pub fields: Vec<CIRTy>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct CIRUnionTy {
    pub fields: Vec<CIRTy>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct CIREnumTy {
    pub variants: Vec<CIREnumTyVariant>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub discriminant_type: Option<Box<CIRTy>>,
}

impl CIREnumTy {
    #[inline]
    pub fn includes_payload(&self) -> bool {
        self.variants.iter().any(|v| !matches!(v, CIREnumTyVariant::Ident))
    }
}

impl CIRTy {
    pub fn as_tuple(&self) -> Option<CIRTupleTy> {
        match self {
            CIRTy::Tuple(tuple) => Some(tuple.clone()),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<CIRFuncTy> {
        match self.const_inner() {
            CIRTy::FuncType(fn_ty) => Some(fn_ty.clone()),
            _ => None,
        }
    }

    pub fn as_plain(&self) -> Option<PlainType> {
        match self.const_inner() {
            CIRTy::PlainType(plain_type) => Some(plain_type.clone()),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<CIRArrayTy> {
        match self.const_inner() {
            CIRTy::Array(arr_ty) => Some(arr_ty.clone()),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<CIREnumTy> {
        match self.const_inner() {
            CIRTy::Enum(enum_ty) => Some(enum_ty.clone()),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<CIRStructTy> {
        match self.const_inner() {
            CIRTy::Struct(struct_ty) => Some(struct_ty.clone()),
            _ => None,
        }
    }

    pub fn struct_or_union_fields(&self) -> Option<Vec<CIRTy>> {
        match self.const_inner() {
            CIRTy::Struct(struct_ty) => Some(struct_ty.fields.clone()),
            CIRTy::Union(union_ty) => Some(union_ty.fields.clone()),
            _ => None,
        }
    }

    pub fn as_union(&self) -> Option<CIRUnionTy> {
        match self.const_inner() {
            CIRTy::Union(union_ty) => Some(union_ty.clone()),
            _ => None,
        }
    }

    pub fn is_scalar(&self) -> bool {
        match self {
            // plain types like int, float, bool, char, etc.
            CIRTy::PlainType(plain_type) => plain_type.is_scalar(),

            // const does not affect scalar-ness
            CIRTy::Const(inner) => inner.is_scalar(),

            // pointers are scalar regardless of pointee
            CIRTy::Pointer(_) => true,

            // enums are scalar if they lower to an integer
            CIRTy::Enum(_) => true,

            CIRTy::Struct(_) => false,
            CIRTy::Union(_) => false,
            CIRTy::Tuple(_) => false,
            CIRTy::Array(_) => false,
            CIRTy::Dynamic(_) => false,
            CIRTy::FuncType(_) => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            CIRTy::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self.const_inner() {
            CIRTy::PlainType(plain_type) => plain_type.is_void(),
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            CIRTy::PlainType(PlainType::Bool) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self.const_inner() {
            CIRTy::PlainType(plain_type) => plain_type.is_float(),
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self.const_inner() {
            CIRTy::PlainType(plain_type) => plain_type.is_integer(),
            _ => false,
        }
    }

    pub fn is_signed_integer(&self) -> bool {
        match self.const_inner() {
            CIRTy::PlainType(plain_type) => plain_type.is_integer() && plain_type.is_signed(),
            _ => false,
        }
    }

    pub fn is_integer_or_bool(&self) -> bool {
        self.is_integer() || self.is_bool()
    }

    pub fn is_char(&self) -> bool {
        match self.const_inner() {
            CIRTy::PlainType(plain_type) => plain_type.is_char(),
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match self.const_inner() {
            CIRTy::Enum(..) => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            CIRTy::Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_union(&self) -> bool {
        match self.const_inner() {
            CIRTy::Union(..) => true,
            _ => false,
        }
    }

    pub fn is_func(&self) -> bool {
        match self.const_inner() {
            CIRTy::FuncType(_) => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            CIRTy::Pointer(_) => true,
            _ => false,
        }
    }

    pub fn pointer_inner(&self) -> Option<&CIRTy> {
        match self {
            CIRTy::Pointer(inner) => Some(&inner),
            _ => None,
        }
    }

    pub fn const_inner(&self) -> &CIRTy {
        match self {
            CIRTy::Const(inner) => inner.const_inner(),
            other => other,
        }
    }
}

#[macro_export]
macro_rules! is_integer_type {
    ($self:expr, $($patterns:pat_param)|+ $(if $guard:expr)?) => {
        match $self.const_inner() {
            CIRTy::PlainType(_plain_type) => {
                matches!(_plain_type, $($patterns)|+ $(if $guard)?)
            }
            _ => false,
        }
    };
}

impl CIRStructTy {
    #[inline]
    pub fn is_packed(&self) -> bool {
        match &self.repr_attr {
            Some(repr_attr) => repr_attr.is_packed(),
            None => false,
        }
    }
}
