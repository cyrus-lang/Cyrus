// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    abi::args::ABIFunctionInfo,
    cir::{
        cir::CIREnumVariant,
        typectx::{CIRTypeContext, CIRTypeContextDeclKey, CIRTypeContextID},
    },
};
use cyrusc_ast::abi::{CallConv, ReprAttr};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{VTableID, types::PlainType};

#[derive(Debug, Clone, PartialEq)]
pub enum CIRType {
    Struct(CIRTypeContextID),
    Enum(CIRTypeContextID),
    Union(CIRTypeContextID),

    Plain(PlainType),
    Const(Box<CIRType>),
    Pointer(Box<CIRType>),
    FuncType(CIRFuncType),
    Array(CIRArrayType),
    Dynamic(CIRDynamicType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CIRDynamicType {
    pub vtable_id: VTableID,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CIRArrayType {
    pub element_type: Box<CIRType>,
    pub len: usize,
}

#[derive(Debug, Clone)]
pub struct CIRFuncType {
    pub params: Vec<CIRType>,
    pub is_var: bool,
    pub ret_type: Box<CIRType>,
    pub callconv: CallConv,
    pub abi_func_info: Option<ABIFunctionInfo>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CIRStructType {
    pub decl_key: Option<CIRTypeContextDeclKey>,
    pub name: Option<String>,
    pub fields: Vec<CIRType>,
    pub fields_info: Vec<(String, Loc)>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CIRUnionType {
    pub decl_key: Option<CIRTypeContextDeclKey>,
    pub name: Option<String>,
    pub fields: Vec<CIRType>,
    pub fields_info: Vec<(String, Loc)>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CIREnumType {
    pub decl_key: Option<CIRTypeContextDeclKey>,
    pub name: Option<String>,
    pub variants: Vec<CIREnumVariant>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub tag_type: Option<Box<CIRType>>,
    pub loc: Loc,
}

impl CIREnumType {
    #[inline]
    pub fn lookup_variant_tag(&self, variant_name: &str) -> Option<u32> {
        self.lookup_variant(variant_name).map(|variant| variant.tag())
    }

    #[inline]
    pub fn lookup_variant(&self, variant_name: &str) -> Option<&CIREnumVariant> {
        self.variants.iter().find(|variant| variant.ident() == variant_name)
    }

    pub fn tag_type_or_infer_or_default(&self) -> Box<CIRType> {
        self.tag_type
            .clone()
            .or_else(|| {
                if self.includes_only_integer_payload() {
                    self.variant_expr_type()
                } else {
                    None
                }
            })
            .unwrap_or_else(|| Box::new(CIRType::Plain(PlainType::Int32)))
    }

    #[inline]
    pub fn is_scalar_optimizable(&self) -> bool {
        self.is_repr_c()
            || (self.variant_expr_type().is_some() && self.includes_only_integer_payload())
            || !self.includes_payload()
    }

    pub fn variant_expr_type(&self) -> Option<Box<CIRType>> {
        let mut expr_type: Option<Box<CIRType>> = None;

        for variant in &self.variants {
            if let CIREnumVariant::Valued(_, value_type, _) = variant {
                match &expr_type {
                    None => {
                        expr_type = Some(Box::new(value_type.clone()));
                    }

                    Some(existing) => {
                        if **existing != *value_type {
                            // inference failed
                            return None;
                        }
                    }
                }
            }
        }

        expr_type
    }

    #[inline]
    pub fn includes_payload(&self) -> bool {
        self.variants.iter().any(|v| !matches!(v, CIREnumVariant::Unit(_, _)))
    }

    pub fn includes_only_integer_payload(&self) -> bool {
        self.variants.iter().all(|v| match v {
            CIREnumVariant::Valued(_, value_type, _) => value_type.is_integer_or_bool(),
            CIREnumVariant::Unit(_, _) => true,
            CIREnumVariant::Payload(_, _, _) => false,
        })
    }
}

impl CIRType {
    pub fn struct_or_union_fields(&self, tctx: &CIRTypeContext) -> Option<Vec<CIRType>> {
        if let Some(struct_type) = self.as_struct(tctx) {
            Some(struct_type.fields)
        } else if let Some(union_type) = self.as_union(tctx) {
            Some(union_type.fields)
        } else {
            None
        }
    }

    #[inline]
    pub fn as_struct(&self, tctx: &CIRTypeContext) -> Option<CIRStructType> {
        match self {
            CIRType::Struct(type_id) => Some(tctx.get_struct(*type_id)),
            _ => None,
        }
    }

    #[inline]
    pub fn as_union(&self, tctx: &CIRTypeContext) -> Option<CIRUnionType> {
        match self {
            CIRType::Union(type_id) => Some(tctx.get_union(*type_id)),
            _ => None,
        }
    }

    #[inline]
    pub fn as_enum(&self, tctx: &CIRTypeContext) -> Option<CIREnumType> {
        match self {
            CIRType::Enum(type_id) => Some(tctx.get_enum(*type_id)),
            _ => None,
        }
    }

    #[inline]
    pub fn as_func(&self) -> Option<CIRFuncType> {
        match self.const_inner() {
            CIRType::FuncType(fn_ty) => Some(fn_ty.clone()),
            _ => None,
        }
    }

    #[inline]
    pub fn as_plain(&self) -> Option<PlainType> {
        match self.const_inner() {
            CIRType::Plain(plain_type) => Some(plain_type.clone()),
            _ => None,
        }
    }

    #[inline]
    pub fn as_array(&self) -> Option<CIRArrayType> {
        match self.const_inner() {
            CIRType::Array(arr_ty) => Some(arr_ty.clone()),
            _ => None,
        }
    }

    #[inline]
    pub fn as_type_id(&self) -> Option<CIRTypeContextID> {
        match self.const_inner() {
            CIRType::Struct(type_id) | CIRType::Union(type_id) | CIRType::Enum(type_id) => Some(*type_id),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        match self.const_inner() {
            CIRType::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self.const_inner() {
            CIRType::Plain(plain_type) => plain_type.is_void(),
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self.const_inner() {
            CIRType::Plain(PlainType::Bool) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self.const_inner() {
            CIRType::Plain(plain_type) => plain_type.is_float(),
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self.const_inner() {
            CIRType::Plain(plain_type) => plain_type.is_integer(),
            _ => false,
        }
    }

    pub fn is_signed_integer(&self) -> bool {
        match self.const_inner() {
            CIRType::Plain(plain_type) => plain_type.is_integer() && plain_type.is_signed(),
            _ => false,
        }
    }

    pub fn is_integer_or_bool(&self) -> bool {
        self.is_integer() || self.is_bool()
    }

    pub fn is_char(&self) -> bool {
        match self.const_inner() {
            CIRType::Plain(plain_type) => plain_type.is_uint8(),
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match self.const_inner() {
            CIRType::Enum(..) => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self.const_inner() {
            CIRType::Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_union(&self) -> bool {
        match self.const_inner() {
            CIRType::Union(..) => true,
            _ => false,
        }
    }

    pub fn is_func(&self) -> bool {
        match self.const_inner() {
            CIRType::FuncType(_) => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self.const_inner() {
            CIRType::Pointer(_) => true,
            _ => false,
        }
    }

    pub fn pointer_inner(&self) -> Option<&CIRType> {
        match self {
            CIRType::Pointer(inner) => Some(&inner),
            _ => None,
        }
    }

    pub fn const_inner(&self) -> &CIRType {
        match self {
            CIRType::Const(inner) => inner.const_inner(),
            other => other,
        }
    }
}

impl PartialEq for CIRFuncType {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params
            && self.is_var == other.is_var
            && self.ret_type == other.ret_type
            && self.callconv == other.callconv
    }
}

#[macro_export]
macro_rules! is_integer_type {
    ($self:expr, $($patterns:pat_param)|+ $(if $guard:expr)?) => {
        match $self.const_inner() {
            CIRType::Plain(_plain_type) => {
                matches!(_plain_type, $($patterns)|+ $(if $guard)?)
            }
            _ => false,
        }
    };
}

impl CIRStructType {
    #[inline]
    pub fn is_packed(&self) -> bool {
        match &self.repr_attr {
            Some(repr_attr) => repr_attr.is_packed(),
            None => false,
        }
    }
}
