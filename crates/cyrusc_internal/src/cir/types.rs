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
    cir::cir::{CIREnumVariant, cir_expr_as_const_integer_value},
};
use cyrusc_ast::abi::{CallConv, ReprAttr};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{VTableID, types::PlainType};

#[derive(Debug, Clone, PartialEq)]
pub enum CIRType {
    Plain(PlainType),
    Const(Box<CIRType>),
    Pointer(Box<CIRType>),
    Struct(CIRStructType),
    Enum(CIREnumType),
    Union(CIRUnionType),
    FuncType(CIRFuncType),
    Tuple(CIRTupleType),
    Array(CIRArrayType),
    Dynamic(CIRDynamicType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CIRTupleType {
    pub elements: Vec<CIRType>,
    pub loc: Loc,
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
    pub name: Option<String>,
    pub fields: Vec<CIRType>,
    pub fields_info: Vec<(String, Loc)>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CIRUnionType {
    pub name: Option<String>,
    pub fields: Vec<CIRType>,
    pub fields_info: Vec<(String, Loc)>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CIREnumType {
    pub name: Option<String>,
    pub variants: Vec<CIREnumVariant>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub tag_type: Option<Box<CIRType>>,
    pub loc: Loc,
}

impl CIREnumType {
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

    pub fn is_scalar_optimizable(&self) -> bool {
        self.is_repr_c()
            || (self.variant_expr_type().is_some() && self.includes_only_integer_payload())
            || !self.includes_payload()
    }

    pub fn variant_expr_type(&self) -> Option<Box<CIRType>> {
        let mut expr_ty: Option<Box<CIRType>> = None;

        for variant in &self.variants {
            if let CIREnumVariant::Valued(_, expr) = variant {
                let ty = expr.ty.clone();

                match &expr_ty {
                    None => {
                        expr_ty = Some(Box::new(ty));
                    }

                    Some(existing) => {
                        if **existing != ty {
                            // inference failed
                            return None;
                        }
                    }
                }
            }
        }

        expr_ty
    }

    #[inline]
    pub fn includes_payload(&self) -> bool {
        self.variants.iter().any(|v| !matches!(v, CIREnumVariant::Unit(_)))
    }

    pub fn includes_only_integer_payload(&self) -> bool {
        self.variants.iter().all(|v| match v {
            CIREnumVariant::Valued(_, expr) => expr.ty.is_integer_or_bool(),
            CIREnumVariant::Unit(_) => true,
            CIREnumVariant::Payload(_, _) => false,
        })
    }

    pub fn compute_variant_tag(&self, lookup_ident: &String) -> Option<u32> {
        let variant_idx = self
            .variants
            .iter()
            .position(|variant| variant.ident() == lookup_ident)?;

        match &self.variants[variant_idx] {
            CIREnumVariant::Valued(_, expr) => {
                if self.is_scalar_optimizable() {
                    let integer_value = match cir_expr_as_const_integer_value(expr) {
                        Some(value) => value,
                        None => return Some(variant_idx.try_into().unwrap()),
                    };
                    Some(integer_value.try_into().unwrap())
                } else {
                    Some(variant_idx.try_into().unwrap())
                }
            }
            CIREnumVariant::Payload(_, _) => Some(variant_idx.try_into().unwrap()),
            CIREnumVariant::Unit(_) => Some(variant_idx.try_into().unwrap()),
        }
    }
}

impl CIRType {
    pub fn as_tuple(&self) -> Option<CIRTupleType> {
        match self.const_inner() {
            CIRType::Tuple(tuple) => Some(tuple.clone()),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<CIRFuncType> {
        match self.const_inner() {
            CIRType::FuncType(fn_ty) => Some(fn_ty.clone()),
            _ => None,
        }
    }

    pub fn as_plain(&self) -> Option<PlainType> {
        match self.const_inner() {
            CIRType::Plain(plain_type) => Some(plain_type.clone()),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<CIRArrayType> {
        match self.const_inner() {
            CIRType::Array(arr_ty) => Some(arr_ty.clone()),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<CIREnumType> {
        match self.const_inner() {
            CIRType::Enum(enum_type) => Some(enum_type.clone()),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<CIRStructType> {
        match self.const_inner() {
            CIRType::Struct(struct_type) => Some(struct_type.clone()),
            _ => None,
        }
    }

    pub fn struct_or_union_fields(&self) -> Option<Vec<CIRType>> {
        match self.const_inner() {
            CIRType::Struct(struct_type) => Some(struct_type.fields.clone()),
            CIRType::Union(union_ty) => Some(union_ty.fields.clone()),
            _ => None,
        }
        .map(|fields| fields.iter().map(|ty| ty.clone()).collect::<Vec<CIRType>>())
    }

    pub fn as_union(&self) -> Option<CIRUnionType> {
        match self.const_inner() {
            CIRType::Union(union_ty) => Some(union_ty.clone()),
            _ => None,
        }
    }

    pub fn is_scalar(&self) -> bool {
        match self.const_inner() {
            // plain types like int, float, bool, char, etc.
            CIRType::Plain(plain_type) => plain_type.is_scalar(),

            // const does not affect scalar-ness
            CIRType::Const(inner) => inner.is_scalar(),

            // pointers are scalar regardless of pointee
            CIRType::Pointer(_) => true,

            // enums are scalar if they lower to an integer
            CIRType::Enum(_) => true,

            CIRType::Struct(_) => false,
            CIRType::Union(_) => false,
            CIRType::Tuple(_) => false,
            CIRType::Array(_) => false,
            CIRType::Dynamic(_) => false,
            CIRType::FuncType(_) => false,
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
            CIRType::Plain(plain_type) => plain_type.is_char(),
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

impl CIRTupleType {
    pub fn as_struct_ty(&self) -> CIRStructType {
        let fields = self.elements.iter().map(|ty| ty.clone()).collect();

        let fields_info = self
            .elements
            .iter()
            .enumerate()
            .map(|(i, _)| (i.to_string(), self.loc))
            .collect();

        CIRStructType {
            name: None,
            fields,
            fields_info,
            repr_attr: None,
            align: None,
            loc: self.loc,
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
