// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                                                         │
// │  Cyrus Programming Language                                             │
// │  https://github.com/cyrus-lang/Cyrus                                    │
// │                                                                         │
// │  A general-purpose, statically-typed, manually memory-managed           │
// │  programming language designed for performance-critical applications.   │
// │                                                                         │
// │  Copyright (c) 2026 The Cyrus Programming Language Project              │
// │                                                                         │
// │  This program is free software: you can redistribute it and/or modify   │
// │  it under the terms of the GNU General Public License as published by   │
// │  the Free Software Foundation, either version 3 of the License, or      │
// │  (at your option) any later version.                                    │
// │                                                                         │
// │  This program is distributed in the hope that it will be useful,        │
// │  but WITHOUT ANY WARRANTY; without even the implied warranty of         │
// │  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the           │
// │  GNU General Public License for more details.                           │
// │                                                                         │
// │  You should have received a copy of the GNU General Public License      │
// │  along with this program. If not, see <https://www.gnu.org/licenses/>.  │
// │                                                                         │
// └─────────────────────────────────────────────────────────────────────────┘

/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
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
    pub fn is_bool(&self) -> bool {
        match self {
            CIRTy::PlainType(PlainType::Bool) => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self.get_const_inner() {
            CIRTy::PlainType(plain_type) => plain_type.is_integer(),
            _ => false,
        }
    }

    pub fn as_tuple(&self) -> Option<CIRTupleTy> {
        match self {
            CIRTy::Tuple(tuple) => Some(tuple.clone()),
            _ => None,
        }
    }

    pub fn as_fn(&self) -> Option<CIRFuncTy> {
        match self.get_const_inner() {
            CIRTy::FuncType(fn_ty) => Some(fn_ty.clone()),
            _ => None,
        }
    }

    pub fn as_plain(&self) -> Option<PlainType> {
        match self.get_const_inner() {
            CIRTy::PlainType(plain_type) => Some(plain_type.clone()),
            _ => None,
        }
    }

    pub fn as_arr_ty(&self) -> Option<CIRArrayTy> {
        match self.get_const_inner() {
            CIRTy::Array(arr_ty) => Some(arr_ty.clone()),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<CIREnumTy> {
        match self.get_const_inner() {
            CIRTy::Enum(enum_ty) => Some(enum_ty.clone()),
            _ => None,
        }
    }

    pub fn is_enum(&self) -> bool {
        match self.get_const_inner() {
            CIRTy::Enum(..) => true,
            _ => false,
        }
    }

    pub fn is_union(&self) -> bool {
        match self.get_const_inner() {
            CIRTy::Union(..) => true,
            _ => false,
        }
    }

    pub fn is_fn(&self) -> bool {
        match self.get_const_inner() {
            CIRTy::FuncType(_) => true,
            _ => false,
        }
    }

    pub fn get_pointer_inner(&self) -> Option<&CIRTy> {
        match self {
            CIRTy::Pointer(inner) => Some(&inner),
            _ => None,
        }
    }

    pub fn get_const_inner(&self) -> &CIRTy {
        match self {
            CIRTy::Const(inner) => inner.get_const_inner(),
            other => other,
        }
    }
}
