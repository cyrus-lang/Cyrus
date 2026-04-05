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

use crate::decls::{EnumDeclID, InterfaceDeclID, StructDeclID, UnionDeclID};
use crate::exprs::{TypedExprStmt, TypedSelfType};
use crate::stmts::{TypedFuncTypeParams, TypedGenericParam, TypedTypeArgs};
use crate::{SymbolID, VTableID};
use cyrusc_source_loc::Loc;
use cyrusc_tokens::TokenKind;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SemanticType {
    Unresolved(UnresolvedType),
    Named(NamedType),
    Plain(PlainType),
    Array(TypedArrayType),
    Const(Box<SemanticType>),
    Pointer(Box<SemanticType>),
    FuncType(TypedFuncType),
    Tuple(TypedTupleType),
    GenericParam(TypedGenericParam),
    SelfType(TypedSelfType),
    InterfaceType(InterfaceType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnresolvedType {
    Infer,
    Symbol(SymbolID),
    GenericInst { base: SymbolID, type_args: TypedTypeArgs },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamedType {
    pub decl_id: TypeDeclID,
    pub type_args: Option<TypedTypeArgs>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeDeclID {
    Struct(StructDeclID),
    Enum(EnumDeclID),
    Union(UnionDeclID),
    Interface(InterfaceDeclID),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PlainType {
    UIntPtr,
    IntPtr,
    ISize,
    USize,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    UInt,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UInt128,
    Float16,
    Float32,
    Float64,
    Float128,
    Char,
    Bool,
    Void,
    Null,
}

#[derive(Debug, Clone, Eq)]
pub struct TypedTupleType {
    pub elements: Vec<SemanticType>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncType {
    pub symbol_id: Option<SymbolID>,
    pub params: TypedFuncTypeParams,
    pub ret_type: Box<SemanticType>,
    pub is_public: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq)]
pub struct InterfaceType {
    pub interface_decl_id: SymbolID,
    pub vtable_id: VTableID,
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq)]
pub struct TypedArrayType {
    pub element_type: Box<SemanticType>,
    pub capacity: TypedArrayCapacity,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedArrayCapacity {
    Fixed(Box<TypedExprStmt>),
    Dynamic,
}

impl TypeDeclID {
    #[inline]
    pub fn is_struct(&self) -> bool {
        matches!(self, Self::Struct(_))
    }

    #[inline]
    pub fn is_enum(&self) -> bool {
        matches!(self, Self::Enum(_))
    }

    #[inline]
    pub fn is_union(&self) -> bool {
        matches!(self, Self::Union(_))
    }

    #[inline]
    pub fn is_interface(&self) -> bool {
        matches!(self, Self::Interface(_))
    }
}

#[inline]
pub fn map_integer_suffix_to_sema_type(suffix: &TokenKind) -> Option<SemanticType> {
    Some(SemanticType::Plain(match suffix {
        TokenKind::UIntPtr => PlainType::UIntPtr,
        TokenKind::IntPtr => PlainType::IntPtr,
        TokenKind::USize => PlainType::USize,
        TokenKind::ISize => PlainType::ISize,
        TokenKind::Int => PlainType::Int,
        TokenKind::Int8 => PlainType::Int8,
        TokenKind::Int16 => PlainType::Int16,
        TokenKind::Int32 => PlainType::Int32,
        TokenKind::Int64 => PlainType::Int64,
        TokenKind::Int128 => PlainType::Int128,
        TokenKind::UInt => PlainType::UInt,
        TokenKind::UInt8 => PlainType::UInt8,
        TokenKind::UInt16 => PlainType::UInt16,
        TokenKind::UInt32 => PlainType::UInt32,
        TokenKind::UInt64 => PlainType::UInt64,
        TokenKind::UInt128 => PlainType::UInt128,
        _ => return None,
    }))
}

#[inline]
pub fn map_float_suffix_to_sema_type(suffix: &TokenKind) -> Option<SemanticType> {
    Some(SemanticType::Plain(match suffix {
        TokenKind::Float16 => PlainType::Float16,
        TokenKind::Float32 => PlainType::Float32,
        TokenKind::Float64 => PlainType::Float64,
        TokenKind::Float128 => PlainType::Float128,
        _ => return None,
    }))
}

impl UnresolvedType {
    #[inline]
    pub fn as_symbol_id(&self) -> Option<SymbolID> {
        match self {
            UnresolvedType::Infer => None,
            UnresolvedType::GenericInst { .. } => None,
            UnresolvedType::Symbol(symbol_id) => Some(*symbol_id),
        }
    }
}

impl SemanticType {
    #[inline]
    pub fn as_unresolved_symbol_id(&self) -> Option<SymbolID> {
        match &self.const_inner() {
            SemanticType::Unresolved(unresolved_type) => unresolved_type.as_symbol_id(),
            _ => None,
        }
    }

    #[inline]
    pub fn const_inner(&self) -> &SemanticType {
        match self {
            SemanticType::Const(sema_type) => sema_type,
            sema_type @ _ => sema_type,
        }
    }

    #[inline]
    pub fn const_inner_mut(&mut self) -> &mut SemanticType {
        match self {
            SemanticType::Const(sema_type) => sema_type,
            sema_type @ _ => sema_type,
        }
    }

    #[inline]
    pub fn count_const_layers(&self) -> usize {
        match self {
            SemanticType::Const(inner) => 1 + inner.count_const_layers(),
            _ => 0,
        }
    }

    #[inline]
    pub fn pointer_inner(&self) -> &SemanticType {
        match self {
            SemanticType::Pointer(sema_type) => sema_type,
            ty @ _ => ty,
        }
    }

    #[inline]
    pub fn as_const(&self) -> SemanticType {
        if self.is_const() {
            return self.clone();
        }
        SemanticType::Const(Box::new(self.clone()))
    }

    #[inline]
    pub fn as_plain_type(&self) -> Option<&PlainType> {
        match self.const_inner() {
            SemanticType::Plain(ty) => Some(ty),
            _ => None,
        }
    }

    #[inline]
    pub fn as_named_type(&self) -> Option<&NamedType> {
        match self.const_inner() {
            SemanticType::Named(named_type) => Some(named_type),
            _ => None,
        }
    }

    #[inline]
    pub fn as_tuple_type(&self) -> Option<&TypedTupleType> {
        match &self.const_inner() {
            SemanticType::Tuple(tuple_type) => Some(tuple_type),
            _ => None,
        }
    }

    #[inline]
    pub fn as_func_type(&self) -> Option<&TypedFuncType> {
        match &self.const_inner() {
            SemanticType::FuncType(func_type) => Some(func_type),
            _ => None,
        }
    }

    #[inline]
    pub fn as_array_type(&self) -> Option<&TypedArrayType> {
        match self.const_inner() {
            SemanticType::Array(ty) => Some(ty),
            _ => None,
        }
    }

    #[inline]
    pub fn as_self_type(&self) -> Option<&TypedSelfType> {
        match self.const_inner() {
            SemanticType::SelfType(self_type) => Some(self_type),
            _ => None,
        }
    }

    #[inline]
    pub fn as_generic_param(&self) -> Option<&TypedGenericParam> {
        match self.const_inner() {
            SemanticType::GenericParam(generic_param) => Some(generic_param),
            _ => None,
        }
    }
}

impl SemanticType {
    #[inline]
    pub fn is_unresolved(&self) -> bool {
        matches!(self, SemanticType::Unresolved(_))
    }

    #[inline]
    pub fn is_char(&self) -> bool {
        match self.const_inner() {
            SemanticType::Plain(PlainType::Char) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_scalar(&self) -> bool {
        match self.const_inner() {
            SemanticType::Plain(plain_type) => plain_type.is_scalar(),
            _ => false,
        }
    }

    #[inline]
    pub fn is_self_type(&self) -> bool {
        match self.const_inner() {
            SemanticType::SelfType(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_integer(&self) -> bool {
        match self.const_inner() {
            SemanticType::Plain(basic) => basic.is_integer(),
            _ => false,
        }
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        match self.const_inner() {
            SemanticType::Plain(basic) => basic.is_float(),
            _ => false,
        }
    }

    #[inline]
    pub fn is_enum(&self) -> bool {
        match self.const_inner() {
            SemanticType::Named(named_type) => named_type.decl_id.is_enum(),
            _ => false,
        }
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self.const_inner(), SemanticType::Plain(PlainType::Bool))
    }

    #[inline]
    pub fn is_array(&self) -> bool {
        matches!(self.const_inner(), SemanticType::Array(..))
    }

    #[inline]
    pub fn is_func_type(&self) -> bool {
        matches!(self.const_inner(), SemanticType::FuncType(..))
    }

    #[inline]
    pub fn is_const(&self) -> bool {
        matches!(self, SemanticType::Const(_))
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        matches!(self.const_inner(), SemanticType::Plain(PlainType::Void))
    }

    #[inline]
    pub fn is_pointer(&self) -> bool {
        matches!(self.const_inner(), SemanticType::Pointer(..))
    }
}

impl PlainType {
    #[inline]
    pub fn is_scalar(&self) -> bool {
        match self {
            PlainType::UIntPtr | PlainType::IntPtr | PlainType::ISize | PlainType::USize => true,

            PlainType::Int
            | PlainType::Int8
            | PlainType::Int16
            | PlainType::Int32
            | PlainType::Int64
            | PlainType::Int128 => true,

            PlainType::UInt
            | PlainType::UInt8
            | PlainType::UInt16
            | PlainType::UInt32
            | PlainType::UInt64
            | PlainType::UInt128 => true,

            // floating point numbers
            PlainType::Float16 | PlainType::Float32 | PlainType::Float64 | PlainType::Float128 => true,

            // char and bool are scalars
            PlainType::Char | PlainType::Bool => true,

            // void and null are not scalars
            PlainType::Void | PlainType::Null => false,
        }
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        matches!(self, PlainType::Void)
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self, PlainType::Bool)
    }

    #[inline]
    pub fn is_char(&self) -> bool {
        matches!(self, PlainType::Char)
    }

    #[inline]
    pub fn is_integer_or_bool(&self) -> bool {
        self.is_integer() || self.is_bool()
    }

    #[inline]
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            PlainType::UIntPtr
                | PlainType::IntPtr
                | PlainType::ISize
                | PlainType::USize
                | PlainType::Int
                | PlainType::Int8
                | PlainType::Int16
                | PlainType::Int32
                | PlainType::Int64
                | PlainType::Int128
                | PlainType::UInt
                | PlainType::UInt8
                | PlainType::UInt16
                | PlainType::UInt32
                | PlainType::UInt64
                | PlainType::UInt128
        )
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        matches!(
            self,
            PlainType::Float16 | PlainType::Float32 | PlainType::Float64 | PlainType::Float128
        )
    }

    #[inline]
    pub fn is_signed(&self) -> bool {
        match self {
            PlainType::UIntPtr
            | PlainType::UInt
            | PlainType::UInt8
            | PlainType::UInt16
            | PlainType::UInt32
            | PlainType::UInt64
            | PlainType::UInt128
            | PlainType::ISize
            | PlainType::USize
            | PlainType::Bool
            | PlainType::Char
            | PlainType::Void
            | PlainType::Null
            | PlainType::Float16
            | PlainType::Float32
            | PlainType::Float64
            | PlainType::Float128 => false,

            PlainType::IntPtr
            | PlainType::Int
            | PlainType::Int8
            | PlainType::Int16
            | PlainType::Int32
            | PlainType::Int64
            | PlainType::Int128 => true,
        }
    }

    #[inline]
    pub fn plain_type_rank(ty: &PlainType) -> Option<u8> {
        use PlainType::*;

        match ty {
            // char has the same rank is int8
            Char => Some(2),

            // integers
            Int8 | UInt8 => Some(2),
            Int16 | UInt16 => Some(3),
            Int32 | UInt32 => Some(4),
            Int | UInt => Some(5),
            Int64 | UInt64 => Some(6),
            IntPtr | UIntPtr | ISize | USize => Some(7),
            Int128 | UInt128 => Some(8),

            // floats
            Float16 => Some(9),
            Float32 => Some(10),
            Float64 => Some(11),
            Float128 => Some(12),

            _ => None,
        }
    }

    #[inline]
    pub fn widen_type(a: PlainType, b: PlainType) -> Option<PlainType> {
        let a_rank = PlainType::plain_type_rank(&a)?;
        let b_rank = PlainType::plain_type_rank(&b)?;
        if a_rank >= b_rank { Some(a) } else { Some(b) }
    }
}

impl Hash for InterfaceType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.vtable_id.hash(state);
    }
}

impl Hash for TypedArrayType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.element_type.hash(state);
        self.capacity.hash(state);
    }
}

impl Hash for TypedFuncType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.params.hash(state);
        self.ret_type.hash(state);
    }
}

impl Hash for TypedTupleType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.elements.hash(state);
    }
}

impl Hash for TypedArrayCapacity {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl PartialEq for TypedArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.element_type == other.element_type && self.capacity == other.capacity
    }
}

impl PartialEq for InterfaceType {
    fn eq(&self, other: &Self) -> bool {
        self.interface_decl_id == other.interface_decl_id
    }
}

impl PartialEq for TypedFuncType {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.ret_type == other.ret_type
    }
}

impl PartialEq for TypedTupleType {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl TryFrom<TokenKind> for SemanticType {
    type Error = ();

    fn try_from(token_kind: TokenKind) -> Result<Self, Self::Error> {
        let basic_type = match &token_kind {
            TokenKind::ISize => PlainType::ISize,
            TokenKind::USize => PlainType::USize,
            TokenKind::IntPtr => PlainType::IntPtr,
            TokenKind::UIntPtr => PlainType::UIntPtr,
            TokenKind::Int => PlainType::Int,
            TokenKind::Int8 => PlainType::Int8,
            TokenKind::Int16 => PlainType::Int16,
            TokenKind::Int32 => PlainType::Int32,
            TokenKind::Int64 => PlainType::Int64,
            TokenKind::Int128 => PlainType::Int128,
            TokenKind::UInt => PlainType::UInt,
            TokenKind::UInt8 => PlainType::UInt8,
            TokenKind::UInt16 => PlainType::UInt16,
            TokenKind::UInt32 => PlainType::UInt32,
            TokenKind::UInt64 => PlainType::UInt64,
            TokenKind::UInt128 => PlainType::UInt128,
            TokenKind::Float16 => PlainType::Float16,
            TokenKind::Float32 => PlainType::Float32,
            TokenKind::Float64 => PlainType::Float64,
            TokenKind::Float128 => PlainType::Float128,
            TokenKind::Bool => PlainType::Bool,
            TokenKind::Void => PlainType::Void,
            TokenKind::Char => PlainType::Char,
            _ => return Err(()),
        };

        Ok(SemanticType::Plain(basic_type))
    }
}

impl fmt::Display for PlainType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            PlainType::UIntPtr => "uintptr",
            PlainType::IntPtr => "intptr",
            PlainType::ISize => "isize",
            PlainType::USize => "usize",
            PlainType::Int => "int",
            PlainType::Int8 => "int8",
            PlainType::Int16 => "int16",
            PlainType::Int32 => "int32",
            PlainType::Int64 => "int64",
            PlainType::Int128 => "int128",
            PlainType::UInt => "uint",
            PlainType::UInt8 => "uint8",
            PlainType::UInt16 => "uint16",
            PlainType::UInt32 => "uint32",
            PlainType::UInt64 => "uint64",
            PlainType::UInt128 => "uint128",
            PlainType::Float16 => "float16",
            PlainType::Float32 => "float32",
            PlainType::Float64 => "float64",
            PlainType::Float128 => "float128",
            PlainType::Char => "char",
            PlainType::Bool => "bool",
            PlainType::Void => "void",
            PlainType::Null => "null",
        };

        write!(f, "{name}")
    }
}

impl Eq for TypedArrayCapacity {}
impl Eq for TypedFuncType {}
