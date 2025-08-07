use crate::SymbolID;
use ast::{
    Identifier,
    token::{Location, TokenKind},
};

#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteType {
    Symbol(SymbolID),
    BasicType(BasicConcreteType),
    Array(TypedArrayType),
    Const(Box<ConcreteType>),
    Pointer(Box<ConcreteType>),
    UnnamedStruct(TypedUnnamedStructType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BasicConcreteType {
    UIntPtr,
    IntPtr,
    SizeT,
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

impl BasicConcreteType {
    pub fn bigger_type(a: BasicConcreteType, b: BasicConcreteType) -> Option<BasicConcreteType> {
        use BasicConcreteType::*;

        fn rank(ty: &BasicConcreteType) -> Option<u8> {
            match ty {
                Int8 | UInt8 => Some(2),
                Int16 | UInt16 => Some(3),
                Int32 | UInt32 => Some(4),
                Int | UInt => Some(5),
                Int64 | UInt64 => Some(6),
                IntPtr | UIntPtr | SizeT => Some(7),
                Int128 | UInt128 => Some(8),

                Float16 => Some(9),
                Float32 => Some(10),
                Float64 => Some(11),
                Float128 => Some(12),

                _ => None,
            }
        }

        let a_rank = rank(&a)?;
        let b_rank = rank(&b)?;
        if a_rank >= b_rank { Some(a) } else { Some(b) }
    }
}

impl From<TokenKind> for ConcreteType {
    fn from(token_kind: TokenKind) -> Self {
        ConcreteType::BasicType(match &token_kind {
            TokenKind::SizeT => BasicConcreteType::SizeT,
            TokenKind::IntPtr => BasicConcreteType::IntPtr,
            TokenKind::UIntPtr => BasicConcreteType::UIntPtr,
            TokenKind::Int => BasicConcreteType::Int,
            TokenKind::Int8 => BasicConcreteType::Int8,
            TokenKind::Int16 => BasicConcreteType::Int16,
            TokenKind::Int32 => BasicConcreteType::Int32,
            TokenKind::Int64 => BasicConcreteType::Int64,
            TokenKind::Int128 => BasicConcreteType::Int128,
            TokenKind::UInt => BasicConcreteType::UInt,
            TokenKind::UInt8 => BasicConcreteType::UInt8,
            TokenKind::UInt16 => BasicConcreteType::UInt16,
            TokenKind::UInt32 => BasicConcreteType::UInt32,
            TokenKind::UInt64 => BasicConcreteType::UInt64,
            TokenKind::UInt128 => BasicConcreteType::UInt128,
            TokenKind::Float16 => BasicConcreteType::Float16,
            TokenKind::Float32 => BasicConcreteType::Float32,
            TokenKind::Float64 => BasicConcreteType::Float64,
            TokenKind::Float128 => BasicConcreteType::Float128,
            TokenKind::Bool => BasicConcreteType::Bool,
            TokenKind::Void => BasicConcreteType::Void,
            TokenKind::Char => BasicConcreteType::Char,
            _ => panic!("Unexpected token kind"),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArrayType {
    pub element_type: Box<ConcreteType>,
    pub capacity: TypedArrayCapacity,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedArrayCapacity {
    Fixed(u32),
    Dynamic,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnnamedStructType {
    pub fields: Vec<TypedUnnamedStructTypeField>,
    pub packed: bool,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnnamedStructTypeField {
    pub field_name: Identifier,
    pub field_type: Box<ConcreteType>,
    pub loc: Location,
}
