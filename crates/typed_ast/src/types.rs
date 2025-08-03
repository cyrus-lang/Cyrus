use ast::token::{Location, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteType {
    BasicType(BasicConcreteType),
    Array(TypedArray),
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
pub struct TypedArray {
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
    pub field_name: String,
    pub field_type: Box<ConcreteType>,
    pub loc: Location,
}
