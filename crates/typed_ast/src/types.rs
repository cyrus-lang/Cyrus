use crate::SymbolID;
use ast::token::{Location, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteType {
    UnresolvedSymbol(SymbolID),
    ResolvedSymbol(ResolvedSymbol),
    BasicType(BasicConcreteType),
    Array(TypedArrayType),
    Const(Box<ConcreteType>),
    Pointer(Box<ConcreteType>),
    UnnamedStruct(TypedUnnamedStructType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedSymbol {
    Enum(SymbolID),
    Typedef(SymbolID),
    NamedStruct(SymbolID),
    Interface(SymbolID),
    GlobalVar(SymbolID),
    Variable(SymbolID),
    Func(SymbolID),
    Method(SymbolID),
}

impl ConcreteType {
    pub fn is_bool(&self) -> bool {
        matches!(self, ConcreteType::BasicType(BasicConcreteType::Bool))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, ConcreteType::Array(..))
    }

    pub fn is_const(&self) -> bool {
        matches!(self, ConcreteType::Const(_))
    }

    pub fn is_void(&self) -> bool {
        matches!(self, ConcreteType::BasicType(BasicConcreteType::Void))
    }

    pub fn as_basic_type(&self) -> Option<&BasicConcreteType> {
        match self {
            ConcreteType::BasicType(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn get_const_inner(&self) -> &ConcreteType {
        match self {
            ConcreteType::UnresolvedSymbol(_) => unreachable!(),
            ConcreteType::Const(concrete_type) => concrete_type,
            concrete_type @ _ => concrete_type,
        }
    }

    pub fn get_pointer_inner(&self) -> Option<ConcreteType> {
        match self {
            ConcreteType::Pointer(concrete_type) => Some(*concrete_type.clone()),
            _ => None
        }
    }

    pub fn as_array_type(&self) -> Option<&TypedArrayType> {
        match self {
            ConcreteType::Array(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn as_struct_symbol_id(&self) -> Option<SymbolID> {
        match self {
            ConcreteType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
                ResolvedSymbol::NamedStruct(symbol_id) => Some(*symbol_id),
                _ => None,
            },
            _ => None,
        }
    }
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
    pub fn is_bool(&self) -> bool {
        matches!(self, BasicConcreteType::Bool)
    }

    pub fn is_float(&self) -> bool {
        match self {
            BasicConcreteType::Float16
            | BasicConcreteType::Float32
            | BasicConcreteType::Float64
            | BasicConcreteType::Float128 => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            BasicConcreteType::UIntPtr
            | BasicConcreteType::UInt
            | BasicConcreteType::UInt8
            | BasicConcreteType::UInt16
            | BasicConcreteType::UInt32
            | BasicConcreteType::UInt64
            | BasicConcreteType::UInt128
            | BasicConcreteType::SizeT
            | BasicConcreteType::IntPtr
            | BasicConcreteType::Int
            | BasicConcreteType::Int8
            | BasicConcreteType::Int16
            | BasicConcreteType::Int32
            | BasicConcreteType::Int64
            | BasicConcreteType::Int128 => true,
            _ => false,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            BasicConcreteType::UIntPtr
            | BasicConcreteType::UInt
            | BasicConcreteType::UInt8
            | BasicConcreteType::UInt16
            | BasicConcreteType::UInt32
            | BasicConcreteType::UInt64
            | BasicConcreteType::UInt128
            | BasicConcreteType::SizeT
            | BasicConcreteType::Bool
            | BasicConcreteType::Char
            | BasicConcreteType::Void
            | BasicConcreteType::Null
            | BasicConcreteType::Float16
            | BasicConcreteType::Float32
            | BasicConcreteType::Float64
            | BasicConcreteType::Float128 => false,

            BasicConcreteType::IntPtr
            | BasicConcreteType::Int
            | BasicConcreteType::Int8
            | BasicConcreteType::Int16
            | BasicConcreteType::Int32
            | BasicConcreteType::Int64
            | BasicConcreteType::Int128 => true,
        }
    }

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

impl TryFrom<TokenKind> for ConcreteType {
    type Error = ();

    fn try_from(token_kind: TokenKind) -> Result<Self, Self::Error> {
        let basic_type = match &token_kind {
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
            _ => return Err(()),
        };

        Ok(ConcreteType::BasicType(basic_type))
    }
}

#[derive(Debug, Clone)]
pub struct TypedArrayType {
    pub element_type: Box<ConcreteType>,
    pub capacity: TypedArrayCapacity,
    pub loc: Location,
}

impl PartialEq for TypedArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.element_type == other.element_type && self.capacity == other.capacity
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedArrayCapacity {
    Fixed(u32),
    Dynamic,
}

#[derive(Debug, Clone)]
pub struct TypedUnnamedStructType {
    pub fields: Vec<TypedUnnamedStructTypeField>,
    pub packed: bool,
    pub loc: Location,
}

impl PartialEq for TypedUnnamedStructType {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields && self.packed == other.packed
    }
}

#[derive(Debug, Clone)]
pub struct TypedUnnamedStructTypeField {
    pub field_name: String,
    pub field_type: Box<ConcreteType>,
    pub loc: Location,
}

impl PartialEq for TypedUnnamedStructTypeField {
    fn eq(&self, other: &Self) -> bool {
        self.field_name == other.field_name && self.field_type == other.field_type
    }
}
