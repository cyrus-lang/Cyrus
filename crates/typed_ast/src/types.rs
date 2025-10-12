use std::hash::{Hash, Hasher};

use crate::{ModuleID, SourceLoc, SymbolID, TypedExpression, TypedFuncTypeParams, TypedIdentifier, TypedTypeArgs};
use ast::{AccessSpecifier, token::TokenKind};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConcreteType {
    UnresolvedSymbol(SymbolID),
    ResolvedSymbol(ResolvedSymbol),
    ResolvedGeneric(ResolvedGeneric),
    BasicType(BasicConcreteType),
    Array(TypedArrayType),
    Const(Box<ConcreteType>),
    Pointer(Box<ConcreteType>),
    UnnamedStruct(TypedUnnamedStructType),
    FuncType(TypedFuncType),
    Tuple(TypedTupleType),
    GenericParam(TypedIdentifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedGeneric {
    pub base: SymbolID,
    pub type_args: TypedTypeArgs,
    pub is_const: bool,
}

#[derive(Debug, Clone, Eq)]
pub struct TypedTupleType {
    pub type_list: Vec<ConcreteType>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncType {
    pub def_module_id: Option<ModuleID>,
    pub params: TypedFuncTypeParams,
    pub return_type: Box<ConcreteType>,
    pub vis_opt: Option<AccessSpecifier>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedSymbol {
    Enum(SymbolID),
    Union(SymbolID),
    Typedef(SymbolID),
    NamedStruct(SymbolID),
    Interface(SymbolID),
    GlobalVar(SymbolID),
    Variable(SymbolID),
    Func(SymbolID),
    Method(SymbolID),
}

impl ResolvedSymbol {
    pub fn get_symbol_id(&self) -> SymbolID {
        match self {
            ResolvedSymbol::Union(symbol_id) => *symbol_id,
            ResolvedSymbol::Enum(symbol_id) => *symbol_id,
            ResolvedSymbol::Typedef(symbol_id) => *symbol_id,
            ResolvedSymbol::NamedStruct(symbol_id) => *symbol_id,
            ResolvedSymbol::Interface(symbol_id) => *symbol_id,
            ResolvedSymbol::GlobalVar(symbol_id) => *symbol_id,
            ResolvedSymbol::Variable(symbol_id) => *symbol_id,
            ResolvedSymbol::Func(symbol_id) => *symbol_id,
            ResolvedSymbol::Method(symbol_id) => *symbol_id,
        }
    }
}

impl ConcreteType {
    pub fn is_enum(&self) -> bool {
        matches!(self, ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(..)))
    }

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

    pub fn is_pointer(&self) -> bool {
        matches!(self, ConcreteType::Pointer(..))
    }

    pub fn as_unresolved_symbol(&self) -> Option<SymbolID> {
        match &self {
            ConcreteType::UnresolvedSymbol(symbol_id) => Some(*symbol_id),
            _ => None,
        }
    }

    pub fn as_tuple_type(&self) -> Option<&TypedTupleType> {
        match &self {
            ConcreteType::Tuple(tuple_type) => Some(tuple_type),
            _ => None,
        }
    }

    pub fn as_func_type(&self) -> Option<&TypedFuncType> {
        match &self {
            ConcreteType::FuncType(func_type) => Some(func_type),
            _ => None,
        }
    }

    pub fn is_resolved_symbol(&self) -> bool {
        match self {
            ConcreteType::ResolvedSymbol(..) => true,
            _ => false,
        }
    }

    pub fn as_rvalue(&self, lvalue: bool) -> Self {
        if lvalue {
            match self {
                ConcreteType::Array(typed_array_type) => *typed_array_type.element_type.clone(),
                ConcreteType::Pointer(concrete_type) => concrete_type.get_pointer_inner().unwrap(),
                _ => self.clone(),
            }
        } else {
            self.clone()
        }
    }

    pub fn is_char(&self) -> bool {
        match self {
            ConcreteType::BasicType(BasicConcreteType::Char) => true,
            _ => false,
        }
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
            _ => None,
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

    pub fn as_union_symbol_id(&self) -> Option<SymbolID> {
        match self {
            ConcreteType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
                ResolvedSymbol::Union(symbol_id) => Some(*symbol_id),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_enum_symbol_id(&self) -> Option<SymbolID> {
        match self {
            ConcreteType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
                ResolvedSymbol::Enum(symbol_id) => Some(*symbol_id),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_unnamed_struct(&self) -> Option<TypedUnnamedStructType> {
        match self {
            ConcreteType::UnnamedStruct(unnamed_struct_type) => Some(unnamed_struct_type.clone()),
            _ => None,
        }
    }

    pub fn as_const_or_unnamed_struct(&self) -> Option<TypedUnnamedStructType> {
        match self {
            ConcreteType::UnnamedStruct(unnamed_struct_type) => Some(unnamed_struct_type.clone()),
            ConcreteType::Const(inner_concrete_type) => inner_concrete_type.as_unnamed_struct(),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Eq)]
pub struct TypedArrayType {
    pub element_type: Box<ConcreteType>,
    pub capacity: TypedArrayCapacity,
    pub loc: SourceLoc,
}

impl PartialEq for TypedArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.element_type == other.element_type && self.capacity == other.capacity
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypedArrayCapacity {
    Fixed(TypedArrayFixedCapacityValue),
    Dynamic,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedArrayFixedCapacityValue {
    Expr(Box<TypedExpression>),
    Value(usize),
}

impl TypedArrayFixedCapacityValue {
    pub fn as_value(&self) -> Option<usize> {
        match self {
            TypedArrayFixedCapacityValue::Expr(..) => None,
            TypedArrayFixedCapacityValue::Value(value) => Some(*value),
        }
    }

    pub fn as_expr(&self) -> Option<Box<TypedExpression>> {
        match self {
            TypedArrayFixedCapacityValue::Expr(typed_expr) => Some(typed_expr.clone()),
            TypedArrayFixedCapacityValue::Value(..) => None,
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub struct TypedUnnamedStructType {
    pub fields: Vec<TypedUnnamedStructTypeField>,
    pub packed: bool,
    pub loc: SourceLoc,
}

impl PartialEq for TypedUnnamedStructType {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields && self.packed == other.packed
    }
}

impl Hash for TypedUnnamedStructType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.fields.hash(state);
        self.packed.hash(state);
    }
}

#[derive(Debug, Clone, Eq)]
pub struct TypedUnnamedStructTypeField {
    pub field_name: String,
    pub field_type: Box<ConcreteType>,
    pub loc: SourceLoc,
}

impl Hash for TypedUnnamedStructTypeField {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.field_name.hash(state);
        self.field_type.hash(state);
    }
}

impl PartialEq for TypedUnnamedStructTypeField {
    fn eq(&self, other: &Self) -> bool {
        self.field_name == other.field_name && self.field_type == other.field_type
    }
}

impl PartialEq for TypedFuncType {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.return_type == other.return_type
    }
}

impl PartialEq for TypedTupleType {
    fn eq(&self, other: &Self) -> bool {
        self.type_list == other.type_list
    }
}

impl Hash for TypedArrayFixedCapacityValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            TypedArrayFixedCapacityValue::Value(v) => v.hash(state),
            TypedArrayFixedCapacityValue::Expr(_) => {
                panic!("Requires a compile-time constant array size.");
            }
        }
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
        self.return_type.hash(state);
    }
}

impl Hash for TypedTupleType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.type_list.hash(state);
    }
}

impl Eq for TypedArrayFixedCapacityValue {}
impl Eq for TypedFuncType {}
