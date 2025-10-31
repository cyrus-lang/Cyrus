use crate::exprs::{TypedExprStmt, TypedIdentifier};
use crate::stmts::{TypedFuncTypeParams, TypedTypeArgs};
use crate::{ModuleID, SourceLoc, SymbolID};
use cyrusc_ast::{AccessSpecifier, token::TokenKind};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SemanticType {
    UnresolvedSymbol(SymbolID),
    ResolvedSymbol(ResolvedSymbol),
    PlainType(PlainType),
    Array(TypedArrayType),
    Const(Box<SemanticType>),
    Pointer(Box<SemanticType>),
    UnnamedStruct(TypedUStructType),
    FuncType(TypedFuncType),
    Tuple(TypedTupleType),
    GenericType(GenericType),
    GenericParam(TypedIdentifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PlainType {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericType {
    pub base: SymbolID,
    pub type_args: TypedTypeArgs,
    pub is_const: bool,
}

#[derive(Debug, Clone, Eq)]
pub struct TypedTupleType {
    pub type_list: Vec<SemanticType>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncType {
    pub def_module_id: Option<ModuleID>,
    pub params: TypedFuncTypeParams,
    pub return_type: Box<SemanticType>,
    pub vis_opt: Option<AccessSpecifier>,
    pub loc: SourceLoc,
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

impl SemanticType {
    pub fn get_symbol_id(&self) -> Option<SymbolID> {
        match self.get_const_inner() {
            SemanticType::ResolvedSymbol(resolved_symbol) => Some(resolved_symbol.get_symbol_id()),
            _ => None,
        }
    }

    pub fn as_generic_type(&self) -> Option<&GenericType> {
        match self {
            SemanticType::GenericType(generic_type) => Some(generic_type),
            _ => None,
        }
    }

    pub fn as_generic_param(&self) -> Option<&TypedIdentifier> {
        match self {
            SemanticType::GenericParam(typed_identifier) => Some(typed_identifier),
            _ => None,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self.get_const_inner() {
            SemanticType::PlainType(basic) => basic.is_integer(),
            SemanticType::Const(inner) => matches!(&**inner, SemanticType::PlainType(b) if b.is_integer()),
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self.get_const_inner() {
            SemanticType::PlainType(basic) => basic.is_float(),
            SemanticType::Const(inner) => matches!(&**inner, SemanticType::PlainType(b) if b.is_float()),
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        matches!(
            self.get_const_inner(),
            SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(..))
        )
    }

    pub fn is_bool(&self) -> bool {
        matches!(
            self.get_const_inner(),
            SemanticType::PlainType(PlainType::Bool)
        )
    }

    pub fn is_array(&self) -> bool {
        matches!(self.get_const_inner(), SemanticType::Array(..))
    }

    pub fn is_const(&self) -> bool {
        matches!(self.get_const_inner(), SemanticType::Const(_))
    }

    pub fn is_void(&self) -> bool {
        matches!(
            self.get_const_inner(),
            SemanticType::PlainType(PlainType::Void)
        )
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.get_const_inner(), SemanticType::Pointer(..))
    }

    pub fn as_unresolved_symbol(&self) -> Option<SymbolID> {
        match &self {
            SemanticType::UnresolvedSymbol(symbol_id) => Some(*symbol_id),
            _ => None,
        }
    }

    pub fn as_tuple_type(&self) -> Option<&TypedTupleType> {
        match &self.get_const_inner() {
            SemanticType::Tuple(tuple_type) => Some(tuple_type),
            _ => None,
        }
    }

    pub fn as_func_type(&self) -> Option<&TypedFuncType> {
        match &self {
            SemanticType::FuncType(func_type) => Some(func_type),
            _ => None,
        }
    }

    pub fn is_resolved_symbol(&self) -> bool {
        match self {
            SemanticType::ResolvedSymbol(..) => true,
            _ => false,
        }
    }

    pub fn as_rvalue(&self, lvalue: bool) -> SemanticType {
        if lvalue {
            match self {
                SemanticType::Array(typed_array_type) => *typed_array_type.element_type.clone(),
                SemanticType::Pointer(sema_ty) => sema_ty.get_pointer_inner().unwrap_or(*sema_ty.clone()),
                _ => self.clone(),
            }
        } else {
            self.clone()
        }
    }

    pub fn is_char(&self) -> bool {
        match self {
            SemanticType::PlainType(PlainType::Char) => true,
            _ => false,
        }
    }

    pub fn as_basic_type(&self) -> Option<&PlainType> {
        match self {
            SemanticType::PlainType(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn get_const_inner(&self) -> &SemanticType {
        match self {
            SemanticType::Const(sema_ty) => sema_ty,
            sema_ty @ _ => sema_ty,
        }
    }

    pub fn get_pointer_inner(&self) -> Option<SemanticType> {
        match self {
            SemanticType::Pointer(sema_ty) => Some(*sema_ty.clone()),
            _ => None,
        }
    }

    pub fn as_array_type(&self) -> Option<&TypedArrayType> {
        match self {
            SemanticType::Array(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn as_struct_symbol_id(&self) -> Option<SymbolID> {
        match self {
            SemanticType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
                ResolvedSymbol::NamedStruct(symbol_id) => Some(*symbol_id),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_union_symbol_id(&self) -> Option<SymbolID> {
        match self {
            SemanticType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
                ResolvedSymbol::Union(symbol_id) => Some(*symbol_id),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_enum_symbol_id(&self) -> Option<SymbolID> {
        match self {
            SemanticType::ResolvedSymbol(resolved_symbol) => match resolved_symbol {
                ResolvedSymbol::Enum(symbol_id) => Some(*symbol_id),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_unnamed_struct(&self) -> Option<TypedUStructType> {
        match self {
            SemanticType::UnnamedStruct(unnamed_struct_type) => Some(unnamed_struct_type.clone()),
            _ => None,
        }
    }

    pub fn as_const_or_unnamed_struct(&self) -> Option<TypedUStructType> {
        match self {
            SemanticType::UnnamedStruct(unnamed_struct_type) => Some(unnamed_struct_type.clone()),
            SemanticType::Const(inner_concrete_type) => inner_concrete_type.as_unnamed_struct(),
            _ => None,
        }
    }
}

impl PlainType {
    pub fn is_bool(&self) -> bool {
        matches!(self, PlainType::Bool)
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            PlainType::UIntPtr
                | PlainType::IntPtr
                | PlainType::SizeT
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

    pub fn is_float(&self) -> bool {
        matches!(
            self,
            PlainType::Float16
                | PlainType::Float32
                | PlainType::Float64
                | PlainType::Float128
        )
    }

    pub fn is_signed(&self) -> bool {
        match self {
            PlainType::UIntPtr
            | PlainType::UInt
            | PlainType::UInt8
            | PlainType::UInt16
            | PlainType::UInt32
            | PlainType::UInt64
            | PlainType::UInt128
            | PlainType::SizeT
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

    pub fn bigger_type(a: PlainType, b: PlainType) -> Option<PlainType> {
        use PlainType::*;

        fn rank(ty: &PlainType) -> Option<u8> {
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

impl TryFrom<TokenKind> for SemanticType {
    type Error = ();

    fn try_from(token_kind: TokenKind) -> Result<Self, Self::Error> {
        let basic_type = match &token_kind {
            TokenKind::SizeT => PlainType::SizeT,
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

        Ok(SemanticType::PlainType(basic_type))
    }
}

#[derive(Debug, Clone, Eq)]
pub struct TypedArrayType {
    pub element_type: Box<SemanticType>,
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
    Expr(Box<TypedExprStmt>),
    Value(usize),
}

impl TypedArrayFixedCapacityValue {
    pub fn as_value(&self) -> Option<usize> {
        match self {
            TypedArrayFixedCapacityValue::Expr(..) => None,
            TypedArrayFixedCapacityValue::Value(value) => Some(*value),
        }
    }

    pub fn as_expr(&self) -> Option<Box<TypedExprStmt>> {
        match self {
            TypedArrayFixedCapacityValue::Expr(typed_expr) => Some(typed_expr.clone()),
            TypedArrayFixedCapacityValue::Value(..) => None,
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub struct TypedUStructType {
    pub fields: Vec<TypedUnnamedStructTypeField>,
    pub is_packed: bool,
    pub loc: SourceLoc,
}

impl PartialEq for TypedUStructType {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields && self.is_packed == other.is_packed
    }
}

impl Hash for TypedUStructType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.fields.hash(state);
        self.is_packed.hash(state);
    }
}

#[derive(Debug, Clone, Eq)]
pub struct TypedUnnamedStructTypeField {
    pub field_name: String,
    pub field_type: Box<SemanticType>,
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
