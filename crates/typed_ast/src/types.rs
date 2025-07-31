use ast::token::Location;

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
