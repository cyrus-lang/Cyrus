use std::collections::HashMap;

use crate::types::ConcreteType;
use ast::{
    AccessSpecifier, Identifier, Literal, SelfModifier,
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
    token::Location,
};

pub mod format;
pub mod types;

pub type ScopeID = u32;
pub type SymbolID = u32;
pub type ModuleID = u64;

#[derive(Debug)]
pub enum TypedProgram {
    ProgramTree(TypedProgramTree),
    Statement(TypedStatement),
    Expression(TypedExpression),
}

#[derive(Debug)]
pub struct TypedProgramTree {
    pub body: Vec<TypedStatement>,
}

// Expressions

#[derive(Debug, Clone)]
pub enum TypedExpression {
    Symbol(SymbolID),
    Literal(Literal),
    Prefix(TypedPrefixExpression),
    Infix(TypedInfixExpression),
    Unary(TypedUnaryExpression),
    Assignment(TypedAssignment),
    Cast(TypedCast),
    Array(TypedArray),
    ArrayIndex(TypedArrayIndex),
    AddressOf(TypedAddressOf),
    Dereference(TypedDereference),
    StructInit(TypedStructInit),
    FuncCall(TypedFuncCall),
    FieldAccess(TypedFieldAccess),
    MethodCall(TypedMethodCall),
    UnnamedStructValue(TypedUnnamedStructValue),
}

impl TypedExpression {
    pub fn is_lvalue(&self) -> bool {
        match self {
            TypedExpression::Symbol(_) => true,
            TypedExpression::ArrayIndex(_) => true,
            TypedExpression::Dereference(_) => true,
            TypedExpression::FieldAccess(_) => true,
            TypedExpression::MethodCall(_) => false,
            TypedExpression::FuncCall(_) => false,
            TypedExpression::StructInit(_) => false,
            TypedExpression::UnnamedStructValue(_) => false,
            TypedExpression::Literal(_) => false,
            TypedExpression::Prefix(_) => false,
            TypedExpression::Infix(_) => false,
            TypedExpression::Unary(_) => false,
            TypedExpression::Assignment(_) => false,
            TypedExpression::Cast(_) => false,
            TypedExpression::AddressOf(_) => false,
            TypedExpression::Array(_) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedIdentifier {
    pub name: String,
    pub symbol_id: SymbolID,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedPrefixExpression {
    pub op: PrefixOperator,
    pub operand: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedUnaryExpression {
    pub operand: Box<TypedExpression>,
    pub op: UnaryOperator,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedInfixExpression {
    pub op: InfixOperator,
    pub lhs: Box<TypedExpression>,
    pub rhs: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedAssignment {
    pub lhs: Box<TypedExpression>,
    pub rhs: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedCast {
    pub operand: Box<TypedExpression>,
    pub target_type: ConcreteType,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedArray {
    pub array_type: ConcreteType,
    pub elements: Vec<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedArrayIndex {
    pub operand: Box<TypedExpression>,
    pub index: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedAddressOf {
    pub operand: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedDereference {
    pub operand: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedStructInit {
    pub symbol_id: SymbolID,
    pub fields: Vec<TypedStructFieldInit>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedStructFieldInit {
    pub name: String,
    pub value: TypedExpression,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedFuncCall {
    pub symbol_id: SymbolID,
    pub args: Vec<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedFieldAccess {
    pub symbol_id: SymbolID,
    pub field_name: String,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedMethodCall {
    pub symbol_id: SymbolID,
    pub method_name: String,
    pub args: Vec<TypedExpression>,
    pub is_fat_arrow: bool,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedUnnamedStructValue {
    pub fields: Vec<TypedUnnamedStructValueField>,
    pub packed: bool,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedUnnamedStructValueField {
    pub field_name: String,
    pub field_type: Option<ConcreteType>,
    pub field_value: Box<TypedExpression>,
    pub loc: Location,
}

// Statements

#[derive(Debug, Clone)]
pub enum TypedStatement {
    Import(TypedImport),
    Variable(TypedVariable),
    Typedef(TypedTypedef),
    GlobalVariable(TypedGlobalVariable),
    FuncDef(TypedFuncDef),
    FuncDecl(TypedFuncDecl),
    BlockStatement(TypedBlockStatement),
    If(TypedIf),
    Return(TypedReturn),
    Break(TypedBreak),
    Continue(TypedContinue),
    For(TypedFor),
    Foreach(TypedForeach),
    Switch(TypedSwitch),
    Struct(TypedStruct),
    Enum(TypedEnum),
    Interface(TypedInterface),
    Expression(TypedExpression),
}

#[derive(Debug, Clone)]
pub struct TypedInterface {
    pub symbol_id: SymbolID,
    pub methods: Vec<TypedFuncDecl>,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedEnum {
    pub name: String,
    pub variants: Vec<TypedEnumVariant>,
    pub methods: HashMap<String, SymbolID>,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum TypedEnumVariant {
    Identifier(Identifier),
    Valued(Identifier, Box<TypedExpression>),
    Variant(Identifier, Vec<TypedEnumValuedField>),
}

#[derive(Debug, Clone)]
pub struct TypedEnumValuedField {
    pub name: String,
    pub field_type: ConcreteType,
    pub loc: Location
}

#[derive(Debug, Clone)]
pub struct TypedStruct {
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub methods: HashMap<String, SymbolID>,
    pub vis: AccessSpecifier,
    pub packed: bool,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedStructField {
    pub name: String,
    pub ty: ConcreteType,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedReturn {
    pub argument: Option<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedGlobalVariable {
    pub name: String,
    pub ty: Option<ConcreteType>,
    pub expr: Option<TypedExpression>,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedTypedef {
    pub name: String,
    pub ty: ConcreteType,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedImport {
    pub resolved_path: Vec<String>,
    pub alias: Option<String>,
    pub module_id: ModuleID,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedBlockStatement {
    pub scope_id: ScopeID,
    pub exprs: Vec<TypedStatement>,
    pub loc: Location,
}

impl TypedBlockStatement {
    pub fn new_empty(scope_id: ScopeID) -> Self {
        Self {
            scope_id,
            exprs: Vec::new(),
            loc: Location::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedVariable {
    pub name: String,
    pub ty: Option<ConcreteType>,
    pub rhs: Option<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedIf {
    pub condition: TypedExpression,
    pub consequent: Box<TypedBlockStatement>,
    pub branches: Vec<TypedIf>,
    pub alternate: Option<Box<TypedBlockStatement>>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDef {
    pub name: String,
    pub params: TypedFuncParams,
    pub body: Box<TypedBlockStatement>,
    pub return_type: ConcreteType,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDecl {
    pub name: String,
    pub params: TypedFuncParams,
    pub return_type: ConcreteType,
    pub vis: AccessSpecifier,
    pub renamed_as: Option<String>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedFuncParams {
    pub list: Vec<TypedFuncParamKind>,
    pub variadic: Option<TypedFuncVariadicParams>,
}

#[derive(Debug, Clone)]
pub enum TypedFuncVariadicParams {
    UntypedCStyle,
    Typed(String, ConcreteType),
}

#[derive(Debug, Clone)]
pub enum TypedFuncParamKind {
    FuncParam(TypedFuncParam),
    SelfModifier(SelfModifier),
}

#[derive(Debug, Clone)]
pub struct TypedFuncParam {
    pub name: String,
    pub ty: ConcreteType,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedFor {
    pub initializer: Option<TypedVariable>,
    pub condition: Option<TypedExpression>,
    pub increment: Option<TypedExpression>,
    pub body: Box<TypedBlockStatement>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedForeach {
    pub item: TypedIdentifier,
    pub index: Option<TypedIdentifier>,
    pub expr: TypedExpression,
    pub body: Box<TypedBlockStatement>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedSwitch {
    pub operand: TypedExpression,
    pub cases: Vec<TypedSwitchCase>,
    pub default_case: Option<TypedBlockStatement>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedSwitchCase {
    pub pattern: TypedSwitchCasePattern,
    pub body: Box<TypedBlockStatement>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum TypedSwitchCasePattern {
    Expression(TypedExpression),
    Identifier(String),
    EnumVariant(String, Vec<String>),
}

#[derive(Debug, Clone)]
pub struct TypedBreak {
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedContinue {
    pub loc: Location,
}
