use crate::types::ConcreteType;
use ast::{
    AccessSpecifier, Literal, SelfModifier,
    operators::{InfixOperator, UnaryOperator},
    token::Location,
};

pub mod types;

pub type ModuleID = u64;

#[derive(Debug)]
pub enum TypedProgram {
    ProgramTree(TypedASTModule),
    Statement(TypedStatement),
    Expression(TypedExpression),
}

#[derive(Debug)]
pub struct TypedASTModule {
    pub body: Vec<TypedStatement>,
}

// Expressions

#[derive(Debug, Clone)]
pub enum TypedExpression {
    Literal(Literal),
    Prefix(TypedUnaryExpression),
    Infix(TypedInfixExpression),
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

#[derive(Debug, Clone)]
pub struct TypedUnaryExpression {
    pub op: UnaryOperator,
    pub operand: Box<TypedExpression>,
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
    pub element_type: ConcreteType,
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
    pub struct_type: ConcreteType,
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
    pub func: Box<TypedExpression>,
    pub args: Vec<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedFieldAccess {
    pub operand: Box<TypedExpression>,
    pub field_name: String,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedMethodCall {
    pub receiver: Box<TypedExpression>,
    pub method_name: String,
    pub args: Vec<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedUnnamedStructValue {
    pub struct_type: ConcreteType,
    pub values: Vec<TypedExpression>,
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
    Expression(TypedExpression),
}

#[derive(Debug, Clone)]
pub struct TypedEnum {
    pub name: String,
    pub variants: Vec<TypedEnumVariant>,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum TypedEnumVariant {
    Identifier(String),
    Valued(String, Box<TypedExpression>),
    Variant(String, Vec<TypedEnumValuedField>),
}

#[derive(Debug, Clone)]
pub struct TypedEnumValuedField {
    pub name: String,
    pub field_type: ConcreteType,
}

#[derive(Debug, Clone)]
pub struct TypedStruct {
    pub name: String,
    // pub impls: Vec<TypedInterface>,
    pub fields: Vec<TypedStructField>,
    pub methods: Vec<TypedFuncDef>,
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
pub struct TypedFieldInit {
    pub name: String,
    pub value: TypedExpression,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedReturn {
    pub argument: Option<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedGlobalVariable {
    pub vis: AccessSpecifier,
    pub name: String,
    pub ty: Option<ConcreteType>,
    pub expr: Option<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedTypedef {
    pub name: String,
    pub vis: AccessSpecifier,
    pub ty: ConcreteType,
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
    pub exprs: Vec<TypedBlockStatement>,
    pub loc: Location,
}

impl TypedBlockStatement {
    pub fn new_empty() -> Self {
        Self {
            exprs: Vec::new(),
            loc: Location::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedVariable {
    pub name: String,
    pub ty: ConcreteType,
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
    pub ty: Option<ConcreteType>,
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
    pub item: String,
    pub index: Option<String>,
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
    pub body: TypedBlockStatement,
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
