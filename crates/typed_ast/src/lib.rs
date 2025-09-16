use crate::types::{ConcreteType, TypedUnnamedStructType};
use ast::{
    AccessSpecifier, AssignmentKind, Identifier, LiteralKind, SelfModifierKind,
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
    token::Location,
};
use std::collections::HashMap;

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

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpression {
    pub kind: TypedExpressionKind,
    pub concrete_type: Option<ConcreteType>,
    pub value_category: ValueCategory,
    pub loc: Location,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueCategory {
    Lvalue,
    Rvalue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpressionKind {
    Symbol(SymbolID, Location),
    Literal(TypedLiteral),
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
    SizeOfExpression(TypedSizeOfExpression),
    ConcreteType(ConcreteType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedSizeOfExpression {
    pub expr: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedLiteral {
    pub ty: Option<ConcreteType>,
    pub kind: LiteralKind,
    pub loc: Location,
}

impl TypedLiteral {
    pub fn format_kind(&self) -> String {
        match &self.kind {
            LiteralKind::Integer(..) => "integer",
            LiteralKind::Float(..) => "float",
            LiteralKind::String(..) => "string",
            LiteralKind::Bool(..) => "bool",
            LiteralKind::Char(..) => "char",
            LiteralKind::Null => "null",
        }
        .to_string()
    }
}

impl TypedExpression {
    pub fn is_lvalue(&self) -> bool {
        self.value_category == ValueCategory::Lvalue
    }

    pub fn is_rvalue(&self) -> bool {
        self.value_category == ValueCategory::Rvalue
    }
}

impl TypedExpressionKind {
    pub fn is_comptime_valid(&self) -> bool {
        match self {
            TypedExpressionKind::Literal(_) => true,
            TypedExpressionKind::Prefix(prefix) => prefix.operand.kind.is_comptime_valid(),
            TypedExpressionKind::Infix(infix) => {
                infix.lhs.kind.is_comptime_valid() && infix.rhs.kind.is_comptime_valid()
            }
            TypedExpressionKind::Unary(unary) => unary.operand.kind.is_comptime_valid(),
            TypedExpressionKind::Cast(cast) => cast.operand.kind.is_comptime_valid(),
            TypedExpressionKind::Array(typed_array) => typed_array
                .elements
                .iter()
                .all(|typed_expr| typed_expr.kind.is_comptime_valid()),
            TypedExpressionKind::StructInit(typed_struct_init) => typed_struct_init
                .fields
                .iter()
                .all(|field_init| field_init.value.kind.is_comptime_valid()),
            TypedExpressionKind::UnnamedStructValue(typed_unnamed_struct_value) => typed_unnamed_struct_value
                .fields
                .iter()
                .all(|typed_unnamed_struct_value_field| {
                    typed_unnamed_struct_value_field.field_value.kind.is_comptime_valid()
                }),
            TypedExpressionKind::Symbol(..)
            | TypedExpressionKind::ArrayIndex(_)
            | TypedExpressionKind::Dereference(_)
            | TypedExpressionKind::FieldAccess(_)
            | TypedExpressionKind::MethodCall(_)
            | TypedExpressionKind::FuncCall(_)
            | TypedExpressionKind::Assignment(_)
            | TypedExpressionKind::SizeOfExpression(_)
            | TypedExpressionKind::ConcreteType(_)
            | TypedExpressionKind::AddressOf(_) => false,
        }
    }

    pub fn is_lvalue(&self) -> bool {
        match self {
            TypedExpressionKind::Symbol(..) => true,
            TypedExpressionKind::ArrayIndex(_) => true,
            TypedExpressionKind::Dereference(_) => true,
            TypedExpressionKind::FieldAccess(_) => true,
            TypedExpressionKind::MethodCall(_) => false,
            TypedExpressionKind::FuncCall(_) => false,
            TypedExpressionKind::StructInit(_) => false,
            TypedExpressionKind::UnnamedStructValue(_) => false,
            TypedExpressionKind::Literal(_) => false,
            TypedExpressionKind::Prefix(_) => false,
            TypedExpressionKind::Infix(_) => false,
            TypedExpressionKind::Unary(_) => false,
            TypedExpressionKind::Assignment(_) => false,
            TypedExpressionKind::Cast(_) => false,
            TypedExpressionKind::AddressOf(_) => false,
            TypedExpressionKind::Array(_) => false,
            TypedExpressionKind::SizeOfExpression(_) => false,
            TypedExpressionKind::ConcreteType(_) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedIdentifier {
    pub name: String,
    pub symbol_id: SymbolID,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedPrefixExpression {
    pub op: PrefixOperator,
    pub operand: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnaryExpression {
    pub operand: Box<TypedExpression>,
    pub op: UnaryOperator,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedInfixExpression {
    pub op: InfixOperator,
    pub lhs: Box<TypedExpression>,
    pub rhs: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedAssignment {
    pub lhs: Box<TypedExpression>,
    pub rhs: Box<TypedExpression>,
    pub kind: AssignmentKind,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedCast {
    pub operand: Box<TypedExpression>,
    pub target_type: ConcreteType,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArray {
    pub array_type: ConcreteType,
    pub elements: Vec<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArrayIndex {
    pub operand: Box<TypedExpression>,
    pub index: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedAddressOf {
    pub operand: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedDereference {
    pub operand: Box<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructInit {
    pub symbol_id: SymbolID,
    pub fields: Vec<TypedStructFieldInit>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructFieldInit {
    pub name: String,
    pub value: TypedExpression,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFuncCall {
    pub symbol_id: SymbolID,
    pub args: Vec<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFieldAccess {
    pub operand: Box<TypedExpression>,
    pub field_name: String,
    pub field_index: Option<usize>,
    pub field_ty: Option<ConcreteType>,
    pub object_symbol_id: Option<SymbolID>,
    pub is_fat_arrow: bool,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedMethodCall {
    pub symbol_id: SymbolID,
    pub operand: Box<TypedExpression>,
    pub method_name: String,
    pub args: Vec<TypedExpression>,
    pub is_fat_arrow: bool,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnnamedStructValue {
    pub fields: Vec<TypedUnnamedStructValueField>,
    pub unnamed_struct_type: Option<TypedUnnamedStructType>,
    pub packed: bool,
    pub is_const: bool,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
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
    While(TypedWhile),
    Switch(TypedSwitch),
    Struct(TypedStruct),
    Enum(TypedEnum),
    Union(TypedUnion),
    Interface(TypedInterface),
    Expression(TypedExpression),
}

impl TypedStatement {
    pub fn get_loc(&self) -> Location {
        match self {
            TypedStatement::Import(typed_import) => typed_import.loc.clone(),
            TypedStatement::Variable(typed_variable) => typed_variable.loc.clone(),
            TypedStatement::Typedef(typed_typedef) => typed_typedef.loc.clone(),
            TypedStatement::GlobalVariable(typed_global_variable) => typed_global_variable.loc.clone(),
            TypedStatement::FuncDef(typed_func_def) => typed_func_def.loc.clone(),
            TypedStatement::FuncDecl(typed_func_decl) => typed_func_decl.loc.clone(),
            TypedStatement::BlockStatement(typed_block_statement) => typed_block_statement.loc.clone(),
            TypedStatement::If(typed_if) => typed_if.loc.clone(),
            TypedStatement::Return(typed_return) => typed_return.loc.clone(),
            TypedStatement::Break(typed_break) => typed_break.loc.clone(),
            TypedStatement::Continue(typed_continue) => typed_continue.loc.clone(),
            TypedStatement::For(typed_for) => typed_for.loc.clone(),
            TypedStatement::Switch(typed_switch) => typed_switch.loc.clone(),
            TypedStatement::Struct(typed_struct) => typed_struct.loc.clone(),
            TypedStatement::Enum(typed_enum) => typed_enum.loc.clone(),
            TypedStatement::Interface(typed_interface) => typed_interface.loc.clone(),
            TypedStatement::Expression(typed_expression) => typed_expression.loc.clone(),
            TypedStatement::While(while_stmt) => while_stmt.loc.clone(),
            TypedStatement::Union(union_stmt) => union_stmt.loc.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedInterface {
    pub name: String,
    pub symbol_id: SymbolID,
    pub methods: Vec<TypedFuncDecl>,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedEnum {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
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

impl TypedEnumVariant {
    pub fn get_identifier(&self) -> &Identifier {
        match self {
            TypedEnumVariant::Identifier(identifier) => identifier,
            TypedEnumVariant::Valued(identifier, ..) => identifier,
            TypedEnumVariant::Variant(identifier, ..) => identifier,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedEnumValuedField {
    pub field_type: ConcreteType,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedStruct {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub methods: HashMap<String, SymbolID>,
    pub vis: AccessSpecifier,
    pub packed: bool,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedUnion {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub fields: Vec<TypedUnionField>,
    pub methods: HashMap<String, SymbolID>,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedUnionField {
    pub name: String,
    pub ty: ConcreteType,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedStructField {
    pub name: String,
    pub ty: ConcreteType,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedReturn {
    pub argument: Option<TypedExpression>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedGlobalVariable {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
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
    pub symbol_id: SymbolID,
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
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub params: TypedFuncParams,
    pub body: Box<TypedBlockStatement>,
    pub return_type: ConcreteType,
    pub vis: AccessSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDecl {
    pub symbol_id: SymbolID,
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
    SelfModifier(TypedSelfModifier),
}

#[derive(Debug, Clone)]
pub struct TypedSelfModifier {
    pub symbol_id: Option<SymbolID>,
    pub self_symbol_id: Option<SymbolID>,
    pub ty: Option<ConcreteType>,
    pub kind: SelfModifierKind,
    pub loc: Location,
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
pub struct TypedWhile {
    pub condition: TypedExpression,
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
    Expression(TypedExpression, Location),
    Identifier(String, Location),
    EnumVariant(String, Vec<Identifier>, Location),
}

#[derive(Debug, Clone)]
pub struct TypedBreak {
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct TypedContinue {
    pub loc: Location,
}
