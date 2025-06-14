use crate::token::*;

#[derive(Debug)]
pub enum Node {
    ProgramTree(ProgramTree),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub struct ProgramTree {
    pub body: Vec<Statement>,
    pub span: Span,
}

impl Default for ProgramTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramTree {
    pub fn new() -> Self {
        Self {
            body: vec![],
            span: Span::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Cast(Cast),
    Identifier(Identifier),
    TypeSpecifier(TypeSpecifier),
    ModuleImport(ModuleImport),
    Assignment(Box<Assignment>),
    Literal(Literal),
    Prefix(UnaryExpression),
    Infix(BinaryExpression),
    UnaryOperator(UnaryOperator),
    Array(Array),
    ArrayIndex(ArrayIndex),
    AddressOf(Box<Expression>),
    Dereference(Box<Expression>),
    StructInit(StructInit),
    FuncCall(FuncCall),
    FieldAccess(FieldAccess),
    MethodCall(MethodCall),
    UnnamedStructValue(UnnamedStructValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnnamedStructValue {
    pub fields: Vec<UnnamedStructValueField>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnnamedStructValueField {
    pub field_name: Identifier,
    pub field_type: Option<TypeSpecifier>,
    pub field_value: Box<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnnamedStructType {
    pub fields: Vec<UnnamedStructTypeField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnnamedStructTypeField {
    pub field_name: String,
    pub field_type: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: Identifier,
    pub variants: Vec<EnumField>,
    pub storage_class: StorageClass,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumField {
    OnlyIdentifier(Identifier),
    Variant(Identifier, Vec<EnumValuedField>),
    Valued(Identifier, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumValuedField {
    pub name: Identifier,
    pub field_type: TypeSpecifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cast {
    pub expr: Box<Expression>,
    pub target_type: TypeSpecifier,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperatorType {
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperator {
    pub module_import: ModuleImport,
    pub ty: UnaryOperatorType,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub operand: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub operand: Box<Expression>,
    pub field_name: Identifier,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodCall {
    pub operand: Box<Expression>,
    pub method_name: Identifier,
    pub arguments: Vec<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleImport {
    pub segments: Vec<ModuleSegment>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeSpecifier {
    TypeToken(Token),
    Identifier(Identifier),
    Const(Box<TypeSpecifier>),
    Array(ArrayTypeSpecifier),
    ModuleImport(ModuleImport),
    AddressOf(Box<TypeSpecifier>),
    Dereference(Box<TypeSpecifier>),
    UnnamedStruct(UnnamedStructType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayTypeSpecifier {
    pub size: ArrayCapacity,
    pub element_type: Box<TypeSpecifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayCapacity {
    Fixed(TokenKind),
    Dynamic,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: Token,
    pub operand: Box<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub operator: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub data_type: TypeSpecifier,
    pub elements: Vec<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayIndex {
    pub expr: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Hash {
    pub pairs: Vec<(Expression, Expression)>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Variable(Variable),
    Expression(Expression),
    If(If),
    Return(Return),
    FuncDef(FuncDef),
    FuncDecl(FuncDecl),
    For(For),
    Switch(Switch),
    Struct(Struct),
    Import(Import),
    BlockStatement(BlockStatement),
    Enum(Enum),
    Break(Break),
    Continue(Continue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Continue {
    pub loc: Location,
    pub span: Span,
}

pub fn format_expressions(exprs: &Vec<Expression>) -> String {
    exprs.iter().map(|expr| expr.to_string()).collect()
}

pub fn format_statements(stmts: &Vec<Statement>) -> String {
    stmts.iter().map(|stmt| stmt.to_string()).collect()
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub argument: Expression,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleSegment {
    SubModule(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModulePath {
    pub alias: Option<String>,
    pub segments: Vec<ModuleSegment>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub paths: Vec<ModulePath>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub inherits: Vec<Identifier>,
    pub fields: Vec<Field>,
    pub methods: Vec<FuncDef>,
    pub storage_class: StorageClass,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInit {
    pub struct_name: ModuleImport,
    pub field_inits: Vec<FieldInit>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInit {
    pub name: String,
    pub value: Expression,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct For {
    pub initializer: Option<Variable>,
    pub condition: Option<Expression>,
    pub increment: Option<Expression>,
    pub body: Box<BlockStatement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Switch {
    pub value: Expression,
    pub sections: Option<Vec<MatchPattern>>,
    pub default: BlockStatement,
    pub body: Box<BlockStatement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchPattern {
    pub raw: Expression,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef {
    pub name: String,
    pub params: FuncParams,
    pub body: Box<BlockStatement>,
    pub return_type: Option<TypeSpecifier>,
    pub storage_class: StorageClass,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub name: String,
    pub params: FuncParams,
    pub return_type: Option<TypeSpecifier>,
    pub storage_class: StorageClass,
    pub renamed_as: Option<String>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StorageClass {
    Extern,
    Public,
    Internal,
    Inline,
    PublicInline,
    PublicExtern,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub exprs: Vec<Statement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub ty: Option<TypeSpecifier>,
    pub expr: Option<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub assign_to: Expression,
    pub expr: Expression,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub identifier: Identifier,
    pub ty: Option<TypeSpecifier>,
    pub default_value: Option<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParams {
    pub list: Vec<FuncParam>,
    pub variadic: Option<FuncVariadicParams>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncVariadicParams {
    UntypedCStyle,
    Typed(Identifier, TypeSpecifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Expression,
    pub consequent: Box<BlockStatement>,
    pub branches: Vec<If>,
    pub alternate: Option<Box<BlockStatement>>,
    pub span: Span,
    pub loc: Location,
}
