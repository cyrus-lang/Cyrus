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
    TypeToken(Token),
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
    MethodCall(MethodCall)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: Identifier,
    pub variants: Vec<EnumVariant>,
    pub loc: Location
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: Identifier,
    pub fields: Option<Vec<EnumField>>,
    pub loc: Location
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumField {
    pub name: Identifier,
    pub field_type: TokenKind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cast {
    pub expr: Box<Expression>,
    pub type_token: TokenKind,
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
    pub identifier: Identifier,
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
    pub data_type: TokenKind,
    pub elements: Vec<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayIndex {
    pub expr: Box<Expression>,
    pub dimensions: Vec<Expression>,
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
    Break(Location),
    Continue(Location),
    Enum(Enum),
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
    pub vis_type: VisType,
    pub inherits: Vec<Identifier>,
    pub fields: Vec<Field>,
    pub methods: Vec<FuncDef>,
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
    pub ty: TokenKind,
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
    pub return_type: Option<Token>,
    pub vis_type: VisType,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub name: String,
    pub params: FuncParams,
    pub return_type: Option<Token>,
    pub vis_type: VisType,
    pub renamed_as: Option<String>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VisType {
    Extern,
    Pub,
    Internal,
    Inline,
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
    pub ty: Option<TokenKind>,
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
    pub ty: Option<TokenKind>,
    pub default_value: Option<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParams {
    pub list: Vec<FuncParam>,
    pub variadic: Option<TokenKind>,
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
