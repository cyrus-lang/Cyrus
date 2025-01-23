use crate::token::*;

#[derive(Debug)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub struct Program {
    pub body: Vec<Statement>,
    pub span: Span,
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl Program {
    pub fn new() -> Self {
        Self {
            body: vec![],
            span: Span::new_empty_span(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Assignment(Box<Assignment>),
    Literal(Literal),
    Prefix(UnaryExpression),
    Infix(BinaryExpression),
    FunctionCall(FunctionCall),
    UnaryOperator(UnaryOperator),
    Array(Array),
    ArrayIndex(ArrayIndex),
    ArrayIndexAssign(Box<ArrayIndexAssign>),
}

#[derive(Debug, Clone)]
pub enum UnaryOperatorType {
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
}

#[derive(Debug, Clone)]
pub struct UnaryOperator {
    pub identifer: Identifier,
    pub ty: UnaryOperatorType,
    pub span: Span,
    pub loc: Location
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub function_name: Identifier,
    pub arguments: Vec<Expression>,
    pub span: Span,
    pub loc: Location
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
    pub loc: Location
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(IntegerLiteral),
    Float(FloatLiteral),
    Bool(BoolLiteral),
    String(StringLiteral),
    Char(CharLiteral),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CharLiteral {
    pub raw: char,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntegerLiteral {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FloatLiteral {
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: Token,
    pub operand: Box<Expression>,
    pub span: Span,
    pub loc: Location
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub operator: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
    pub loc: Location
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BoolLiteral {
    pub raw: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StringLiteral {
    pub raw: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub elements: Vec<Expression>,
    pub span: Span,
    pub loc: Location
}

#[derive(Debug, Clone)]
pub struct ArrayIndex {
    pub identifier: Identifier,
    pub dimensions: Vec<Expression>,
    pub span: Span,
    pub loc: Location
}

#[derive(Debug, Clone)]
pub struct ArrayIndexAssign {
    pub identifier: Identifier,
    pub dimensions: Vec<Expression>,
    pub expr: Expression,
    pub span: Span,
    pub loc: Location
}

#[derive(Debug, Clone)]
pub struct Hash {
    pub pairs: Vec<(Expression, Expression)>,
    pub span: Span,
    pub loc: Location
}

#[derive(Debug, Clone)]
pub enum Statement {
    Variable(Variable),
    Expression(Expression),
    If(If),
    Return(Return),
    FuncDef(FuncDef),
    For(For),
    Match(Match),
    Struct(Struct),
    Import(Import),
    BlockStatement(BlockStatement),
    Break(Location),
    Continue(Location),
}

pub fn format_expressions(exprs: &Vec<Expression>) -> String {
    exprs.iter().map(|expr| expr.to_string()).collect()
}

pub fn format_statements(stmts: &Vec<Statement>) -> String {
    stmts.iter().map(|stmt| stmt.to_string()).collect()
}

#[derive(Debug, Clone)]
pub struct Return {
    pub argument: Expression,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct PackagePath {
    pub package_name: Identifier,
    pub is_relative: bool,
    pub span: Span,
    pub loc: Location,
}


#[derive(Debug, Clone)]
pub struct Import {
    pub sub_packages: Vec<PackagePath>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Identifier,
    pub fields: Option<Field>,
    pub extends: Option<Box<Struct>>,
    pub methods: Option<FuncDef>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Identifier,
    pub ty: Literal,
    pub default_value: Literal,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct For {
    pub initializer: Option<Variable>,
    pub condition: Option<Expression>,
    pub increment: Option<Expression>,
    pub body: Box<BlockStatement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub value: Expression,
    pub sections: Option<Vec<MatchPattern>>,
    pub default: BlockStatement,
    pub body: Box<BlockStatement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct MatchPattern {
    pub raw: Expression,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: FunctionParams,
    pub body: Box<BlockStatement>,
    pub return_type: Option<Token>,
    pub span: Span,
    pub vis_type: FuncVisType,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum FuncVisType {
    Extern,
    Pub,
    Internal,
    Inline,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: Option<TokenKind>,
    pub expr: Option<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub identifier: Identifier,
    pub expr: Expression,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub identifier: Identifier,
    pub ty: Option<TokenKind>,
    pub default_value: Option<Expression>,
    pub span: Span,
    pub loc: Location,
}

pub type FunctionParams = Vec<FunctionParam>;

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub consequent: Box<BlockStatement>,
    pub branches: Vec<If>,
    pub alternate: Option<Box<BlockStatement>>,
    pub span: Span,
    pub loc: Location,
}

pub fn integer_literal_as_value(integer_literal: IntegerLiteral) -> i64 {
    match integer_literal {
        IntegerLiteral::I8(value) => value.into(),
        IntegerLiteral::I16(value ) => value.into(),
        IntegerLiteral::I32(value) => value.into(),
        IntegerLiteral::I64(value) => value,
        IntegerLiteral::I128(value) => value.try_into().unwrap(),
        IntegerLiteral::U8(value) => value.into(),
        IntegerLiteral::U16(value) => value.into(),
        IntegerLiteral::U32(value) => value.into(),
        IntegerLiteral::U64(value) => value.try_into().unwrap(),
        IntegerLiteral::U128(value) => value.try_into().unwrap(),
    }
}