use crate::token::*;

pub mod format;
pub mod token;

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
    AddressOf(AddressOf),
    Dereference(Dereference),
    StructInit(StructInit),
    FuncCall(FuncCall),
    FieldAccess(FieldAccess),
    MethodCall(MethodCall),
    UnnamedStructValue(UnnamedStructValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dereference {
    pub expr: Box<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AddressOf {
    pub expr: Box<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnnamedStructValue {
    pub fields: Vec<UnnamedStructValueField>,
    pub packed: bool,
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
    pub packed: bool,
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
    pub identifier: Identifier,
    pub variants: Vec<EnumField>,
    pub access_specifier: AccessSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumField {
    Identifier(Identifier),
    Variant(Identifier, Vec<EnumValuedField>),
    Valued(Identifier, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumValuedField {
    pub identifier: Identifier,
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
    pub is_fat_arrow: bool,
    pub operand: Box<Expression>,
    pub field_name: Identifier,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodCall {
    pub is_fat_arrow: bool,
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

impl ModuleImport {
    pub fn as_identifier(&self) -> Option<Identifier> {
        if self.segments.len() == 1 {
            match self.segments.last().unwrap() {
                ModuleSegment::SubModule(identifier) => Some(identifier.clone()),
                ModuleSegment::Single(_) => None,
            }
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralKind {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String, Option<StringPrefix>),
    Char(char),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringPrefix {
    C, // C-style string which is const char*
    B, // Bytes string
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeSpecifier {
    TypeToken(Token),
    Identifier(Identifier),
    Const(Box<TypeSpecifier>),
    Array(ArrayTypeSpecifier),
    ModuleImport(ModuleImport),
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
    Foreach(Foreach),
    Switch(Switch),
    Struct(Struct),
    Import(Import),
    BlockStatement(BlockStatement),
    Enum(Enum),
    Break(Break),
    Continue(Continue),
    Typedef(Typedef),
    GlobalVariable(GlobalVariable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVariable {
    pub access_specifier: AccessSpecifier,
    pub identifier: Identifier,
    pub type_specifier: Option<TypeSpecifier>,
    pub expr: Option<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Typedef {
    pub access_specifier: AccessSpecifier,
    pub identifier: Identifier,
    pub type_specifier: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
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
    pub argument: Option<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleSegmentSingle {
    pub identifier: Identifier,
    pub renamed: Option<Identifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleSegment {
    SubModule(Identifier),
    Single(Vec<ModuleSegmentSingle>),
}

impl ModuleSegment {
    pub fn as_identifier(&self) -> Identifier {
        match self {
            ModuleSegment::SubModule(identifier) => identifier.clone(),
            ModuleSegment::Single(_) => {
                panic!("ModuleSegment is not a SubModule.");
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModulePath {
    pub alias: Option<String>,
    pub segments: Vec<ModuleSegment>,
    pub loc: Location,
    pub span: Span,
}

impl ModulePath {
    pub fn as_module_import(&self) -> ModuleImport {
        ModuleImport {
            segments: self.segments.clone(),
            span: self.span.clone(),
            loc: self.loc.clone(),
        }
    }
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
    pub access_specifier: AccessSpecifier,
    pub packed: bool,
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
pub struct Foreach {
    pub item: Identifier,
    pub index: Option<Identifier>,
    pub expr: Expression,
    pub body: Box<BlockStatement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Switch {
    pub value: Expression,
    pub cases: Vec<SwitchCase>,
    pub default: Option<BlockStatement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchCase {
    pub pattern: SwitchCasePattern,
    pub body: BlockStatement,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SwitchCasePattern {
    Expression(Expression),
    Identifier(Identifier),
    EnumVariant(Identifier, Vec<Identifier>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef {
    pub name: String,
    pub params: FuncParams,
    pub body: Box<BlockStatement>,
    pub return_type: Option<TypeSpecifier>,
    pub access_specifier: AccessSpecifier,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub name: String,
    pub params: FuncParams,
    pub return_type: Option<TypeSpecifier>,
    pub access_specifier: AccessSpecifier,
    pub renamed_as: Option<String>,
    pub span: Span,
    pub loc: Location,
}

impl FuncDecl {
    pub fn get_usable_name(&self) -> String {
        match &self.renamed_as {
            Some(name) => name.clone(),
            None => self.name.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessSpecifier {
    Extern,
    Public,
    Internal,
    Inline,
    PublicInline,
    PublicExtern,
}

impl AccessSpecifier {
    pub fn is_private(&self) -> bool {
        matches!(self, Self::Internal | Self::Inline | Self::Extern)
    }

    pub fn is_public(&self) -> bool {
        matches!(self, Self::Public | Self::PublicInline | Self::PublicExtern)
    }
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
pub enum SelfModifier {
    Copied,
    Referenced,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncParamKind {
    FuncParam(FuncParam),
    SelfModifier(SelfModifier),
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
    pub list: Vec<FuncParamKind>,
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
