use crate::{
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
    token::*,
};

pub mod format;
pub mod operators;
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

#[derive(Debug, Clone)]
pub enum Expression {
    Cast(Cast),
    Identifier(Identifier),
    TypeSpecifier(TypeSpecifier),
    ModuleImport(ModuleImport),
    Assignment(Box<Assignment>),
    Literal(Literal),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Unary(UnaryExpression),
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

#[derive(Debug, Clone)]
pub struct Dereference {
    pub expr: Box<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AddressOf {
    pub expr: Box<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructValue {
    pub fields: Vec<UnnamedStructValueField>,
    pub packed: bool,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructValueField {
    pub field_name: Identifier,
    pub field_type: Option<TypeSpecifier>,
    pub field_value: Box<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructType {
    pub fields: Vec<UnnamedStructTypeField>,
    pub packed: bool,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructTypeField {
    pub field_name: String,
    pub field_type: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub identifier: Identifier,
    pub variants: Vec<EnumVariant>,
    pub vis: AccessSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Identifier(Identifier),
    Variant(Identifier, Vec<EnumValuedField>),
    Valued(Identifier, Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct EnumValuedField {
    pub identifier: Identifier,
    pub field_type: TypeSpecifier,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub expr: Box<Expression>,
    pub target_type: TypeSpecifier,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub operand: Box<Expression>,
    pub op: PrefixOperator,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub operand: Box<Expression>,
    pub args: Vec<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub is_fat_arrow: bool,
    pub operand: Box<Expression>,
    pub field_name: Identifier,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct MethodCall {
    pub is_fat_arrow: bool,
    pub operand: Box<Expression>,
    pub method_name: Identifier,
    pub args: Vec<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub kind: LiteralKind,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String, Option<StringPrefix>),
    Char(char),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPrefix {
    C, // C-style string which is const char*
    B, // Bytes string
}

#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    TypeToken(Token),
    Identifier(Identifier),
    Const(Box<TypeSpecifier>),
    Array(ArrayTypeSpecifier),
    ModuleImport(ModuleImport),
    Dereference(Box<TypeSpecifier>),
    UnnamedStruct(UnnamedStructType),
}

#[derive(Debug, Clone)]
pub struct ArrayTypeSpecifier {
    pub size: ArrayCapacity,
    pub element_type: Box<TypeSpecifier>,
}

#[derive(Debug, Clone)]
pub enum ArrayCapacity {
    Fixed(TokenKind),
    Dynamic,
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operand: Box<Expression>,
    pub op: UnaryOperator,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub op: InfixOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub data_type: TypeSpecifier,
    pub elements: Vec<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct ArrayIndex {
    pub operand: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    pub vis: AccessSpecifier,
    pub identifier: Identifier,
    pub type_specifier: Option<TypeSpecifier>,
    pub expr: Option<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Typedef {
    pub identifier: Identifier,
    pub type_specifier: TypeSpecifier,
    pub vis: AccessSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Break {
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Continue {
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub argument: Option<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct ModuleSegmentSingle {
    pub identifier: Identifier,
    pub renamed: Option<Identifier>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Import {
    pub paths: Vec<ModulePath>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub impls: Vec<Identifier>,
    pub fields: Vec<StructField>,
    pub methods: Vec<FuncDef>,
    pub vis: AccessSpecifier,
    pub packed: bool,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructInit {
    pub struct_name: ModuleImport,
    pub field_inits: Vec<FieldInit>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub ty: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldInit {
    pub name: String,
    pub value: Expression,
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
pub struct Foreach {
    pub item: Identifier,
    pub index: Option<Identifier>,
    pub expr: Expression,
    pub body: Box<BlockStatement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Switch {
    pub operand: Expression,
    pub cases: Vec<SwitchCase>,
    pub default_case: Option<BlockStatement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub pattern: SwitchCasePattern,
    pub body: BlockStatement,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum SwitchCasePattern {
    Expression(Expression),
    Identifier(Identifier),
    EnumVariant(Identifier, Vec<Identifier>),
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: FuncParams,
    pub body: Box<BlockStatement>,
    pub return_type: Option<TypeSpecifier>,
    pub vis: AccessSpecifier,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub params: FuncParams,
    pub return_type: Option<TypeSpecifier>,
    pub vis: AccessSpecifier,
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub exprs: Vec<Statement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: Option<TypeSpecifier>,
    pub rhs: Option<Expression>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lhs: Expression,
    pub rhs: Expression,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum SelfModifier {
    Copied,
    Referenced,
}

#[derive(Debug, Clone)]
pub enum FuncParamKind {
    FuncParam(FuncParam),
    SelfModifier(SelfModifier),
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub identifier: Identifier,
    pub ty: Option<TypeSpecifier>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncParams {
    pub list: Vec<FuncParamKind>,
    pub variadic: Option<FuncVariadicParams>,
}

#[derive(Debug, Clone)]
pub enum FuncVariadicParams {
    UntypedCStyle,
    Typed(Identifier, TypeSpecifier),
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub consequent: Box<BlockStatement>,
    pub branches: Vec<If>,
    pub alternate: Option<Box<BlockStatement>>,
    pub span: Span,
    pub loc: Location,
}
