use crate::{
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
    token::*,
};
use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

pub mod format;
pub mod operators;
pub mod source_loc;
pub mod token;

#[derive(Debug)]
pub struct ProgramTree {
    pub body: Rc<Vec<Statement>>,
}

impl Default for ProgramTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramTree {
    pub fn new() -> Self {
        Self {
            body: Rc::new(Vec::new()),
        }
    }

    pub fn get_imports(&self) -> Vec<Import> {
        let mut imports: Vec<Import> = Vec::new();

        self.body.iter().for_each(|stmt| match stmt {
            Statement::Import(import) => {
                imports.push(import.clone());
            }
            _ => {}
        });

        imports
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
    AddrOf(AddrOf),
    Deref(Deref),
    StructInit(StructInit),
    FuncCall(FuncCall),
    FieldAccess(FieldAccess),
    MethodCall(MethodCall),
    UStructValue(UStructValue),
    SizeOf(SizeOf),
    Lambda(Lambda),
    Tuple(TupleValue),
    TupleAccess(TupleAccess),
}

#[derive(Debug, Clone)]
pub struct TupleAccess {
    pub operand: Box<Expression>,
    pub index: usize,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleValue {
    pub expr_list: Vec<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SizeOf {
    pub expr: Box<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Deref {
    pub expr: Box<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AddrOf {
    pub expr: Box<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UStructValue {
    pub fields: Vec<UnnamedStructValueField>,
    pub is_packed: bool,
    pub is_const: bool,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructValueField {
    pub field_name: Identifier,
    pub field_ty: Option<TypeSpecifier>,
    pub field_value: Box<Expression>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructType {
    pub fields: Vec<UnnamedStructTypeField>,
    pub is_packed: bool,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructTypeField {
    pub field_name: Identifier,
    pub field_ty: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub identifier: Identifier,
    pub fields: Vec<UnionField>,
    pub generic_params: Option<GenericParamsList>,
    pub methods: Vec<FuncDef>,
    pub vis: AccessSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnionField {
    pub identifier: Identifier,
    pub ty: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub identifier: Identifier,
    pub variants: Vec<EnumVariant>,
    pub generic_params: Option<GenericParamsList>,
    pub methods: Vec<FuncDef>,
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
    pub field_ty: TypeSpecifier,
    pub loc: Location,
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
    pub type_args: Option<TypeArgs>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct MethodCall {
    pub is_fat_arrow: bool,
    pub operand: Box<Expression>,
    pub method_name: Identifier,
    pub args: Vec<Expression>,
    pub type_args: Option<TypeArgs>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
    pub loc: Location,
}

impl Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Identifier {
    pub fn as_string(&self) -> String {
        self.name.clone()
    }
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
    Integer(i64, Option<Box<TokenKind>>),
    Float(f64, Option<Box<TokenKind>>),
    String(String, Option<StringPrefix>),
    Bool(bool),
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
    Deref(Box<TypeSpecifier>),
    UnnamedStruct(UnnamedStructType),
    FuncType(Box<FuncType>),
    Tuple(TupleType),
    GenericInst(GenericInst),
}

impl TypeSpecifier {
    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const(..))
    }

    pub fn as_module_import(&self) -> Option<ModuleImport> {
        match self {
            TypeSpecifier::Identifier(identifier) => Some(ModuleImport {
                segments: vec![ModuleSegment::SubModule(identifier.clone())],
                span: identifier.span.clone(),
                loc: identifier.loc.clone(),
            }),
            TypeSpecifier::ModuleImport(module_import) => Some(module_import.clone()),
            _ => None,
        }
    }

    pub fn get_loc(&self) -> (Location, usize) {
        match self {
            TypeSpecifier::TypeToken(token) => (token.loc.clone(), token.span.end),
            TypeSpecifier::Identifier(identifier) => (identifier.loc.clone(), identifier.span.end),
            TypeSpecifier::Const(inner) => inner.get_loc(),
            TypeSpecifier::Array(array) => array.element_type.get_loc(),
            TypeSpecifier::ModuleImport(module_import) => (module_import.loc.clone(), module_import.span.end),
            TypeSpecifier::Deref(inner) => inner.get_loc(),
            TypeSpecifier::UnnamedStruct(struct_type) => (struct_type.loc.clone(), struct_type.span.end),
            TypeSpecifier::FuncType(func_type) => (func_type.loc.clone(), func_type.span.end),
            TypeSpecifier::Tuple(tuple_type) => (tuple_type.loc.clone(), tuple_type.span.end),
            TypeSpecifier::GenericInst(generic_inst) => (generic_inst.loc.clone(), generic_inst.span.end),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenericInst {
    pub base: Box<TypeSpecifier>,
    pub type_args: TypeArgs,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleType {
    pub type_list: Vec<TypeSpecifier>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayTypeSpecifier {
    pub size: ArrayCapacity,
    pub element_type: Box<TypeSpecifier>,
}

#[derive(Debug, Clone)]
pub enum ArrayCapacity {
    Fixed(Box<Expression>),
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
pub struct FuncType {
    pub params: FuncTypeParams,
    pub return_type: Box<TypeSpecifier>,
    pub vis_opt: Option<AccessSpecifier>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncTypeParams {
    pub list: Vec<TypeSpecifier>,
    pub variadic: Option<FuncTypeVariadicParams>,
}

#[derive(Debug, Clone)]
pub enum FuncTypeVariadicParams {
    UntypedCStyle,
    Typed(TypeSpecifier),
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
    Interface(Interface),
    Variable(Variable),
    ExportTuple(ExportTuple),
    Expression(Expression),
    If(If),
    Return(Return),
    FuncDef(FuncDef),
    FuncDecl(FuncDecl),
    For(For),
    While(While),
    Foreach(Foreach),
    Switch(Switch),
    Struct(Struct),
    Import(Import),
    BlockStatement(BlockStatement),
    Enum(Enum),
    Union(Union),
    Break(Break),
    Continue(Continue),
    Typedef(Typedef),
    GlobalVariable(GlobalVariable),
    Defer(Defer),
}

#[derive(Debug, Clone)]
pub struct Defer {
    pub operand: Box<Statement>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Interface {
    pub identifier: Identifier,
    pub methods: Vec<FuncDecl>,
    pub vis: AccessSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    pub vis: AccessSpecifier,
    pub identifier: Identifier,
    pub type_specifier: Option<TypeSpecifier>,
    pub expr: Option<Expression>,
    pub is_const: bool,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Typedef {
    pub identifier: Identifier,
    pub type_specifier: TypeSpecifier,
    pub generic_params: Option<GenericParamsList>,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ModuleSegmentSingle {
    pub identifier: Identifier,
    pub renamed: Option<Identifier>,
}

#[derive(Debug, Clone)]
pub enum ModuleSegment {
    SubModule(Identifier),
    Single(Vec<ModuleSegmentSingle>),
}

impl Hash for ModuleSegmentSingle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.identifier.hash(state);
        self.renamed.hash(state);
    }
}

impl ModuleSegment {
    pub fn as_identifier_opt(&self) -> Option<Identifier> {
        match self {
            ModuleSegment::SubModule(identifier) => Some(identifier.clone()),
            ModuleSegment::Single(_) => None,
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

impl Hash for ModulePath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for segment in &self.segments {
            if let ModuleSegment::SubModule(identifier) = segment {
                identifier.name.hash(state);
            }
        }
    }
}

impl Eq for ModulePath {}

impl PartialEq for ModulePath {
    fn eq(&self, other: &Self) -> bool {
        let self_submodules: Vec<&String> = self
            .segments
            .iter()
            .filter_map(|segment| {
                if let ModuleSegment::SubModule(identifier) = segment {
                    Some(&identifier.name)
                } else {
                    None
                }
            })
            .collect();

        let other_submodules: Vec<&String> = other
            .segments
            .iter()
            .filter_map(|segment| {
                if let ModuleSegment::SubModule(identifier) = segment {
                    Some(&identifier.name)
                } else {
                    None
                }
            })
            .collect();

        self_submodules == other_submodules
    }
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
    pub identifier: Identifier,
    pub generic_params: Option<GenericParamsList>,
    pub impls: Vec<Identifier>,
    pub fields: Vec<StructField>,
    pub methods: Vec<FuncDef>,
    pub vis: AccessSpecifier,
    pub is_packed: bool,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructInit {
    pub struct_name: ModuleImport,
    pub field_inits: Vec<FieldInit>,
    pub type_args: Option<TypeArgs>,
    pub is_const: bool,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub identifier: Identifier,
    pub vis: AccessSpecifier,
    pub ty: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldInit {
    pub identifier: Identifier,
    pub value: Expression,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expression,
    pub body: Box<BlockStatement>,
    pub span: Span,
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
pub struct Lambda {
    pub params: FuncParams,
    pub body: Box<BlockStatement>,
    pub return_type: TypeSpecifier,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub identifier: Identifier,
    pub params: FuncParams,
    pub body: Box<BlockStatement>,
    pub return_type: Option<TypeSpecifier>,
    pub vis: AccessSpecifier,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub identifier: Identifier,
    pub params: FuncParams,
    pub return_type: Option<TypeSpecifier>,
    pub vis: AccessSpecifier,
    pub renamed_as: Option<Identifier>,
    pub span: Span,
    pub loc: Location,
}

impl FuncDef {
    pub fn as_func_decl(&self) -> FuncDecl {
        FuncDecl {
            identifier: self.identifier.clone(),
            params: self.params.clone(),
            return_type: self.return_type.clone(),
            vis: self.vis.clone(),
            renamed_as: None,
            span: self.span.clone(),
            loc: self.loc.clone(),
        }
    }
}

impl FuncDecl {
    pub fn get_usable_name(&self) -> String {
        match &self.renamed_as {
            Some(identifier) => identifier.name.clone(),
            None => self.identifier.name.clone(),
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

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub exprs: Vec<Statement>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub identifier: Identifier,
    pub ty: Option<TypeSpecifier>,
    pub rhs: Option<Expression>,
    pub is_const: bool,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct ExportTuple {
    pub exports: Vec<Identifier>,
    pub ty: Option<TypeSpecifier>,
    pub rhs: Option<Expression>,
    pub is_const: bool,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lhs: Expression,
    pub rhs: Expression,
    pub kind: AssignmentKind,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentKind {
    Default,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitwiseAndAssign,
    BitwiseXorAssign,
    BitwiseAndNotAssign,
    LeftShiftAssign,
    RightShiftAssign,
}

impl AssignmentKind {
    pub fn to_infix_operator(&self) -> InfixOperator {
        match self {
            AssignmentKind::Default => unreachable!(),
            AssignmentKind::AddAssign => InfixOperator::Add,
            AssignmentKind::SubAssign => InfixOperator::Sub,
            AssignmentKind::MulAssign => InfixOperator::Mul,
            AssignmentKind::DivAssign => InfixOperator::Div,
            AssignmentKind::ModAssign => InfixOperator::Rem,
            AssignmentKind::BitwiseAndAssign => InfixOperator::BitwiseAnd,
            AssignmentKind::BitwiseXorAssign => InfixOperator::BitwiseXor,
            AssignmentKind::BitwiseAndNotAssign => InfixOperator::BitwiseAndNot,
            AssignmentKind::LeftShiftAssign => InfixOperator::ShiftLeft,
            AssignmentKind::RightShiftAssign => InfixOperator::ShiftRight,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SelfModifier {
    pub kind: SelfModifierKind,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SelfModifierKind {
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

impl Statement {
    pub fn get_loc(&self) -> Location {
        match self {
            Statement::Interface(interface) => interface.loc.clone(),
            Statement::Variable(variable) => variable.loc.clone(),
            Statement::ExportTuple(export_tuple_values) => export_tuple_values.loc.clone(),
            Statement::If(if_stmt) => if_stmt.loc.clone(),
            Statement::Return(ret) => ret.loc.clone(),
            Statement::FuncDef(func_def) => func_def.loc.clone(),
            Statement::FuncDecl(func_decl) => func_decl.loc.clone(),
            Statement::For(for_stmt) => for_stmt.loc.clone(),
            Statement::While(while_stmt) => while_stmt.loc.clone(),
            Statement::Foreach(foreach) => foreach.loc.clone(),
            Statement::Switch(switch) => switch.loc.clone(),
            Statement::Struct(struct_stmt) => struct_stmt.loc.clone(),
            Statement::Import(import) => import.loc.clone(),
            Statement::BlockStatement(block_stmt) => block_stmt.loc.clone(),
            Statement::Enum(enum_stmt) => enum_stmt.loc.clone(),
            Statement::Union(union) => union.loc.clone(),
            Statement::Break(brk) => brk.loc.clone(),
            Statement::Continue(cont) => cont.loc.clone(),
            Statement::Typedef(typedef) => typedef.loc.clone(),
            Statement::GlobalVariable(global_variable) => global_variable.loc.clone(),
            Statement::Defer(defer) => defer.loc.clone(),
            Statement::Expression(..) => unreachable!(),
        }
    }
}

pub type GenericParamsList = Vec<GenericParam>;

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub param_name: Identifier,
    pub bounds: Option<Vec<Bound>>,
    pub default: Option<TypeSpecifier>,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub symbol: Identifier,
    pub type_args: Vec<TypeArg>,
}

#[derive(Debug, Clone)]
pub enum TypeArg {
    Positional(TypeSpecifier),
    Named { key: Identifier, ty: TypeSpecifier },
}

pub type TypeArgs = Vec<TypeArg>;
