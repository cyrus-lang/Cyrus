/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::operators::{InfixOperator, PrefixOperator, UnaryOperator};
use cyrusc_abi::{
    modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers},
    visibility::Visibility,
};
use cyrusc_tokens::{
    Token,
    literals::Literal,
    loc::{Location, Span},
};
use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

pub mod format;
pub mod operators;

#[derive(Debug)]
pub struct ProgramTree {
    pub body: Rc<Vec<Stmt>>,
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

    pub fn import_stmts(&self) -> Vec<Import> {
        let mut imports: Vec<Import> = Vec::new();

        self.body.iter().for_each(|stmt| match stmt {
            Stmt::Import(import) => {
                imports.push(import.clone());
            }
            _ => {}
        });

        imports
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Cast(Cast),
    Ident(Ident),
    TypeSpecifier(TypeSpecifier),
    ModuleImport(ModuleImport),
    Assign(Box<Assign>),
    Literal(Literal),
    Prefix(PrefixExpr),
    Infix(InfixExpr),
    Unary(UnaryExpr),
    Array(Array),
    UntypedArray(UntypedArray),
    ArrayIndex(ArrayIndex),
    AddrOf(AddrOf),
    Deref(Deref),
    StructInit(StructInit),
    FuncCall(FuncCall),
    FieldAccess(FieldAccess),
    MethodCall(MethodCall),
    UnnamedStructValue(UnnamedStructValue),
    SizeOf(SizeOf),
    Lambda(Lambda),
    Tuple(TupleValue),
    TupleAccess(TupleAccess),
    Dynamic(Dynamic),
}

#[derive(Debug, Clone)]
pub struct Dynamic {
    pub operand: Box<Expr>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleAccess {
    pub operand: Box<Expr>,
    pub index: usize,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleValue {
    pub expr_list: Vec<Expr>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SizeOf {
    pub expr: Box<Expr>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Deref {
    pub expr: Box<Expr>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AddrOf {
    pub expr: Box<Expr>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructValue {
    pub fields: Vec<UnnamedStructValueField>,
    pub is_packed: bool,
    pub is_const: bool,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructValueField {
    pub field_name: Ident,
    pub field_ty: Option<TypeSpecifier>,
    pub field_value: Box<Expr>,
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
    pub field_name: Ident,
    pub field_ty: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub ident: Ident,
    pub fields: Vec<UnionField>,
    pub generic_params: Option<GenericParamsList>,
    pub methods: Vec<FuncDef>,
    pub impls: Vec<ModuleImport>,
    pub modifiers: UnionModifiers,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnionField {
    pub ident: Ident,
    pub ty: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub ident: Ident,
    pub variants: Vec<EnumVariant>,
    pub generic_params: Option<GenericParamsList>,
    pub methods: Vec<FuncDef>,
    pub impls: Vec<ModuleImport>,
    pub modifiers: EnumModifiers,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Ident(Ident),
    Variant(Ident, Vec<EnumValuedField>),
    Valued(Ident, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct EnumValuedField {
    pub field_ty: TypeSpecifier,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub expr: Box<Expr>,
    pub target_type: TypeSpecifier,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub operand: Box<Expr>,
    pub op: PrefixOperator,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub operand: Box<Expr>,
    pub args: Vec<Expr>,
    pub type_args: Option<TypeArgs>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub is_fat_arrow: bool,
    pub operand: Box<Expr>,
    pub field_name: Ident,
    pub type_args: Option<TypeArgs>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct MethodCall {
    pub is_fat_arrow: bool,
    pub operand: Box<Expr>,
    pub method_name: Ident,
    pub args: Vec<Expr>,
    pub type_args: Option<TypeArgs>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub value: String,
    pub span: Span,
    pub loc: Location,
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl Ident {
    pub fn as_string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ModuleImport {
    pub segments: Vec<ModuleSegment>,
    pub span: Span,
    pub loc: Location,
}

impl ModuleImport {
    pub fn as_identifier(&self) -> Option<Ident> {
        if self.segments.len() == 1 {
            match self.segments.last()? {
                ModuleSegment::SubModule(ident) => Some(ident.clone()),
                ModuleSegment::Single(_) => None,
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    TypeToken(Token),
    Ident(Ident),
    Const(Box<TypeSpecifier>),
    Array(ArrayTypeSpecifier),
    ModuleImport(ModuleImport),
    Deref(Box<TypeSpecifier>),
    UnnamedStruct(UnnamedStructType),
    FuncType(Box<FuncType>),
    Tuple(TupleType),
    GenericInst(GenericInst),
    SelfType(SelfType),
}

impl TypeSpecifier {
    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const(..))
    }

    pub fn as_module_import(&self) -> Option<ModuleImport> {
        match self {
            TypeSpecifier::Ident(ident) => Some(ModuleImport {
                segments: vec![ModuleSegment::SubModule(ident.clone())],
                loc: ident.loc,
                span: ident.span,
            }),
            TypeSpecifier::ModuleImport(module_import) => Some(module_import.clone()),
            _ => None,
        }
    }

    pub fn loc(&self) -> (Location, usize) {
        match self {
            TypeSpecifier::TypeToken(token) => (token.loc.clone(), token.span.end),
            TypeSpecifier::Ident(ident) => (ident.loc.clone(), ident.span.end),
            TypeSpecifier::Const(inner) => inner.loc(),
            TypeSpecifier::Array(array) => array.element_type.loc(),
            TypeSpecifier::ModuleImport(module_import) => (module_import.loc.clone(), module_import.span.end),
            TypeSpecifier::Deref(inner) => inner.loc(),
            TypeSpecifier::UnnamedStruct(struct_type) => (struct_type.loc.clone(), struct_type.span.end),
            TypeSpecifier::FuncType(func_type) => (func_type.loc.clone(), func_type.span.end),
            TypeSpecifier::Tuple(tuple_type) => (tuple_type.loc.clone(), tuple_type.span.end),
            TypeSpecifier::GenericInst(generic_inst) => (generic_inst.loc.clone(), generic_inst.span.end),
            TypeSpecifier::SelfType(self_type) => (self_type.loc.clone(), self_type.span.end),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SelfType {
    pub loc: Location,
    pub span: Span,
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
    Fixed(Box<Expr>),
    Dynamic,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operand: Box<Expr>,
    pub op: UnaryOperator,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params: FuncTypeParams,
    pub return_type: Box<TypeSpecifier>,
    pub vis_opt: Option<Visibility>,
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
pub struct InfixExpr {
    pub op: InfixOperator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub data_type: TypeSpecifier,
    pub elements: Vec<Expr>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct UntypedArray {
    pub elements: Vec<Expr>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct ArrayIndex {
    pub operand: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Import(Import),
    Variable(Variable),
    ExportTuple(ExportTuple),
    Expr(Expr),
    If(If),
    Return(Return),
    FuncDef(FuncDef),
    FuncDecl(FuncDecl),
    For(For),
    While(While),
    Foreach(Foreach),
    Switch(Switch),
    BlockStmt(BlockStmt),
    Interface(Interface),
    Struct(Struct),
    Enum(Enum),
    Union(Union),
    Break(Break),
    Continue(Continue),
    Typedef(Typedef),
    GlobalVar(GlobalVar),
    Defer(Defer),
    Label(Label),
    Goto(Goto),
}

#[derive(Debug, Clone)]
pub struct Goto {
    pub name: Ident,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub name: Ident,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Defer {
    pub operand: Box<Stmt>,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Interface {
    pub ident: Ident,
    pub methods: Vec<FuncDecl>,
    pub vis: Visibility,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub ident: Ident,
    pub type_specifier: Option<TypeSpecifier>,
    pub expr: Option<Expr>,
    pub is_const: bool,
    pub modifiers: GlobalVarModifiers,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Typedef {
    pub ident: Ident,
    pub type_specifier: TypeSpecifier,
    pub generic_params: Option<GenericParamsList>,
    pub vis: Visibility,
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
    pub argument: Option<Expr>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ModuleSegmentSingle {
    pub ident: Ident,
    pub renamed: Option<Ident>,
}

#[derive(Debug, Clone)]
pub enum ModuleSegment {
    SubModule(Ident),
    Single(Vec<ModuleSegmentSingle>),
}

impl Hash for ModuleSegmentSingle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
        self.renamed.hash(state);
    }
}

impl ModuleSegment {
    pub fn as_identifier_opt(&self) -> Option<Ident> {
        match self {
            ModuleSegment::SubModule(ident) => Some(ident.clone()),
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
            if let ModuleSegment::SubModule(ident) = segment {
                ident.value.hash(state);
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
                if let ModuleSegment::SubModule(ident) = segment {
                    Some(&ident.value)
                } else {
                    None
                }
            })
            .collect();

        let other_submodules: Vec<&String> = other
            .segments
            .iter()
            .filter_map(|segment| {
                if let ModuleSegment::SubModule(ident) = segment {
                    Some(&ident.value)
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
            span: self.span,
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
    pub ident: Ident,
    pub generic_params: Option<GenericParamsList>,
    pub impls: Vec<ModuleImport>,
    pub fields: Vec<StructField>,
    pub methods: Vec<FuncDef>,
    pub modifiers: StructModifiers,
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
    pub ident: Ident,
    pub vis: Visibility,
    pub ty: TypeSpecifier,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldInit {
    pub ident: Ident,
    pub value: Expr,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expr,
    pub body: Box<BlockStmt>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct For {
    pub initializer: Option<Variable>,
    pub condition: Option<Expr>,
    pub increment: Option<Expr>,
    pub body: Box<BlockStmt>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Foreach {
    pub item: Ident,
    pub index: Option<Ident>,
    pub expr: Expr,
    pub body: Box<BlockStmt>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Switch {
    pub operand: Expr,
    pub cases: Vec<SwitchCase>,
    pub default_case: Option<BlockStmt>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub patterns: Vec<SwitchCasePattern>,
    pub body: BlockStmt,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum SwitchCasePattern {
    Expr(Expr),
    Range(Range),
    Ident(Ident),
    EnumVariant(Ident, Vec<Ident>),
}

#[derive(Debug, Clone)]
pub struct Range {
    pub lower: Expr,
    pub upper: Expr,
    pub inclusive_upper: bool,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: FuncParams,
    pub body: Box<BlockStmt>,
    pub return_type: TypeSpecifier,
    pub inline: bool,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub ident: Ident,
    pub generic_params: Option<GenericParamsList>,
    pub params: FuncParams,
    pub body: Box<BlockStmt>,
    pub return_type: Option<TypeSpecifier>,
    pub modifiers: FuncModifiers,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub ident: Ident,
    pub generic_params: Option<GenericParamsList>,
    pub params: FuncParams,
    pub return_type: Option<TypeSpecifier>,
    pub modifiers: FuncModifiers,
    pub renamed_as: Option<Ident>,
    pub span: Span,
    pub loc: Location,
}

impl FuncDef {
    pub fn as_func_decl(&self) -> FuncDecl {
        FuncDecl {
            ident: self.ident.clone(),
            generic_params: self.generic_params.clone(),
            params: self.params.clone(),
            return_type: self.return_type.clone(),
            modifiers: self.modifiers.clone(),
            renamed_as: None,
            span: self.span,
            loc: self.loc.clone(),
        }
    }
}

impl FuncDecl {
    /// Returns the function's effective name, preferring the renamed version if available.
    pub fn usable_name(&self) -> String {
        match &self.renamed_as {
            Some(ident) => ident.value.clone(),
            None => self.ident.value.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub exprs: Vec<Stmt>,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub ident: Ident,
    pub ty: Option<TypeSpecifier>,
    pub rhs: Option<Expr>,
    pub is_const: bool,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct ExportTuple {
    pub pattern: ExportPattern,
    pub ty: Option<TypeSpecifier>,
    pub rhs: Option<Expr>,
    pub is_const: bool,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum ExportPattern {
    Ident(Ident),
    Tuple(Vec<ExportPattern>),
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs: Expr,
    pub rhs: Expr,
    pub kind: AssignKind,
    pub span: Span,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignKind {
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

impl AssignKind {
    pub fn to_infix_operator(&self) -> InfixOperator {
        match self {
            AssignKind::Default => unreachable!(),
            AssignKind::AddAssign => InfixOperator::Add,
            AssignKind::SubAssign => InfixOperator::Sub,
            AssignKind::MulAssign => InfixOperator::Mul,
            AssignKind::DivAssign => InfixOperator::Div,
            AssignKind::ModAssign => InfixOperator::Rem,
            AssignKind::BitwiseAndAssign => InfixOperator::BitwiseAnd,
            AssignKind::BitwiseXorAssign => InfixOperator::BitwiseXor,
            AssignKind::BitwiseAndNotAssign => InfixOperator::BitwiseAndNot,
            AssignKind::LeftShiftAssign => InfixOperator::ShiftLeft,
            AssignKind::RightShiftAssign => InfixOperator::ShiftRight,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SelfModifier {
    pub kind: SelfModifierKind,
    pub loc: Location,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub ident: Ident,
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
    Typed(Ident, TypeSpecifier),
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expr,
    pub consequent: Box<BlockStmt>,
    pub branches: Vec<If>,
    pub alternate: Option<Box<BlockStmt>>,
    pub span: Span,
    pub loc: Location,
}

impl Stmt {
    pub fn loc(&self) -> Location {
        match self {
            Stmt::Interface(interface) => interface.loc.clone(),
            Stmt::Variable(variable) => variable.loc.clone(),
            Stmt::ExportTuple(export_tuple_values) => export_tuple_values.loc.clone(),
            Stmt::If(if_stmt) => if_stmt.loc.clone(),
            Stmt::Return(ret) => ret.loc.clone(),
            Stmt::FuncDef(func_def) => func_def.loc.clone(),
            Stmt::FuncDecl(func_decl) => func_decl.loc.clone(),
            Stmt::For(for_stmt) => for_stmt.loc.clone(),
            Stmt::While(while_stmt) => while_stmt.loc.clone(),
            Stmt::Foreach(foreach) => foreach.loc.clone(),
            Stmt::Switch(switch) => switch.loc.clone(),
            Stmt::Struct(struct_stmt) => struct_stmt.loc.clone(),
            Stmt::Import(import) => import.loc.clone(),
            Stmt::BlockStmt(block_stmt) => block_stmt.loc.clone(),
            Stmt::Enum(enum_stmt) => enum_stmt.loc.clone(),
            Stmt::Union(union) => union.loc.clone(),
            Stmt::Break(brk) => brk.loc.clone(),
            Stmt::Continue(cont) => cont.loc.clone(),
            Stmt::Typedef(typedef) => typedef.loc.clone(),
            Stmt::GlobalVar(global_variable) => global_variable.loc.clone(),
            Stmt::Defer(defer) => defer.loc.clone(),
            Stmt::Label(label) => label.loc.clone(),
            Stmt::Goto(goto) => goto.loc.clone(),
            Stmt::Expr(..) => unreachable!(),
        }
    }
}

pub type GenericParamsList = Vec<GenericParam>;

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub param_name: Ident,
    pub bounds: Option<Vec<Bound>>,
    pub default: Option<TypeSpecifier>,
}

#[derive(Debug, Clone)]
pub struct Bound {
    pub symbol: Ident,
    pub type_args: Vec<TypeArg>,
}

#[derive(Debug, Clone)]
pub enum TypeArg {
    Positional(TypeSpecifier),
    Named { key: Ident, ty: TypeSpecifier },
}

pub type TypeArgs = Vec<TypeArg>;
