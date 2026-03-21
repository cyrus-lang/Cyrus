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

use crate::abi::{ReprAttr, Visibility};
use crate::modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers};
use crate::operators::{InfixOperator, PrefixOperator, UnaryOperator};
use cyrusc_source_loc::Loc;
use cyrusc_tokens::TokenKind;
use cyrusc_tokens::{Token, literals::Literal};
use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

pub mod abi;
pub mod format;
pub mod modifiers;
pub mod operators;

#[derive(Debug)]
pub struct ProgramTree {
    pub body: Rc<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
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
    Builtin(Builtin),
    FieldAccess(FieldAccess),
    MethodCall(MethodCall),
    Lambda(Lambda),
    Tuple(TupleValue),
    TupleAccess(TupleAccess),
    Dynamic(Dynamic),
    UnnamedStructValue(UnnamedStructValue),
    UnnamedUnionValue(UnnamedUnionValue),
    UnnamedEnumValue(UnnamedEnumValue),
}

#[derive(Debug, Clone)]
pub struct Dynamic {
    pub operand: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TupleAccess {
    pub operand: Box<Expr>,
    pub index: usize,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TupleValue {
    pub elements: Vec<Expr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Deref {
    pub expr: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct AddrOf {
    pub expr: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructValue {
    pub fields: Vec<UnnamedStructValueField>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedUnionValue {
    pub field_name: Ident,
    pub field_value: Box<Expr>,
    pub is_const: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedEnumValue {
    pub ident: Ident,
    pub kind: UnnamedEnumValueKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum UnnamedEnumValueKind {
    Plain,
    Fielded(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct UnnamedStructValueField {
    pub field_name: Ident,
    pub field_ty: Option<TypeSpecifier>,
    pub field_value: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructType {
    pub fields: Vec<UnnamedStructTypeField>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedStructTypeField {
    pub field_name: Ident,
    pub field_ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedUnionType {
    pub fields: Vec<UnnamedUnionTypeField>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedUnionTypeField {
    pub field_name: Ident,
    pub field_ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub ident: Ident,
    pub fields: Vec<UnionField>,
    pub generic_params: Option<GenericParamsList>,
    pub methods: Vec<FuncDef>,
    pub impls: Vec<TypeSpecifier>,
    pub modifiers: UnionModifiers,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnionField {
    pub ident: Ident,
    pub ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub ident: Ident,
    pub variants: Vec<EnumVariant>,
    pub generic_params: Option<GenericParamsList>,
    pub methods: Vec<FuncDef>,
    pub impls: Vec<TypeSpecifier>,
    pub modifiers: EnumModifiers,
    pub tag_type: Option<TypeSpecifier>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UnnamedEnumType {
    pub variants: Vec<UnnamedEnumVariant>,
    pub tag_type: Option<Box<TypeSpecifier>>,
    pub repr_attr: Option<ReprAttr>,
    pub align: Option<usize>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnnamedEnumVariant {
    Ident(Ident),
    Variant(Ident, Vec<UnnamedEnumValuedField>),
    Valued(Ident, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct UnnamedEnumValuedField {
    pub ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Ident(Ident),
    Variant(Ident, Vec<EnumValuedField>),
    Valued(Ident, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct EnumValuedField {
    pub ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrefixExpr {
    pub operand: Box<Expr>,
    pub op: PrefixOperator,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Builtin {
    BuiltinFunc(BuiltinFunc),
    BuiltinScope(BuiltinScope),
}

#[derive(Debug, Clone)]
pub struct BuiltinFunc {
    pub name: Ident,
    pub args: Vec<Expr>,
    pub child_stmt: Option<Box<Stmt>>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct BuiltinScope {
    pub name: Ident,
    pub args: Vec<Expr>,
    pub block: Box<BlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncCall {
    pub operand: Box<Expr>,
    pub args: Vec<Expr>,
    pub type_args: Option<TypeArgs>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccess {
    pub is_fat_arrow: bool,
    pub operand: Box<Expr>,
    pub field_name: Ident,
    pub type_args: Option<TypeArgs>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodCall {
    pub is_fat_arrow: bool,
    pub operand: Box<Expr>,
    pub method_name: Ident,
    pub args: Vec<Expr>,
    pub type_args: Option<TypeArgs>,
    pub loc: Loc,
}

#[derive(Debug, Clone, Eq)]
pub struct Ident {
    pub value: String,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ModuleImport {
    pub segments: Vec<ModuleSegment>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSpecifier {
    TypeToken(Token),
    Ident(Ident),
    Const(Box<TypeSpecifier>),
    Array(ArrayType),
    ModuleImport(ModuleImport),
    Deref(Box<TypeSpecifier>),
    UnnamedStruct(UnnamedStructType),
    UnnamedUnion(UnnamedUnionType),
    UnnamedEnum(UnnamedEnumType),
    FuncType(Box<FuncType>),
    Tuple(TupleType),
    GenericInst(GenericInst),
    SelfType(SelfType),
}

#[derive(Debug, Clone)]
pub struct SelfType {
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct GenericInst {
    pub base: Box<TypeSpecifier>,
    pub type_args: TypeArgs,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TupleType {
    pub type_list: Vec<TypeSpecifier>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub size: ArrayCapacity,
    pub element_type: Box<TypeSpecifier>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrayCapacity {
    Fixed(Box<Expr>),
    Dynamic,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operand: Box<Expr>,
    pub op: UnaryOperator,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params: FuncTypeParams,
    pub return_type: Box<TypeSpecifier>,
    pub vis_opt: Option<Visibility>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncTypeParams {
    pub list: Vec<TypeSpecifier>,
    pub variadic: Option<FuncTypeVariadicParams>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuncTypeVariadicParams {
    UntypedCStyle,
    Typed(TypeSpecifier),
}

#[derive(Debug, Clone)]
pub struct InfixExpr {
    pub op: InfixOperator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub data_type: TypeSpecifier,
    pub elements: Vec<Expr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct UntypedArray {
    pub elements: Vec<Expr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ArrayIndex {
    pub operand: Box<Expr>,
    pub index: Box<Expr>,
    pub loc: Loc,
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
    Builtin(Builtin),
}

#[derive(Debug, Clone)]
pub struct Goto {
    pub name: Ident,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub name: Ident,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Defer {
    pub operand: Box<Stmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Interface {
    pub ident: Ident,
    pub methods: Vec<FuncDecl>,
    pub generic_params: Option<GenericParamsList>,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub ident: Ident,
    pub type_specifier: Option<TypeSpecifier>,
    pub expr: Option<Expr>,
    pub is_const: bool,
    pub modifiers: GlobalVarModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Typedef {
    pub ident: Ident,
    pub type_specifier: TypeSpecifier,
    pub generic_params: Option<GenericParamsList>,
    pub vis: Visibility,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Break {
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Continue {
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub argument: Option<Expr>,

    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleSegmentSingle {
    pub ident: Ident,
    pub renamed: Option<Ident>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleSegment {
    SubModule(Ident),
    Single(Vec<ModuleSegmentSingle>),
}

#[derive(Debug, Clone)]
pub struct ModulePath {
    pub alias: Option<String>,
    pub segments: Vec<ModuleSegment>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub paths: Vec<ModulePath>,

    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub ident: Ident,
    pub generic_params: Option<GenericParamsList>,
    pub impls: Vec<TypeSpecifier>,
    pub fields: Vec<StructField>,
    pub methods: Vec<FuncDef>,
    pub modifiers: StructModifiers,
    pub align: Option<usize>,
    pub is_packed: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct StructInit {
    pub struct_name: ModuleImport,
    pub field_inits: Vec<FieldInit>,
    pub type_args: Option<TypeArgs>,
    pub is_const: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub ident: Ident,
    pub vis: Visibility,
    pub ty: TypeSpecifier,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct FieldInit {
    pub ident: Ident,
    pub value: Expr,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expr,
    pub body: Box<BlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct For {
    pub initializer: Option<Variable>,
    pub condition: Option<Expr>,
    pub increment: Option<Expr>,
    pub body: Box<BlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Foreach {
    pub item: Ident,
    pub index: Option<Ident>,
    pub expr: Expr,
    pub body: Box<BlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Switch {
    pub operand: Expr,
    pub cases: Vec<SwitchCase>,
    pub default_case: Option<BlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub patterns: Vec<SwitchCasePattern>,
    pub body: BlockStmt,
    pub loc: Loc,
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
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: FuncParams,
    pub body: Box<BlockStmt>,
    pub return_type: TypeSpecifier,
    pub inline: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub ident: Ident,
    pub generic_params: Option<GenericParamsList>,
    pub params: FuncParams,
    pub body: Box<BlockStmt>,
    pub return_type: Option<TypeSpecifier>,
    pub modifiers: FuncModifiers,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub ident: Ident,
    pub generic_params: Option<GenericParamsList>,
    pub params: FuncParams,
    pub return_type: Option<TypeSpecifier>,
    pub modifiers: FuncModifiers,
    pub renamed_as: Option<Ident>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub exprs: Vec<Stmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub ident: Ident,
    pub ty: Option<TypeSpecifier>,
    pub rhs: Option<Expr>,
    pub is_const: bool,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ExportTuple {
    pub pattern: ExportPattern,
    pub ty: Option<TypeSpecifier>,
    pub rhs: Option<Expr>,
    pub is_const: bool,
    pub loc: Loc,
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
    pub loc: Loc,
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

#[derive(Debug, Clone)]
pub struct SelfModifier {
    pub kind: SelfModifierKind,
    pub loc: Loc,
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

    pub loc: Loc,
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

    pub loc: Loc,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeArg {
    Positional(TypeSpecifier),
    Named { key: Ident, ty: TypeSpecifier },
}

pub type TypeArgs = Vec<TypeArg>;

// Returns the provided return type, or constructs a default `void` type.
///
/// This utility is used by the parser when building the raw AST to ensure
/// that functions without an explicit return type are assigned a `void`
/// return type. The resulting `TypeSpecifier` preserves the provided
/// source location and span for accurate diagnostics.
pub fn return_type_or_default_void(return_type: Option<TypeSpecifier>, loc: Loc) -> TypeSpecifier {
    return_type.unwrap_or(TypeSpecifier::TypeToken(Token {
        kind: TokenKind::Void,
        loc,
    }))
}

impl ModuleSegment {
    pub fn as_identifier_opt(&self) -> Option<Ident> {
        match self {
            ModuleSegment::SubModule(ident) => Some(ident.clone()),
            ModuleSegment::Single(_) => None,
        }
    }
}

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
            loc: self.loc.clone(),
        }
    }
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

impl Stmt {
    #[inline]
    pub fn decl_name(&self) -> Option<&Ident> {
        match self {
            Stmt::Variable(variable) => Some(&variable.ident),
            Stmt::FuncDef(func_def) => Some(&func_def.ident),
            Stmt::FuncDecl(func_decl) => Some(&func_decl.ident),
            Stmt::Interface(interface) => Some(&interface.ident),
            Stmt::Struct(struct_) => Some(&struct_.ident),
            Stmt::Enum(enum_) => Some(&enum_.ident),
            Stmt::Union(union_) => Some(&union_.ident),
            Stmt::Typedef(typedef) => Some(&typedef.ident),
            Stmt::GlobalVar(global_var) => Some(&global_var.ident),

            Stmt::Import(_)
            | Stmt::ExportTuple(_)
            | Stmt::Expr(_)
            | Stmt::If(_)
            | Stmt::Return(_)
            | Stmt::For(_)
            | Stmt::While(_)
            | Stmt::Foreach(_)
            | Stmt::Switch(_)
            | Stmt::BlockStmt(_)
            | Stmt::Break(_)
            | Stmt::Continue(_)
            | Stmt::Defer(_)
            | Stmt::Label(_)
            | Stmt::Goto(_)
            | Stmt::Builtin(_) => None,
        }
    }

    pub fn loc(&self) -> Loc {
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
            Stmt::Builtin(builtin) => match builtin {
                Builtin::BuiltinFunc(builtin_func) => builtin_func.loc.clone(),
                Builtin::BuiltinScope(builtin_scope) => builtin_scope.loc.clone(),
            },
            Stmt::Expr(..) => unreachable!(),
        }
    }
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

impl Ident {
    pub fn as_string(&self) -> String {
        self.value.clone()
    }
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

impl TypeSpecifier {
    pub fn as_type_token(&self) -> Option<&Token> {
        match self {
            TypeSpecifier::TypeToken(token) => Some(token),
            _ => None,
        }
    }

    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const(..))
    }

    pub fn as_module_import(&self) -> Option<ModuleImport> {
        match self {
            TypeSpecifier::Ident(ident) => Some(ModuleImport {
                segments: vec![ModuleSegment::SubModule(ident.clone())],
                loc: ident.loc,
            }),
            TypeSpecifier::ModuleImport(module_import) => Some(module_import.clone()),
            _ => None,
        }
    }

    pub fn loc(&self) -> Loc {
        match self {
            TypeSpecifier::TypeToken(token) => token.loc,
            TypeSpecifier::Ident(ident) => ident.loc,
            TypeSpecifier::Const(inner) => inner.loc(),
            TypeSpecifier::Array(array) => array.loc,
            TypeSpecifier::ModuleImport(module_import) => module_import.loc,
            TypeSpecifier::Deref(type_specifier) => type_specifier.loc(),
            TypeSpecifier::UnnamedStruct(unnamed_struct_type) => unnamed_struct_type.loc,
            TypeSpecifier::UnnamedUnion(unnamed_union_type) => unnamed_union_type.loc,
            TypeSpecifier::UnnamedEnum(unnamed_enum_type) => unnamed_enum_type.loc,
            TypeSpecifier::FuncType(func_type) => func_type.loc,
            TypeSpecifier::Tuple(tuple_type) => tuple_type.loc,
            TypeSpecifier::GenericInst(generic_inst) => generic_inst.loc,
            TypeSpecifier::SelfType(self_type) => self_type.loc,
        }
    }
}

impl Builtin {
    pub fn loc(&self) -> Loc {
        match self {
            Builtin::BuiltinFunc(builtin_func) => builtin_func.loc,
            Builtin::BuiltinScope(builtin_scope) => builtin_scope.loc,
        }
    }
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

impl Hash for ModuleSegmentSingle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
        self.renamed.hash(state);
    }
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

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl PartialEq for ModuleImport {
    fn eq(&self, other: &Self) -> bool {
        self.segments == other.segments
    }
}

impl PartialEq for UnnamedStructType {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields && self.repr_attr == other.repr_attr
    }
}

impl PartialEq for UnnamedStructTypeField {
    fn eq(&self, other: &Self) -> bool {
        self.field_name == other.field_name && self.field_ty == other.field_ty
    }
}

impl PartialEq for UnnamedUnionType {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields
    }
}

impl PartialEq for UnnamedUnionTypeField {
    fn eq(&self, other: &Self) -> bool {
        self.field_name == other.field_name && self.field_ty == other.field_ty
    }
}

impl PartialEq for FuncType {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.return_type == other.return_type
    }
}

impl PartialEq for TupleType {
    fn eq(&self, other: &Self) -> bool {
        self.type_list == other.type_list
    }
}

impl PartialEq for UnnamedEnumType {
    fn eq(&self, other: &Self) -> bool {
        self.variants == other.variants
    }
}

impl PartialEq for UnnamedEnumValuedField {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl PartialEq for GenericInst {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base && self.type_args == other.type_args
    }
}

impl PartialEq for SelfType {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl PartialEq for Assign {
    fn eq(&self, other: &Self) -> bool {
        self.lhs == other.lhs && self.rhs == other.rhs && self.kind == other.kind
    }
}

impl PartialEq for InfixExpr {
    fn eq(&self, other: &Self) -> bool {
        self.op == other.op && self.lhs == other.lhs && self.rhs == other.rhs
    }
}

impl PartialEq for UnaryExpr {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand && self.op == other.op
    }
}

impl PartialEq for Array {
    fn eq(&self, other: &Self) -> bool {
        self.data_type == other.data_type && self.elements == other.elements
    }
}

impl PartialEq for UntypedArray {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl PartialEq for ArrayIndex {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand && self.index == other.index
    }
}

impl PartialEq for AddrOf {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl PartialEq for Deref {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl PartialEq for StructInit {
    fn eq(&self, other: &Self) -> bool {
        self.struct_name == other.struct_name
            && self.field_inits == other.field_inits
            && self.type_args == other.type_args
            && self.is_const == other.is_const
    }
}

impl PartialEq for FieldInit {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.value == other.value
    }
}

impl PartialEq for TupleValue {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl PartialEq for TupleAccess {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand && self.index == other.index
    }
}

impl PartialEq for Dynamic {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand
    }
}

impl PartialEq for UnnamedStructValue {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields && self.repr_attr == other.repr_attr && self.align == other.align
    }
}

impl PartialEq for UnnamedUnionValue {
    fn eq(&self, other: &Self) -> bool {
        self.field_name == other.field_name && self.field_value == other.field_value && self.is_const == other.is_const
    }
}

impl PartialEq for UnnamedStructValueField {
    fn eq(&self, other: &Self) -> bool {
        self.field_name == other.field_name && self.field_ty == other.field_ty && self.field_value == other.field_value
    }
}

impl PartialEq for UnnamedEnumValue {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.kind == other.kind
    }
}

impl PartialEq for UnnamedEnumValueKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (UnnamedEnumValueKind::Plain, UnnamedEnumValueKind::Plain) => true,
            (UnnamedEnumValueKind::Fielded(exprs1), UnnamedEnumValueKind::Fielded(exprs2)) => exprs1 == exprs2,
            _ => false,
        }
    }
}

impl PartialEq for Lambda {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.return_type == other.return_type && self.inline == other.inline
    }
}

impl PartialEq for FuncParam {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.ty == other.ty
    }
}

impl PartialEq for FuncParams {
    fn eq(&self, other: &Self) -> bool {
        self.list == other.list && self.variadic == other.variadic
    }
}

impl PartialEq for FuncParamKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::FuncParam(func_param1), Self::FuncParam(func_param2)) => func_param1 == func_param2,
            (Self::SelfModifier(self_modifier1), Self::SelfModifier(self_modifier2)) => {
                self_modifier1 == self_modifier2
            }
            _ => false,
        }
    }
}

impl PartialEq for SelfModifier {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl PartialEq for FuncVariadicParams {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FuncVariadicParams::UntypedCStyle, FuncVariadicParams::UntypedCStyle) => true,
            (
                FuncVariadicParams::Typed(ident1, type_specifier1),
                FuncVariadicParams::Typed(ident2, type_specifier2),
            ) => ident1 == ident2 && type_specifier1 == type_specifier2,
            _ => false,
        }
    }
}

impl PartialEq for BuiltinScope {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args
    }
}

impl PartialEq for BuiltinFunc {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args && self.child_stmt.is_some() == other.child_stmt.is_some()
    }
}

impl Eq for BuiltinFunc {}
impl Eq for BuiltinScope {}
impl Eq for Array {}
impl Eq for UntypedArray {}
impl Eq for ArrayIndex {}
impl Eq for AddrOf {}
impl Eq for Deref {}
impl Eq for StructInit {}
impl Eq for Lambda {}
impl Eq for TupleValue {}
impl Eq for TupleAccess {}
impl Eq for Dynamic {}
impl Eq for UnnamedStructValue {}
impl Eq for UnnamedUnionValue {}
impl Eq for UnnamedEnumValue {}
impl Eq for UnaryExpr {}
impl Eq for InfixExpr {}
impl Eq for Assign {}
impl Eq for ModuleImport {}
impl Eq for ModulePath {}
impl Eq for UnnamedStructType {}
impl Eq for UnnamedStructTypeField {}
impl Eq for UnnamedUnionType {}
impl Eq for UnnamedUnionTypeField {}
impl Eq for FuncType {}
impl Eq for TupleType {}
impl Eq for UnnamedEnumType {}
impl Eq for UnnamedEnumValuedField {}
impl Eq for GenericInst {}
impl Eq for SelfType {}
