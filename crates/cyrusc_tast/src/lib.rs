use crate::types::{SemanticType, TypedUStructType};
use ast::{
    AccessSpecifier, AssignmentKind, Identifier, LiteralKind, SelfModifierKind,
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
    source_loc::SourceLoc,
};
use std::{collections::HashMap, hash::Hash};

pub mod format;
pub mod types;

pub type ScopeID = u32;
pub type SymbolID = u32;
pub type ModuleID = u64;

#[derive(Debug)]
pub struct TypedProgramTree {
    pub body: Vec<TypedStmt>,
}

// Expressions

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExprStmt {
    pub kind: TypedExprKind,
    pub sema_ty: Option<SemanticType>,
    pub vcat: ValueCategory,
    pub loc: SourceLoc,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueCategory {
    LValue,
    RValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExprKind {
    Symbol(SymbolID, SourceLoc),
    Literal(TypedLiteralExpr),
    Prefix(TypedPrefixExpr),
    Infix(TypedInfixExpr),
    Unary(TypedUnaryExpr),
    Assign(TypedAssignExpr),
    Cast(TypedCastExpr),
    AddrOf(TypedAddrOfExpr),
    Deref(TypedDerefExpr),
    Array(TypedArrayExpr),
    ArrayIndex(TypedArrayIndexExpr),
    StructInit(TypedStructInitExpr),
    UStructValue(TypedUStructValue),
    FuncCall(TypedFuncCall),
    MethodCall(TypedMethodCall),
    FieldAccess(TypedFieldAccess),
    SizeOf(TypedSizeOfExpr),
    Lambda(TypedLambdaExpr),
    Tuple(TypedTupleExpr),
    TupleAccess(TypedTupleAccessExpr),
    SemanticType(SemanticType),
}

#[derive(Debug, Clone)]
pub struct TypedTupleAccessExpr {
    pub operand: Box<TypedExprStmt>,
    pub index: usize,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedTupleExpr {
    pub expr_list: Vec<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedLambdaExpr {
    pub params: TypedFuncParams,
    pub body: Box<TypedBlockStmt>,
    pub return_type: SemanticType,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedSizeOfExpr {
    pub operand: Box<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedLiteralExpr {
    pub ty: Option<SemanticType>,
    pub kind: LiteralKind,
    pub loc: SourceLoc,
}

impl TypedLiteralExpr {
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

impl TypedExprStmt {
    pub fn is_lvalue(&self) -> bool {
        self.vcat == ValueCategory::LValue
    }

    pub fn is_rvalue(&self) -> bool {
        self.vcat == ValueCategory::RValue
    }
}

impl TypedExprKind {
    pub fn is_comptime_valid(&self) -> bool {
        match self {
            TypedExprKind::Literal(_) => true,
            TypedExprKind::Lambda(_) => true,
            TypedExprKind::Tuple(tuple_value) => {
                let mut comptime_valid = true;

                for expr in &tuple_value.expr_list {
                    if !expr.kind.is_comptime_valid() {
                        comptime_valid = false;
                        break;
                    }
                }

                comptime_valid
            }
            TypedExprKind::Prefix(prefix) => prefix.operand.kind.is_comptime_valid(),
            TypedExprKind::Infix(infix) => infix.lhs.kind.is_comptime_valid() && infix.rhs.kind.is_comptime_valid(),
            TypedExprKind::Unary(unary) => unary.operand.kind.is_comptime_valid(),
            TypedExprKind::Cast(cast) => cast.operand.kind.is_comptime_valid(),
            TypedExprKind::Array(typed_array) => typed_array
                .elements
                .iter()
                .all(|typed_expr| typed_expr.kind.is_comptime_valid()),
            TypedExprKind::StructInit(typed_struct_init) => typed_struct_init
                .fields
                .iter()
                .all(|field_init| field_init.value.kind.is_comptime_valid()),
            TypedExprKind::UStructValue(typed_unnamed_struct_value) => {
                typed_unnamed_struct_value
                    .fields
                    .iter()
                    .all(|typed_unnamed_struct_value_field| {
                        typed_unnamed_struct_value_field.field_value.kind.is_comptime_valid()
                    })
            }
            TypedExprKind::Symbol(..)
            | TypedExprKind::ArrayIndex(_)
            | TypedExprKind::TupleAccess(_)
            | TypedExprKind::Deref(_)
            | TypedExprKind::FieldAccess(_)
            | TypedExprKind::MethodCall(_)
            | TypedExprKind::FuncCall(_)
            | TypedExprKind::Assign(_)
            | TypedExprKind::SizeOf(_)
            | TypedExprKind::SemanticType(_)
            | TypedExprKind::AddrOf(_) => false,
        }
    }

    pub fn is_lvalue(&self) -> bool {
        match self {
            TypedExprKind::Symbol(..) => true,
            TypedExprKind::ArrayIndex(_) => true,
            TypedExprKind::Deref(_) => true,
            TypedExprKind::FieldAccess(_) => true,
            TypedExprKind::TupleAccess(_) => true,
            TypedExprKind::MethodCall(_) => false,
            TypedExprKind::FuncCall(_) => false,
            TypedExprKind::StructInit(_) => false,
            TypedExprKind::UStructValue(_) => false,
            TypedExprKind::Literal(_) => false,
            TypedExprKind::Prefix(_) => false,
            TypedExprKind::Infix(_) => false,
            TypedExprKind::Unary(_) => false,
            TypedExprKind::Assign(_) => false,
            TypedExprKind::Cast(_) => false,
            TypedExprKind::AddrOf(_) => false,
            TypedExprKind::Array(_) => false,
            TypedExprKind::SizeOf(_) => false,
            TypedExprKind::SemanticType(_) => false,
            TypedExprKind::Lambda(_) => false,
            TypedExprKind::Tuple(_) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedIdentifier {
    pub name: String,
    pub symbol_id: SymbolID,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedPrefixExpr {
    pub op: PrefixOperator,
    pub operand: Box<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUnaryExpr {
    pub operand: Box<TypedExprStmt>,
    pub op: UnaryOperator,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedInfixExpr {
    pub op: InfixOperator,
    pub lhs: Box<TypedExprStmt>,
    pub rhs: Box<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedAssignExpr {
    pub lhs: Box<TypedExprStmt>,
    pub rhs: Box<TypedExprStmt>,
    pub kind: AssignmentKind,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedCastExpr {
    pub operand: Box<TypedExprStmt>,
    pub target_type: SemanticType,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArrayExpr {
    pub array_type: SemanticType,
    pub elements: Vec<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArrayIndexExpr {
    pub operand: Box<TypedExprStmt>,
    pub index: Box<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedAddrOfExpr {
    pub operand: Box<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedDerefExpr {
    pub operand: Box<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructInitExpr {
    pub symbol_id: SymbolID,
    pub type_args: Option<TypedTypeArgs>,
    pub fields: Vec<TypedStructFieldInit>,
    pub is_const: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructFieldInit {
    pub name: String,
    pub value: TypedExprStmt,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFuncCall {
    pub operand: Box<TypedExprStmt>,
    pub args: Vec<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFieldAccess {
    pub operand: Box<TypedExprStmt>,
    pub object_symbol_id: Option<SymbolID>,
    pub field_name: String,
    pub field_index: Option<usize>,
    pub field_ty: Option<SemanticType>,
    pub type_args: Option<TypedTypeArgs>,
    pub is_fat_arrow: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedMethodCall {
    pub operand: Box<TypedExprStmt>,
    pub object_symbol_id: Option<SymbolID>,
    pub method_name: String,
    pub args: Vec<TypedExprStmt>,
    pub type_args: Option<TypedTypeArgs>,
    pub is_fat_arrow: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUStructValue {
    pub fields: Vec<TypedUStructValueField>,
    pub unnamed_struct_type: Option<TypedUStructType>,
    pub packed: bool,
    pub is_const: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUStructValueField {
    pub field_name: String,
    pub field_type: Option<SemanticType>,
    pub field_value: Box<TypedExprStmt>,
    pub loc: SourceLoc,
}

// Statements

#[derive(Debug, Clone)]
pub enum TypedStmt {
    Variable(TypedVarStmt),
    Typedef(TypedTypedefStmt),
    GlobalVariable(TypedGlobalVarStmt),
    FuncDef(TypedFuncDefStmt),
    FuncDecl(TypedFuncDeclStmt),
    BlockStatement(TypedBlockStmt),
    If(TypedIfStmt),
    Return(TypedReturnStmt),
    Break(TypedBreakStmt),
    Continue(TypedContinueStmt),
    For(TypedForStmt),
    While(TypedWhileStmt),
    Switch(TypedSwitchStmt),
    Struct(TypedStructStmt),
    Enum(TypedEnumStmt),
    Union(TypedUnionStmt),
    Interface(TypedInterfaceStmt),
    Expression(TypedExprStmt),
    Defer(TypedDeferStmt),
    ExportTuple(TypedExportTupleStmt),
}

impl TypedStmt {
    pub fn get_loc(&self) -> SourceLoc {
        match self {
            TypedStmt::Variable(typed_variable) => typed_variable.loc.clone(),
            TypedStmt::Typedef(typed_typedef) => typed_typedef.loc.clone(),
            TypedStmt::GlobalVariable(typed_global_variable) => typed_global_variable.loc.clone(),
            TypedStmt::FuncDef(typed_func_def) => typed_func_def.loc.clone(),
            TypedStmt::FuncDecl(typed_func_decl) => typed_func_decl.loc.clone(),
            TypedStmt::BlockStatement(typed_block_statement) => typed_block_statement.loc.clone(),
            TypedStmt::If(typed_if) => typed_if.loc.clone(),
            TypedStmt::Return(typed_return) => typed_return.loc.clone(),
            TypedStmt::Break(typed_break) => typed_break.loc.clone(),
            TypedStmt::Continue(typed_continue) => typed_continue.loc.clone(),
            TypedStmt::For(typed_for) => typed_for.loc.clone(),
            TypedStmt::Switch(typed_switch) => typed_switch.loc.clone(),
            TypedStmt::Struct(typed_struct) => typed_struct.loc.clone(),
            TypedStmt::Enum(typed_enum) => typed_enum.loc.clone(),
            TypedStmt::Interface(typed_interface) => typed_interface.loc.clone(),
            TypedStmt::Expression(typed_expression) => typed_expression.loc.clone(),
            TypedStmt::While(while_stmt) => while_stmt.loc.clone(),
            TypedStmt::Union(union_stmt) => union_stmt.loc.clone(),
            TypedStmt::Defer(typed_defer) => typed_defer.loc.clone(),
            TypedStmt::ExportTuple(export_tuple_values) => export_tuple_values.loc.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedExportTupleStmt {
    pub exports: Vec<SymbolID>,
    pub ty: Option<SemanticType>,
    pub rhs: Option<TypedExprStmt>,
    pub is_const: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedDeferStmt {
    pub operand: Box<TypedStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedInterfaceStmt {
    pub name: String,
    pub symbol_id: SymbolID,
    pub methods: Vec<TypedFuncDeclStmt>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedEnumStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub is_local: Option<ScopeID>,
    pub name: String,
    pub variants: Vec<TypedEnumVariant>,
    pub methods: HashMap<String, SymbolID>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub enum TypedEnumVariant {
    Identifier(Identifier),
    Valued(Identifier, Box<TypedExprStmt>),
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
    pub field_type: SemanticType,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedStructStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub is_local: Option<ScopeID>,
    pub name: String,
    pub fields: Vec<TypedStructField>,
    pub methods: HashMap<String, SymbolID>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub impls: Vec<TypedIdentifier>,
    pub vis: AccessSpecifier,
    pub packed: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedUnionStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub is_local: Option<ScopeID>,
    pub name: String,
    pub fields: Vec<TypedUnionField>,
    pub methods: HashMap<String, SymbolID>,
    pub generic_params: Option<TypedGenericParamsList>,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedUnionField {
    pub name: String,
    pub ty: SemanticType,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedStructField {
    pub name: String,
    pub ty: SemanticType,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedReturnStmt {
    pub arg: Option<TypedExprStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedGlobalVarStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: Option<SemanticType>,
    pub expr: Option<TypedExprStmt>,
    pub is_const: bool,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedTypedefStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: SemanticType,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedBlockStmt {
    pub scope_id: ScopeID,
    pub stmts: Vec<TypedStmt>,
    pub defers: Vec<TypedDeferStmt>,
    pub loc: SourceLoc,
}

impl TypedBlockStmt {
    pub fn new_empty(scope_id: ScopeID, loc: SourceLoc) -> Self {
        Self {
            scope_id,
            stmts: Vec::new(),
            defers: Vec::new(),
            loc,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedVarStmt {
    pub symbol_id: SymbolID,
    pub name: String,
    pub ty: Option<SemanticType>,
    pub rhs: Option<TypedExprStmt>,
    pub is_const: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedIfStmt {
    pub cond: TypedExprStmt,
    pub then_block: Box<TypedBlockStmt>,
    pub branches: Vec<TypedIfStmt>,
    pub else_block: Option<Box<TypedBlockStmt>>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDefStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub params: TypedFuncParams,
    pub body: Box<TypedBlockStmt>,
    pub return_type: SemanticType,
    pub vis: AccessSpecifier,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedFuncDeclStmt {
    pub module_id: ModuleID,
    pub symbol_id: SymbolID,
    pub name: String,
    pub params: TypedFuncParams,
    pub return_type: SemanticType,
    pub vis: AccessSpecifier,
    pub renamed_as: Option<String>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFuncParams {
    pub list: Vec<TypedFuncParamKind>,
    pub variadic: Option<TypedFuncVariadicParams>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedFuncVariadicParams {
    UntypedCStyle,
    Typed(String, SemanticType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedFuncTypeParams {
    pub list: Vec<SemanticType>,
    pub variadic: Option<Box<TypedFuncTypeVariadicParams>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypedFuncTypeVariadicParams {
    UntypedCStyle,
    Typed(SemanticType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedFuncParamKind {
    FuncParam(TypedFuncParam),
    SelfModifier(TypedSelfModifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedSelfModifier {
    pub symbol_id: Option<SymbolID>,
    pub self_symbol_id: Option<SymbolID>,
    pub ty: Option<SemanticType>,
    pub kind: SelfModifierKind,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFuncParam {
    pub name: String,
    pub ty: SemanticType,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedForStmt {
    pub initializer: Option<TypedVarStmt>,
    pub cond: Option<TypedExprStmt>,
    pub increment: Option<TypedExprStmt>,
    pub body: Box<TypedBlockStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedWhileStmt {
    pub cond: TypedExprStmt,
    pub body: Box<TypedBlockStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedSwitchStmt {
    pub operand: TypedExprStmt,
    pub cases: Vec<TypedSwitchCase>,
    pub default_case: Option<TypedBlockStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedSwitchCase {
    pub pattern: TypedSwitchCasePattern,
    pub body: Box<TypedBlockStmt>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub enum TypedSwitchCasePattern {
    Expression(TypedExprStmt, SourceLoc),
    Identifier(String, SourceLoc),
    EnumVariant(String, Vec<TypedIdentifier>, SourceLoc),
}

#[derive(Debug, Clone)]
pub struct TypedBreakStmt {
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct TypedContinueStmt {
    pub loc: SourceLoc,
}

pub type TypedGenericParamsList = Vec<TypedGenericParam>;

#[derive(Debug, Clone)]
pub struct TypedGenericParam {
    pub param_name: TypedIdentifier,
    pub bounds: Option<Vec<TypedBound>>,
    pub default: Option<SemanticType>,
}

#[derive(Debug, Clone)]
pub struct TypedBound {
    pub symbol: Identifier,
    pub type_args: Vec<TypedTypeArg>,
}

#[derive(Debug, Clone, Eq, Hash)]
pub enum TypedTypeArg {
    Positional(SemanticType),
    Named { key: String, value: SemanticType },
}

pub type TypedTypeArgs = Vec<TypedTypeArg>;

impl PartialEq for TypedTypeArg {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Positional(l0), Self::Positional(r0)) => l0 == r0,
            (
                Self::Named {
                    key: l_key,
                    value: l_value,
                },
                Self::Named {
                    key: r_key,
                    value: r_value,
                },
            ) => l_key == r_key && l_value == r_value,
            _ => false,
        }
    }
}

impl PartialEq for TypedLambdaExpr {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.return_type == other.return_type
    }
}

impl PartialEq for TypedTupleExpr {
    fn eq(&self, other: &Self) -> bool {
        self.expr_list == other.expr_list
    }
}

impl PartialEq for TypedTupleAccessExpr {
    fn eq(&self, other: &Self) -> bool {
        self.operand == other.operand && self.index == other.index
    }
}

impl PartialEq for TypedIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.symbol_id == other.symbol_id
    }
}

impl Eq for TypedIdentifier {}

impl Hash for TypedIdentifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.symbol_id.hash(state);
    }
}

pub fn lookup_symbol_from_generic_params(
    list: &TypedGenericParamsList,
    symbol_id: SymbolID,
) -> Option<TypedGenericParam> {
    list.iter()
        .find(|generic_param| generic_param.param_name.symbol_id == symbol_id)
        .cloned()
}
