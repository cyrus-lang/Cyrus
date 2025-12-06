use crate::{
    SymbolID,
    generics::monomorph::MonomorphKey,
    stmts::{TypedBlockStmt, TypedFuncParams, TypedTypeArgs},
    types::{SemanticType, TypedUStructType},
};
use cyrusc_ast::{
    AssignmentKind, LiteralKind,
    operators::{InfixOperator, PrefixOperator, UnaryOperator},
    source_loc::SourceLoc,
};

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
            LiteralKind::Integer { .. } => "integer",
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

#[derive(Debug, Clone, Eq)]
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
    pub type_args: Option<TypedTypeArgs>,
    pub return_type: Option<SemanticType>,
    pub monomorph_key: Option<MonomorphKey>, // only used when calling a generic func
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
    pub method_symbol_id: Option<SymbolID>,
    pub method_name: String,
    pub args: Vec<TypedExprStmt>,
    pub type_args: Option<TypedTypeArgs>,
    pub monomorph_key: Option<MonomorphKey>, // only used when calling a generic method
    pub return_type: Option<SemanticType>,
    pub is_fat_arrow: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUStructValue {
    pub fields: Vec<TypedUStructValueField>,
    pub unnamed_struct_type: Option<TypedUStructType>,
    pub is_packed: bool,
    pub is_const: bool,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedUStructValueField {
    pub field_name: String,
    pub field_ty: Option<SemanticType>,
    pub field_value: Box<TypedExprStmt>,
    pub loc: SourceLoc,
}
