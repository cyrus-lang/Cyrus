// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use std::hash::{Hash, Hasher};

use crate::{exprs::TypedExpr, stmts::TypedBlockStmt, types::SemaType};
use cyrusc_ast::Ident;
use cyrusc_source_loc::Loc;

#[macro_export]
macro_rules! builtin_lookup {
    (
        $vis:vis fn $name:ident($arg:ident : &str) -> Option<$ret:ty> {
            $(
                $key:literal => $value:expr
            ),* $(,)?
        }
    ) => {
        $vis fn $name($arg: &str) -> Option<$ret> {
            match $arg {
                $(
                    $key => Some($value),
                )*
                _ => None,
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedBuiltin {
    BuiltinFunc(TypedBuiltinFunc),
    BuiltinBlock(TypedBuiltinBlock),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedBuiltinForm {
    Expr,
    Stmt,
}

#[derive(Debug, Clone)]
pub struct TypedBuiltinFunc {
    pub name: Ident,
    pub args: Vec<TypedExpr>,
    pub ret_type: Option<SemaType>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedBuiltinBlock {
    pub name: Ident,
    pub args: Vec<TypedExpr>,
    pub block: Box<TypedBlockStmt>,
    pub is_toplevel: Option<bool>,
    pub loc: Loc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedBuiltinFamily {
    ConstEval,
    Intrinsic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedBuiltinPhase {
    Resolver,
    ConstEval,
    Codegen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypedBuiltinKind {
    FuncName,
    MethodName,
    ModuleName,
    FileName,
    Line,
    Column,

    SizeOf,
    AlignOf,
    OffsetOf,
    TypeOf,

    Cast,
    Memcpy,
    Memset,
    Assert,
    Panic,
    Todo,
    Unimplemented,
    Unreachable,

    Debug,
    Release,
    Unroll,
}

#[derive(Debug, Clone)]
pub struct TypedBuiltinSpec {
    pub kind: TypedBuiltinKind,
    pub name: &'static str,

    pub family: TypedBuiltinFamily,
    pub phase: TypedBuiltinPhase,
    pub form: TypedBuiltinForm,
    pub unreachable: bool,

    pub min_args: usize,
    pub max_args: Option<usize>,
}

pub static BUILTIN_SPECS: &[TypedBuiltinSpec] = &[
    // -- const-evals --
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::FuncName,
        name: "func_name",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::ConstEval,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 0,
        max_args: Some(0),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::MethodName,
        name: "method_name",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::ConstEval,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 0,
        max_args: Some(0),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::ModuleName,
        name: "module_name",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::ConstEval,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 0,
        max_args: Some(0),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::FileName,
        name: "file_name",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::ConstEval,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 0,
        max_args: Some(0),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Line,
        name: "line",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::ConstEval,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 0,
        max_args: Some(0),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Column,
        name: "column",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::ConstEval,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 0,
        max_args: Some(0),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::SizeOf,
        name: "sizeof",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::ConstEval,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 1,
        max_args: Some(1),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::AlignOf,
        name: "alignof",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::ConstEval,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 1,
        max_args: Some(1),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::OffsetOf,
        name: "offsetof",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::ConstEval,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 2,
        max_args: Some(2),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::TypeOf,
        name: "typeof",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::ConstEval,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 1,
        max_args: Some(1),
    },
    // -- intrinsics --
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Cast,
        name: "cast",
        family: TypedBuiltinFamily::Intrinsic,
        phase: TypedBuiltinPhase::Codegen,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 2,
        max_args: Some(2),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Memcpy,
        name: "memcpy",
        family: TypedBuiltinFamily::Intrinsic,
        phase: TypedBuiltinPhase::Codegen,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 3,
        max_args: Some(3),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Memset,
        name: "memset",
        family: TypedBuiltinFamily::Intrinsic,
        phase: TypedBuiltinPhase::Codegen,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 3,
        max_args: Some(3),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Assert,
        name: "assert",
        family: TypedBuiltinFamily::Intrinsic,
        phase: TypedBuiltinPhase::Codegen,
        form: TypedBuiltinForm::Expr,
        unreachable: false,
        min_args: 1,
        max_args: Some(2),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Panic,
        name: "panic",
        family: TypedBuiltinFamily::Intrinsic,
        phase: TypedBuiltinPhase::Codegen,
        form: TypedBuiltinForm::Expr,
        unreachable: true,
        min_args: 0,
        max_args: Some(1),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Todo,
        name: "todo",
        family: TypedBuiltinFamily::Intrinsic,
        phase: TypedBuiltinPhase::Codegen,
        form: TypedBuiltinForm::Expr,
        unreachable: true,
        min_args: 0,
        max_args: Some(1),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Unimplemented,
        name: "unimplemented",
        family: TypedBuiltinFamily::Intrinsic,
        phase: TypedBuiltinPhase::Codegen,
        form: TypedBuiltinForm::Expr,
        unreachable: true,
        min_args: 0,
        max_args: Some(1),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Unreachable,
        name: "unreachable",
        family: TypedBuiltinFamily::Intrinsic,
        phase: TypedBuiltinPhase::Codegen,
        form: TypedBuiltinForm::Expr,
        unreachable: true,
        min_args: 0,
        max_args: Some(1),
    },
    // -- statements --
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Debug,
        name: "debug",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::Resolver,
        form: TypedBuiltinForm::Stmt,
        unreachable: false,
        min_args: 0,
        max_args: Some(0),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Release,
        name: "release",
        family: TypedBuiltinFamily::ConstEval,
        phase: TypedBuiltinPhase::Resolver,
        form: TypedBuiltinForm::Stmt,
        unreachable: false,
        min_args: 0,
        max_args: Some(0),
    },
];

builtin_lookup! {
    pub fn lookup_builtin(name: &str) -> Option<TypedBuiltinKind> {
        // const-evals
        "func_name" => TypedBuiltinKind::FuncName,
        "method_name" => TypedBuiltinKind::MethodName,
        "module_name" => TypedBuiltinKind::ModuleName,
        "file_name" => TypedBuiltinKind::FileName,
        "line" => TypedBuiltinKind::Line,
        "column" => TypedBuiltinKind:: Column,
        "sizeof" => TypedBuiltinKind::SizeOf,
        "alignof" => TypedBuiltinKind::AlignOf,
        "offsetof" => TypedBuiltinKind::OffsetOf,
        "typeof" => TypedBuiltinKind::TypeOf,

        // intrinsics
        "cast" => TypedBuiltinKind::Cast,
        "memcpy" => TypedBuiltinKind::Memcpy,
        "memset" => TypedBuiltinKind::Memset,
        "assert" => TypedBuiltinKind::Assert,
        "panic" => TypedBuiltinKind::Panic,
        "todo" => TypedBuiltinKind::Todo,
        "unimplemented" => TypedBuiltinKind::Unimplemented,
        "unreachable" => TypedBuiltinKind::Unreachable,

        // statements
        "debug" => TypedBuiltinKind::Debug,
        "release" => TypedBuiltinKind::Release,
    }
}

pub fn builtin_spec_of(kind: TypedBuiltinKind) -> &'static TypedBuiltinSpec {
    BUILTIN_SPECS
        .iter()
        .find(|s| s.kind == kind)
        .expect("builtin spec missing")
}

impl TypedBuiltin {
    #[inline]
    pub fn as_builtin_func(&self) -> Option<&TypedBuiltinFunc> {
        match self {
            TypedBuiltin::BuiltinFunc(builtin_func) => Some(builtin_func),
            _ => None,
        }
    }

    #[inline]
    pub fn as_builtin_block(&self) -> Option<&TypedBuiltinBlock> {
        match self {
            TypedBuiltin::BuiltinBlock(builtin_block) => Some(builtin_block),
            _ => None,
        }
    }
}

// hash

impl Hash for TypedBuiltinFunc {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.ret_type.hash(state);
    }
}

// partial-eq

impl PartialEq for TypedBuiltinFunc {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialEq for TypedBuiltinBlock {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

// eq

impl Eq for TypedBuiltinFunc {}
impl Eq for TypedBuiltinBlock {}
