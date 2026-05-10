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

use crate::{
    exprs::TypedExpr,
    stmts::{TypedBlockStmt, TypedStmt},
    types::SemaType,
};
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

#[derive(Debug, Clone)]
pub struct TypedBuiltinFunc {
    pub name: Ident,
    pub args: Vec<TypedExpr>,
    pub child_stmt: Option<Box<TypedStmt>>,
    pub ret_type: Option<SemaType>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct TypedBuiltinBlock {
    pub name: Ident,
    pub args: Vec<TypedExpr>,
    pub block: Box<TypedBlockStmt>,
    pub loc: Loc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedBuiltinFamily {
    Attribute,
    ConstEval,
    Intrinsic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedBuiltinPhase {
    Analyzer,
    ConstEval,
    CIRLowering,
    Codegen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedBuiltinForm {
    Expr,
    Stmt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypedBuiltinKind {
    SizeOf,
    AlignOf,
    OffsetOf,
    Memcpy,
    Memset,

    Debug,
    Release,
    Unroll,
}

#[derive(Debug, Clone)]
pub struct TypedBuiltinSpec {
    pub kind: TypedBuiltinKind,
    pub name: &'static str,

    pub family: TypedBuiltinFamily,
    pub form: TypedBuiltinForm,
    pub phase: TypedBuiltinPhase,

    pub min_args: usize,
    pub max_args: Option<usize>,
}

pub static BUILTIN_SPECS: &[TypedBuiltinSpec] = &[
    // -- const-evals --
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::SizeOf,
        name: "sizeof",
        family: TypedBuiltinFamily::ConstEval,
        form: TypedBuiltinForm::Expr,
        phase: TypedBuiltinPhase::ConstEval,
        min_args: 1,
        max_args: Some(1),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::AlignOf,
        name: "alignof",
        family: TypedBuiltinFamily::ConstEval,
        form: TypedBuiltinForm::Expr,
        phase: TypedBuiltinPhase::ConstEval,
        min_args: 1,
        max_args: Some(1),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::OffsetOf,
        name: "offsetof",
        family: TypedBuiltinFamily::ConstEval,
        form: TypedBuiltinForm::Expr,
        phase: TypedBuiltinPhase::ConstEval,
        min_args: 2,
        max_args: Some(2),
    },
    // -- intrinsics --
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Memcpy,
        name: "memcpy",
        family: TypedBuiltinFamily::Intrinsic,
        form: TypedBuiltinForm::Expr,
        phase: TypedBuiltinPhase::Codegen,
        min_args: 3,
        max_args: Some(3),
    },
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Memset,
        name: "memset",
        family: TypedBuiltinFamily::Intrinsic,
        form: TypedBuiltinForm::Expr,
        phase: TypedBuiltinPhase::Codegen,
        min_args: 3,
        max_args: Some(3),
    },
    // -- statements --
    TypedBuiltinSpec {
        kind: TypedBuiltinKind::Debug,
        name: "debug",
        family: TypedBuiltinFamily::ConstEval,
        form: TypedBuiltinForm::Stmt,
        phase: TypedBuiltinPhase::CIRLowering,
        min_args: 0,
        max_args: Some(0),
    },
    // -- attributes --
    // TypedBuiltinSpec {
    //     kind: TypedBuiltinKind::Unroll,
    //     name: "unroll",
    //     family: TypedBuiltinFamily::Attribute,
    //     form: TypedBuiltinForm::Stmt,
    //     phase: TypedBuiltinPhase::Codegen,
    //     min_args: 0,
    //     max_args: Some(1),
    // },
    // TypedBuiltinSpec {
    //     kind: TypedBuiltinKind::Allow,
    //     name: "allow",
    //     family: TypedBuiltinFamily::Attribute,
    //     form: TypedBuiltinForm::Block,
    //     phase: TypedBuiltinPhase::Analyzer,
    //     min_args: 1,
    //     max_args: Some(1),
    // },
];

builtin_lookup! {
    pub fn lookup_builtin(name: &str) -> Option<TypedBuiltinKind> {
        "sizeof" => TypedBuiltinKind::SizeOf,
        "alignof" => TypedBuiltinKind::AlignOf,
        "offsetof" => TypedBuiltinKind::OffsetOf,
        "unroll" => TypedBuiltinKind::Unroll,
        "memcpy" => TypedBuiltinKind::Memcpy,
        "memset" => TypedBuiltinKind::Memset,
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

impl PartialEq for TypedBuiltinFunc {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.child_stmt.is_some() == other.child_stmt.is_some()
    }
}

impl PartialEq for TypedBuiltinBlock {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for TypedBuiltinFunc {}
impl Eq for TypedBuiltinBlock {}

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
