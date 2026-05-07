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
pub enum BuiltinFamily {
    Attribute,
    ConstEval,
    Intrinsic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinPhase {
    Analyzer,
    CIRLowering,
    CIRConstEval,
    Codegen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinForm {
    Expr,
    Stmt,
    Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinLowering {
    Remove,
    ToConst,
    ToAttr,
    ToInstruction,
    PreserveToCodegen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypedBuiltinKind {
    Allow,
    SizeOf,
    AlignOf,
    Unroll,
    Memcpy,
    Memset,
}

#[derive(Debug)]
pub struct BuiltinSpec {
    pub kind: TypedBuiltinKind,
    pub name: &'static str,

    pub family: BuiltinFamily,
    pub form: BuiltinForm,

    pub phase: BuiltinPhase,
    pub lowering: BuiltinLowering,

    pub min_args: usize,
    pub max_args: usize,
}

pub static BUILTIN_SPECS: &[BuiltinSpec] = &[
    // -- const-evals --
    BuiltinSpec {
        kind: TypedBuiltinKind::SizeOf,
        name: "sizeof",
        family: BuiltinFamily::ConstEval,
        form: BuiltinForm::Expr,
        phase: BuiltinPhase::CIRConstEval,
        lowering: BuiltinLowering::ToConst,
        min_args: 1,
        max_args: 1,
    },
    BuiltinSpec {
        kind: TypedBuiltinKind::AlignOf,
        name: "alignof",
        family: BuiltinFamily::ConstEval,
        form: BuiltinForm::Expr,
        phase: BuiltinPhase::CIRConstEval,
        lowering: BuiltinLowering::ToConst,
        min_args: 1,
        max_args: 1,
    },
    // -- intrinsics --
    BuiltinSpec {
        kind: TypedBuiltinKind::Memcpy,
        name: "memcpy",
        family: BuiltinFamily::Intrinsic,
        form: BuiltinForm::Expr,
        phase: BuiltinPhase::Codegen,
        lowering: BuiltinLowering::ToInstruction,
        min_args: 3,
        max_args: 3,
    },
    BuiltinSpec {
        kind: TypedBuiltinKind::Memset,
        name: "memset",
        family: BuiltinFamily::Intrinsic,
        form: BuiltinForm::Expr,
        phase: BuiltinPhase::Codegen,
        lowering: BuiltinLowering::ToInstruction,
        min_args: 3,
        max_args: 3,
    },
    // -- attributes --
    BuiltinSpec {
        kind: TypedBuiltinKind::Unroll,
        name: "unroll",
        family: BuiltinFamily::Attribute,
        form: BuiltinForm::Stmt,
        phase: BuiltinPhase::Codegen,
        lowering: BuiltinLowering::ToAttr,
        min_args: 0,
        max_args: 1,
    },
    BuiltinSpec {
        kind: TypedBuiltinKind::Allow,
        name: "allow",
        family: BuiltinFamily::Attribute,
        form: BuiltinForm::Block,
        phase: BuiltinPhase::Analyzer,
        lowering: BuiltinLowering::Remove,
        min_args: 1,
        max_args: 1,
    },
];

builtin_lookup! {
    pub fn lookup_builtin(name: &str) -> Option<TypedBuiltinKind> {
        "allow" => TypedBuiltinKind::Allow,
        "sizeof" => TypedBuiltinKind::SizeOf,
        "alignof" => TypedBuiltinKind::AlignOf,
        "unroll" => TypedBuiltinKind::Unroll,
        "memcpy" => TypedBuiltinKind::Memcpy,
        "memset" => TypedBuiltinKind::Memset,
    }
}

pub fn builtin_spec_of(kind: TypedBuiltinKind) -> &'static BuiltinSpec {
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
