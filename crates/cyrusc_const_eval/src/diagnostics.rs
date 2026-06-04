// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_typed_ast::decls::DeclID;

#[derive(Debug)]
pub enum ConstEvalError {
    NonConstSymbol(DeclID),
    CyclicConst(DeclID),
    UnsupportedExpr,
    DivisionByZero,
    TypeError,
}
