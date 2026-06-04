// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_strescape::unescape_string;
use cyrusc_tokens::{
    TokenKind,
    literals::{LiteralKind, StringPrefix},
};
use cyrusc_typed_ast::{
    exprs::{TypedLiteralExpr, literal_expr_from_const_int},
    types::{
        PlainType, SemaType, TypedArrayCapacity, TypedArrayType, map_float_suffix_to_sema_type,
        map_integer_suffix_to_sema_type,
    },
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_literal(
        &mut self,
        literal: &mut TypedLiteralExpr,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let literal_clone = literal.clone();

        let type_opt = match &mut literal.kind {
            LiteralKind::Integer(_, suffix_opt) => {
                match infer_integer_type(&literal_clone, suffix_opt, expected_type.clone()) {
                    Ok(ty) => Some(ty),
                    Err(diag) => {
                        self.reporter.report(diag);
                        None
                    }
                }
            }
            LiteralKind::Float(_, suffix_opt) => {
                match infer_float_type(&literal_clone, suffix_opt, expected_type.clone()) {
                    Ok(ty) => Some(ty),
                    Err(diag) => {
                        self.reporter.report(diag);
                        None
                    }
                }
            }
            LiteralKind::String(value, prefix_opt) => {
                *value = match unescape_string(&value).and_then(|v| unescape_string(&v)) {
                    Ok(v) => v,
                    Err(unescape_err) => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::UnescapeError(unescape_err)),
                            loc: Some(literal.loc),
                            hint: None,
                        });
                        return None;
                    }
                };

                let capacity = literal_expr_from_const_int(value.len(), literal.loc);

                let ty = if let Some(prefix) = prefix_opt {
                    match prefix {
                        StringPrefix::C => SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char))),
                        StringPrefix::B => SemaType::Array(TypedArrayType {
                            element_type: Box::new(SemaType::Const(Box::new(SemaType::Plain(PlainType::Char)))),
                            capacity: TypedArrayCapacity::Fixed(Box::new(capacity)),
                            loc: literal.loc,
                        }),
                    }
                } else {
                    SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char)))
                };

                Some(ty)
            }
            LiteralKind::Bool(_) => Some(SemaType::Plain(PlainType::Bool)),
            LiteralKind::Char(_) => Some(SemaType::Plain(PlainType::Char)),
            LiteralKind::Null => Some(SemaType::Plain(PlainType::Null)),
        };

        if let Some(ty) = &type_opt {
            literal.ty = Some(ty.clone());
        }

        type_opt
    }
}

fn infer_integer_type(
    literal: &TypedLiteralExpr,
    suffix_opt: &Option<Box<TokenKind>>,
    expected: Option<SemaType>,
) -> Result<SemaType, Diag> {
    if let Some(suffix) = suffix_opt {
        return match map_integer_suffix_to_sema_type(&suffix) {
            Some(ty) => Ok(ty),
            None => Err(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidIntegerLiteralSuffix),
                loc: Some(literal.loc),
                hint: Some(format!("Invalid suffix '{}' for integer literal.", suffix)),
            }),
        };
    } else if let Some(ty) = expected {
        if ty.is_integer() {
            return Ok(ty);
        }
    }

    // default integer type
    Ok(SemaType::Plain(PlainType::Int))
}

fn infer_float_type(
    literal: &TypedLiteralExpr,
    suffix_opt: &Option<Box<TokenKind>>,
    expected: Option<SemaType>,
) -> Result<SemaType, Diag> {
    if let Some(suffix) = suffix_opt {
        return match map_float_suffix_to_sema_type(&suffix) {
            Some(ty) => Ok(ty),
            None => Err(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidFloatLiteralSuffix),
                loc: Some(literal.loc),
                hint: Some(format!("Invalid suffix '{}' for float literal.", suffix)),
            }),
        };
    } else if let Some(ty) = expected {
        if ty.is_float() {
            return Ok(ty);
        }
    }

    // default float type
    Ok(SemaType::Plain(PlainType::Float64))
}
