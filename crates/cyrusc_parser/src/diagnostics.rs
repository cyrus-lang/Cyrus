// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::Parser;
use cyrusc_diagcentral::{Diag, DiagKind, DiagLevel};
use cyrusc_tokens::{Token, TokenKind};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ParserDiagKind {
    #[error("{0}")]
    InvalidModifier(String),

    #[error("Invalid ABI: '{0}'.")]
    InvalidABI(String),

    #[error("Expected token '{0}'.")]
    ExpectedToken(TokenKind),

    #[error("Unexpected token: '{0}'.")]
    InvalidToken(TokenKind),

    #[error("Expected type token but got '{0}'.")]
    InvalidTypeToken(TokenKind),

    #[error("Expected expression or statement.")]
    ExpectedExprOrStmt,

    #[error("Expected expression after operator '{0}'.")]
    ExpectedExpressionAfterOperator(TokenKind),

    #[error("Comparison operators cannot be chained.")]
    ChainedComparisonOperator,

    #[error("Tuple type must contain at least two elements.")]
    SingleElementTupleType,

    #[error("Missing closing brace '}}'.")]
    MissingClosingBrace,

    #[error("Missing opening brace '{{'.")]
    MissingOpeningBrace,

    #[error("Missing closing paren ')'.")]
    MissingClosingParen,

    #[error("Missing opening paren '('.")]
    MissingOpeningParen,

    #[error("Expected an identifier but got '{got}'.")]
    ExpectedIdentifier { got: String },

    #[error("Missing semicolon.")]
    MissingSemicolon,

    #[error("Missing comma.")]
    MissingComma,

    #[error("Cannot define self modifier several times in a function.")]
    SeveralSelfModifierUsed,

    #[error("Self modifier ident must be 'self' not '{0}'.")]
    ExpectedSelfModifier(String),

    #[error("Invalid infix operator '{0}'.")]
    InvalidInfixOperator(TokenKind),

    #[error("Invalid prefix operator '{0}'.")]
    InvalidPrefixOperator(TokenKind),

    #[error("String literals cannot have prefixes in this context.")]
    StringPrefixNotAllowed,

    #[error("Integer literals cannot have suffixes in this context.")]
    IntegerSuffixNotAllowed,

    #[error("Invalid assign operator '{0}'.")]
    InvalidAssignOperator(TokenKind),

    #[error("Cannot use non-array type for array construction.")]
    NonArrayDataTypeForArrayConstruction,

    #[error("Variable declaration requires an explicit type or an initializer expression.")]
    IncompleteVariableDeclaration,

    #[error("Declared method must have a body.")]
    MethodMustHaveABody,

    #[error("Cannot use grouped modifiers here.")]
    InvalidGroupedModifiers,

    #[error("Group modifiers cannot be nested.")]
    GroupedModifiersCannotBeNested,
}

impl<'source_file> Parser<'source_file> {
    #[inline]
    pub(crate) fn error_invalid_token(&self) -> Diag {
        let token = self.current_token();

        Diag {
            kind: Box::new(ParserDiagKind::InvalidToken(token.kind)),
            level: DiagLevel::Error,
            loc: Some(token.loc),
            hint: None,
        }
    }

    #[inline]
    pub(crate) fn error_at_current_with_hint(&self, kind: ParserDiagKind, hint: &str) -> Diag {
        let token = self.current_token();

        Diag {
            kind: Box::new(kind),
            level: DiagLevel::Error,
            loc: Some(token.loc),
            hint: Some(hint.to_string()),
        }
    }

    #[inline]
    pub(crate) fn error_at_current(&self, kind: ParserDiagKind) -> Diag {
        let token = self.current_token();

        Diag {
            kind: Box::new(kind),
            level: DiagLevel::Error,
            loc: Some(token.loc),
            hint: None,
        }
    }

    #[inline]
    pub(crate) fn error_at_peek(&self, kind: ParserDiagKind) -> Diag {
        let token = self.peek_token();

        Diag {
            kind: Box::new(kind),
            level: DiagLevel::Error,
            loc: Some(token.loc),
            hint: None,
        }
    }

    #[inline]
    pub(crate) fn error_at_token(&self, token: &Token, kind: ParserDiagKind) -> Diag {
        Diag {
            kind: Box::new(kind),
            level: DiagLevel::Error,
            loc: Some(token.loc),
            hint: None,
        }
    }

    #[inline]
    pub(crate) fn error_with_hint(&self, token: &Token, kind: ParserDiagKind, hint: &str) -> Diag {
        Diag {
            kind: Box::new(kind),
            level: DiagLevel::Error,
            loc: Some(token.loc),
            hint: Some(hint.to_string()),
        }
    }
}

impl DiagKind for ParserDiagKind {}
