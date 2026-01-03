// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
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
use crate::Diag;
use crate::Parser;
use crate::diagnostics::ParserDiagKind;
use crate::prec::Precedence;
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_ast::token::*;
use cyrusc_ast::*;
use cyrusc_diagcentral::{DiagLevel, DiagLoc};

impl Parser {
    pub(crate) fn parse_identifier(&mut self) -> Result<Identifier, Diag> {
        match self.current_token().kind {
            TokenKind::Identifier { name } => Ok(Identifier {
                name,
                span: self.current_token().span.clone(),
                loc: self.current_token().loc.clone(),
            }),
            _ => Err(Diag {
                kind: Box::new(ParserDiagKind::ExpectedIdentifier),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: None,
            }),
        }
    }

    pub(crate) fn is_type_token(&mut self, token_kind: TokenKind) -> bool {
        if PRIMITIVE_TYPES.contains(&token_kind) {
            true
        } else if let TokenKind::Identifier { .. } = token_kind {
            true
        } else {
            matches!(
                token_kind,
                TokenKind::Asterisk | TokenKind::Ampersand | TokenKind::Const
            )
        }
    }

    pub(crate) fn parse_type_specifier(&mut self) -> Result<TypeSpecifier, Diag> {
        let mut base_type = self.parse_base_type_token()?;

        loop {
            if self.peek_token_is(TokenKind::Asterisk) {
                self.next_token();
                base_type = TypeSpecifier::Deref(Box::new(base_type));
            } else if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token(); // consume base_type
                base_type = self.parse_array_type(base_type)?;
            } else {
                break;
            }
        }

        if self.peek_token_is(TokenKind::LessThan) {
            let start = self.peek_token().span.start;
            let loc = self.peek_token().loc.clone();

            self.next_token();
            let type_args = self.parse_type_arg_list()?;

            Ok(TypeSpecifier::GenericInst(GenericInst {
                base: Box::new(base_type),
                type_args,
                loc,
                span: Span::new(start, self.current_token().span.end),
            }))
        } else {
            Ok(base_type)
        }
    }

    pub(crate) fn parse_generic_params(&mut self) -> Result<GenericParamsList, Diag> {
        self.expect_current(TokenKind::LessThan)?;

        let mut generic_params: GenericParamsList = GenericParamsList::new();

        loop {
            generic_params.push(self.parse_generic_param()?);

            match self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                    continue;
                }
                _ => break,
            }
        }

        self.expect_current(TokenKind::GreaterThan)?;
        Ok(generic_params)
    }

    pub(crate) fn parse_type_arg_list(&mut self) -> Result<Vec<TypeArg>, Diag> {
        self.expect_current(TokenKind::LessThan)?;

        let mut args = Vec::new();

        loop {
            if matches!(self.current_token().kind, TokenKind::Identifier { .. })
                && self.peek_token_is(TokenKind::Assign)
            {
                let key = self.parse_identifier()?;
                self.next_token(); // consume identifier
                self.expect_current(TokenKind::Assign)?;

                let ty = self.parse_type_specifier()?;
                self.next_token();

                args.push(TypeArg::Named { key, ty });
            } else {
                let ty = self.parse_type_specifier()?;
                self.next_token();
                args.push(TypeArg::Positional(ty));
            }

            match self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                    continue;
                }
                TokenKind::GreaterThan => {
                    // self.next_token();
                    break;
                }
                _ => {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            self.current_token().loc.clone(),
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
            }
        }

        Ok(args)
    }

    pub(crate) fn is_type_arg_start(&mut self, last_parsed_expression: Expr) -> bool {
        if !self.peek_token_is(TokenKind::LessThan) {
            return false;
        }

        if !self.current_expr_is_path_like(last_parsed_expression) {
            return false;
        }

        let mut i = 1;
        let mut depth = 0;

        while let Some(token) = self.peek_n_token(i) {
            if self.token_disqualifies_type_arg(&token.kind) {
                return false;
            }

            match token.kind {
                TokenKind::LessThan => {
                    depth += 1;
                }
                TokenKind::GreaterThan => {
                    depth -= 1;
                    if depth == 0 {
                        return true;
                    }
                }
                TokenKind::Semicolon | TokenKind::EOF => {
                    return false;
                }
                _ => {}
            }
            i += 1;
        }

        false
    }

    fn token_disqualifies_type_arg(&mut self, kind: &TokenKind) -> bool {
        match kind {
            // allowed tokens
            TokenKind::Identifier { .. } => false,

            TokenKind::UIntPtr
            | TokenKind::IntPtr
            | TokenKind::SizeT
            | TokenKind::Int
            | TokenKind::Int8
            | TokenKind::Int16
            | TokenKind::Int32
            | TokenKind::Int64
            | TokenKind::Int128
            | TokenKind::UInt
            | TokenKind::UInt8
            | TokenKind::UInt16
            | TokenKind::UInt32
            | TokenKind::UInt64
            | TokenKind::UInt128
            | TokenKind::Float16
            | TokenKind::Float32
            | TokenKind::Float64
            | TokenKind::Float128
            | TokenKind::Char
            | TokenKind::Void
            | TokenKind::Bool => false,

            TokenKind::DoubleColon => false,

            TokenKind::LessThan | TokenKind::GreaterThan | TokenKind::Comma => false,

            TokenKind::Const | TokenKind::Public | TokenKind::Extern => false,

            TokenKind::Asterisk | TokenKind::Ampersand | TokenKind::LeftBracket | TokenKind::RightBracket => false,

            // assign used in named-type-args that why it doesn't disqualify
            TokenKind::Assign => false,

            // any other token disqualifies type arg
            TokenKind::Literal(_) => true,

            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Increment
            | TokenKind::Decrement
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::Bang
            | TokenKind::LessEqual
            | TokenKind::GreaterEqual
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Pipe
            | TokenKind::Caret
            | TokenKind::AmpTilde
            | TokenKind::Tilde
            | TokenKind::ShiftLeft
            | TokenKind::ShiftRight => true,

            TokenKind::LeftParen | TokenKind::RightParen => true,

            TokenKind::If
            | TokenKind::Else
            | TokenKind::For
            | TokenKind::While
            | TokenKind::Foreach
            | TokenKind::Switch
            | TokenKind::Case
            | TokenKind::Default
            | TokenKind::Return
            | TokenKind::Break
            | TokenKind::Continue
            | TokenKind::Goto
            | TokenKind::Defer => true,

            TokenKind::ThinArrow
            | TokenKind::FatArrow
            | TokenKind::DoubleQuote
            | TokenKind::SingleQuote
            | TokenKind::Dot
            | TokenKind::DoubleDot
            | TokenKind::TripleDot => true,

            TokenKind::Struct | TokenKind::Union | TokenKind::Enum | TokenKind::Interface | TokenKind::Bits => true,

            TokenKind::True | TokenKind::False | TokenKind::Null => true,

            TokenKind::Macro | TokenKind::In | TokenKind::As => true,

            TokenKind::LeftBrace | TokenKind::RightBrace => true,

            TokenKind::Semicolon => true,

            TokenKind::Typedef | TokenKind::Typecast | TokenKind::SizeOf | TokenKind::AlignOf | TokenKind::OffsetOf => {
                true
            }

            other => !self.is_type_token(other.clone()),
        }
    }

    pub(crate) fn parse_single_array_index(&mut self) -> Result<Expr, Diag> {
        self.expect_current(TokenKind::LeftBracket)?;

        if self.current_token_is(TokenKind::RightBracket) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: None,
            });
        }
        let index = self.parse_expr(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::RightBracket)?;
        Ok(index)
    }

    pub(crate) fn parse_string_literal(&mut self) -> Result<String, Diag> {
        let loc = self.current_token().loc.clone();

        match self.current_token().kind {
            TokenKind::Literal(literal) => match literal.kind {
                LiteralKind::String(value, string_prefix) => {
                    if string_prefix.is_some() {
                        return Err(Diag {
                            kind: Box::new(ParserDiagKind::ExpectedStringLiteral),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                            hint: Some("Consider to remove the string prefix.".to_string()),
                        });
                    }
                    Ok(value)
                }
                _ => Err(Diag {
                    kind: Box::new(ParserDiagKind::ExpectedStringLiteral),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                    hint: Some("Expected string literal as argument".to_string()),
                }),
            },
            _ => Err(Diag {
                kind: Box::new(ParserDiagKind::ExpectedStringLiteral),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                hint: Some("Expected string literal as argument".to_string()),
            }),
        }
    }

    fn parse_func_type_params(&mut self) -> Result<FuncTypeParams, Diag> {
        let loc = self.current_token().loc.clone();

        self.expect_current(TokenKind::LeftParen)?;

        let mut variadic: Option<FuncTypeVariadicParams> = None;
        let mut list: Vec<TypeSpecifier> = Vec::new();

        while self.current_token().kind != TokenKind::RightParen {
            if self.current_token_is(TokenKind::TripleDot) {
                self.next_token(); // consume triple dot

                if self.current_token_is(TokenKind::RightParen) {
                    variadic = Some(FuncTypeVariadicParams::UntypedCStyle);
                    break;
                } else {
                    let variadic_data_type = self.parse_type_specifier()?;
                    self.next_token();

                    variadic = Some(FuncTypeVariadicParams::Typed(variadic_data_type));
                    break;
                }
            } else {
                let var_type = self.parse_type_specifier()?;
                self.next_token();
                list.push(var_type);
            }

            match &self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                }
                TokenKind::RightParen => {
                    break;
                }
                _ => {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::MissingComma),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                        hint: None,
                    });
                }
            }
        }

        self.expect_current(TokenKind::RightParen)?;

        Ok(FuncTypeParams { list, variadic })
    }

    fn parse_func_type(&mut self) -> Result<TypeSpecifier, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token(); // consume function

        let params = self.parse_func_type_params()?;
        let ret = self.parse_type_specifier()?;

        Ok(TypeSpecifier::FuncType(Box::new(FuncType {
            params,
            return_type: Box::new(ret),
            span: Span::new(start, self.current_token().span.end),
            vis_opt: None,
            loc,
        })))
    }

    fn parse_tuple(&mut self) -> Result<TypeSpecifier, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.expect_current(TokenKind::LeftParen)?;

        let mut type_list: Vec<TypeSpecifier> = Vec::new();

        loop {
            let type_specifier = self.parse_type_specifier()?;
            self.next_token();

            type_list.push(type_specifier);

            match self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                    continue;
                }
                _ => break,
            }
        }

        if !self.current_token_is(TokenKind::RightParen) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::MissingClosingParen),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: None,
            });
        }

        if type_list.len() <= 1 {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::SingleElementTupleType),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: Some(
                    "If you only need a single element, remove the tuple syntax and use the type directly.".to_string(),
                ),
            });
        }

        Ok(TypeSpecifier::Tuple(TupleType {
            type_list,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    fn parse_base_type_token(&mut self) -> Result<TypeSpecifier, Diag> {
        let current = self.current_token().clone();

        match current.kind {
            ref token_kind if PRIMITIVE_TYPES.contains(&token_kind) => Ok(TypeSpecifier::TypeToken(current)),
            TokenKind::LeftParen => self.parse_tuple(),
            TokenKind::Function => self.parse_func_type(),
            TokenKind::Struct | TokenKind::Bits => self.parse_struct_type(),
            TokenKind::Const => {
                self.next_token(); // consume const
                let inner_type = self.parse_base_type_token()?;
                Ok(TypeSpecifier::Const(Box::new(inner_type)))
            }
            TokenKind::Identifier { ref name } => {
                if self.peek_token_is(TokenKind::DoubleColon) {
                    let module_import = self.parse_module_import()?;
                    Ok(TypeSpecifier::ModuleImport(module_import))
                } else {
                    if name == "Self" {
                        Ok(TypeSpecifier::SelfType(SelfType {
                            span: current.span,
                            loc: self.current_token().loc.clone(),
                        }))
                    } else {
                        Ok(TypeSpecifier::Identifier(Identifier {
                            name: {
                                if let TokenKind::Identifier { name } = current.kind {
                                    name
                                } else {
                                    unreachable!()
                                }
                            },
                            span: current.span,
                            loc: self.current_token().loc.clone(),
                        }))
                    }
                }
            }
            _ => Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidTypeToken(current.kind)),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: None,
            }),
        }
    }

    fn parse_array_type(&mut self, base_type_specifier: TypeSpecifier) -> Result<TypeSpecifier, Diag> {
        let mut dimensions: Vec<ArrayCapacity> = Vec::new();

        while self.current_token_is(TokenKind::LeftBracket) {
            let array_capacity = self.parse_single_array_capacity()?;
            // prevent consuming the latest token_kind here
            if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token(); // consume right bracket
            }
            dimensions.push(array_capacity);
        }

        let mut type_specifier = base_type_specifier.clone();
        for dimension in dimensions.iter().rev() {
            type_specifier = TypeSpecifier::Array(ArrayTypeSpecifier {
                size: dimension.clone(),
                element_type: Box::new(type_specifier),
            });
        }

        Ok(type_specifier)
    }

    fn parse_single_array_capacity(&mut self) -> Result<ArrayCapacity, Diag> {
        self.expect_current(TokenKind::LeftBracket)?;
        if self.current_token_is(TokenKind::RightBracket) {
            return Ok(ArrayCapacity::Dynamic);
        }
        let capacity = self.parse_expr(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::RightBracket)?;
        Ok(ArrayCapacity::Fixed(Box::new(capacity)))
    }

    fn parse_struct_type(&mut self) -> Result<TypeSpecifier, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let is_packed = {
            if self.current_token_is(TokenKind::Bits) {
                self.next_token();
                true
            } else if self.current_token_is(TokenKind::Struct) {
                self.next_token();
                false
            } else {
                return Err(Diag {
                    kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        self.current_token().loc.clone(),
                        self.file_name.clone(),
                    ))),
                    hint: None,
                });
            }
        };
        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields: Vec<UnnamedStructTypeField> = Vec::new();

        loop {
            match self.current_token().kind {
                TokenKind::RightBrace => {
                    break;
                }
                TokenKind::EOF => {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::MissingClosingBrace),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            self.current_token().loc.clone(),
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
                TokenKind::Identifier { .. } => {
                    let start = self.current_token().span.start;
                    let loc = self.current_token().loc.clone();

                    let field_name = self.parse_identifier()?;
                    self.next_token(); // consume identifier

                    self.expect_current(TokenKind::Colon)?;

                    let field_type_specifier = self.parse_type_specifier()?;
                    self.next_token();

                    fields.push(UnnamedStructTypeField {
                        field_name,
                        field_ty: field_type_specifier,
                        loc,
                        span: Span {
                            start,
                            end: self.current_token().span.end,
                        },
                    });

                    if self.current_token_is(TokenKind::RightBrace) {
                        break;
                    } else {
                        self.expect_current(TokenKind::Comma)?;
                    }
                }
                _ => {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            self.current_token().loc.clone(),
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
            }
        }

        Ok(TypeSpecifier::UnnamedStruct(UnnamedStructType {
            fields,
            is_packed,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    fn parse_bounds(&mut self) -> Result<Vec<Bound>, Diag> {
        self.expect_current(TokenKind::Colon)?;

        let mut list: Vec<Bound> = Vec::new();

        loop {
            let symbol = self.parse_identifier()?;
            self.next_token();

            list.push(Bound {
                symbol,
                type_args: Vec::new(),
            });

            match self.current_token().kind {
                TokenKind::Plus => {
                    self.next_token();
                    continue;
                }
                _ => break,
            }
        }

        Ok(list)
    }

    fn parse_generic_param(&mut self) -> Result<GenericParam, Diag> {
        let param_name = self.parse_identifier()?;
        self.next_token();

        let bounds = if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume identifier
            Some(self.parse_bounds()?)
        } else {
            None
        };

        let default = if self.current_token_is(TokenKind::Assign) {
            self.next_token(); // consume assign
            let type_specifier = self.parse_type_specifier()?;
            self.next_token();
            Some(type_specifier)
        } else {
            None
        };

        Ok(GenericParam {
            param_name,
            bounds,
            default,
        })
    }

    fn current_expr_is_path_like(&self, last_parsed_expression: Expr) -> bool {
        matches!(last_parsed_expression, Expr::Identifier(..) | Expr::ModuleImport(..))
    }
}
