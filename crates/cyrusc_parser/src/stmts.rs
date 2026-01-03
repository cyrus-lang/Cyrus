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
use crate::modifiers::UnresolvedModifiers;
use crate::prec::Precedence;
use cyrusc_abi::modifiers::EnumModifiers;
use cyrusc_abi::modifiers::FuncModifiers;
use cyrusc_abi::modifiers::StructModifiers;
use cyrusc_abi::modifiers::UnionModifiers;
use cyrusc_abi::visibility::Visibility;
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_ast::token::*;
use cyrusc_ast::*;
use cyrusc_diagcentral::DiagLevel;
use cyrusc_diagcentral::DiagLoc;

impl Parser {
    pub(crate) fn parse_statement(&mut self, toplevel: bool) -> Result<Stmt, Diag> {
        let modifiers = self.parse_unresolved_modifiers()?;
        let loc = self.current_token().loc.clone();

        if self.current_token_is(TokenKind::Function) {
            let func_modifiers = modifiers.into_func_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;
            return self.parse_func(func_modifiers);
        } else if self.current_token_is(TokenKind::Struct) {
            let struct_modifiers = modifiers.into_struct_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;
            return self.parse_struct(struct_modifiers, false);
        } else if self.current_token_is(TokenKind::Bits) {
            let struct_modifiers = modifiers.into_struct_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;
            return self.parse_struct(struct_modifiers, true);
        } else if self.current_token_is(TokenKind::Enum) {
            let enum_modifiers = modifiers.into_enum_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;
            return self.parse_enum(enum_modifiers);
        } else if self.current_token_is(TokenKind::Union) {
            let union_modifiers = modifiers.into_union_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;
            return self.parse_union(union_modifiers);
        } else if self.current_token_is(TokenKind::Typedef) {
            let typedef_modifiers =
                modifiers.into_typedef_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;
            return self.parse_typedef(typedef_modifiers.vis);
        } else if (self.current_token_is(TokenKind::Var) || self.current_token_is(TokenKind::Const)) && toplevel {
            return self.parse_global_variable(modifiers.clone());
        } else if self.current_token_is(TokenKind::Interface) {
            let interface_modifiers =
                modifiers.into_interface_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;
            return self.parse_interface(interface_modifiers.vis);
        }

        if !toplevel {
            // modifiers should not be used with non-top-level stmts.
            if modifiers != UnresolvedModifiers::default() {
                return Err(Diag {
                    kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        self.current_token().loc,
                        self.file_name.clone(),
                    ))),
                    hint: None,
                });
            }

            match self.current_token().kind {
                TokenKind::Var | TokenKind::Const => self.parse_variable(),
                TokenKind::Defer => self.parse_defer_stmt(),
                TokenKind::If => self.parse_if_stmt(),
                TokenKind::Return => self.parse_return(),
                TokenKind::For => self.parse_for_loop(),
                TokenKind::While => self.parse_while_loop(),
                TokenKind::Foreach => self.parse_foreach(),
                TokenKind::Break => self.parse_break(),
                TokenKind::Continue => self.parse_continue(),
                TokenKind::Switch => self.parse_switch(),
                TokenKind::Goto => self.parse_goto(),
                TokenKind::LeftBrace => {
                    let block_statement = self.parse_compound_stmt()?;
                    Ok(Stmt::BlockStmt(block_statement))
                }
                _ => {
                    if matches!(self.current_token().kind, TokenKind::Identifier { .. })
                        && self.peek_token_is(TokenKind::Colon)
                    {
                        return self.parse_label_statement();
                    }

                    self.parse_expr_statement()
                }
            }
        } else {
            match self.current_token().kind {
                TokenKind::Import => self.parse_import(),
                _ => {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            self.current_token().loc,
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
            }
        }
    }

    pub(crate) fn parse_goto(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();
        self.expect_current(TokenKind::Goto)?;
        let label = self.parse_identifier()?;
        self.expect_peek(TokenKind::Semicolon)?;

        Ok(Stmt::Goto(Goto {
            name: label,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    pub(crate) fn parse_label_statement(&mut self) -> Result<Stmt, Diag> {
        let name = self.parse_identifier()?;
        let start = name.span.start;
        let loc = name.loc.clone();
        self.expect_peek(TokenKind::Colon)?;

        Ok(Stmt::Label(Label {
            name,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    pub(crate) fn parse_compound_stmt(&mut self) -> Result<BlockStmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        if self.peek_token_is(TokenKind::EOF) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::MissingClosingBrace),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc,
                    self.file_name.clone(),
                ))),
                hint: None,
            });
        }

        self.expect_current(TokenKind::LeftBrace)?;

        let mut block_statement: Vec<Stmt> = Vec::new();

        if self.current_token_is(TokenKind::RightBrace) {
            // detected empty block statement
            return Ok(BlockStmt {
                exprs: block_statement,
                span: Span {
                    start,
                    end: self.current_token().span.end,
                },
                loc,
            });
        }

        loop {
            let statement = self.parse_statement(false)?;
            block_statement.push(statement);

            match self.peek_token().kind {
                TokenKind::RightBrace => break,
                _ => {
                    self.next_token();
                }
            }
        }

        self.expect_peek(TokenKind::RightBrace)?;
        let end = self.current_token().span.end;

        Ok(BlockStmt {
            exprs: block_statement,
            span: Span { start, end },
            loc,
        })
    }

    pub(crate) fn parse_func_params(&mut self) -> Result<FuncParams, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.expect_current(TokenKind::LeftParen)?;

        let mut variadic: Option<FuncVariadicParams> = None;
        let mut list: Vec<FuncParamKind> = Vec::new();
        let mut self_modifier_count: u32 = 0;

        while self.current_token().kind != TokenKind::RightParen {
            match self.current_token().kind {
                TokenKind::TripleDot => {
                    self.next_token(); // consume triple_dot

                    if self.current_token_is(TokenKind::Comma) {
                        return Err(Diag {
                            kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(
                                self.current_token().loc,
                                self.file_name.clone(),
                            ))),
                            hint: Some("Fixed parameters must be defined before the vargs.".to_string()),
                        });
                    }

                    variadic = Some(FuncVariadicParams::UntypedCStyle);
                    break;
                }
                TokenKind::Ampersand => {
                    self.next_token(); // ampersand
                    let identifier = self.parse_identifier()?;
                    self.next_token(); // consume identifier

                    if &identifier.name != "self" {
                        return Err(Diag {
                            kind: Box::new(ParserDiagKind::ExpectedSelfModifier(identifier.name.clone())),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                            hint: None,
                        });
                    }

                    self_modifier_count += 1;
                    list.push(FuncParamKind::SelfModifier(SelfModifier {
                        kind: SelfModifierKind::Referenced,
                        loc: loc.clone(),
                        span: Span::new(start, self.current_token().span.end),
                    }));
                }
                TokenKind::Identifier { name } => {
                    let param_loc = self.current_token().loc.clone();
                    let start = self.current_token().span.start;
                    let identifier = self.parse_identifier()?;
                    self.next_token(); // consume the identifier

                    if identifier.name == "self" {
                        self_modifier_count += 1;
                        list.push(FuncParamKind::SelfModifier(SelfModifier {
                            kind: SelfModifierKind::Copied,
                            loc: param_loc,
                            span: Span::new(start, self.current_token().span.end),
                        }));
                    } else {
                        // get the var type

                        let mut var_type: Option<TypeSpecifier> = None;
                        if self.current_token_is(TokenKind::Colon) {
                            self.next_token(); // consume the colon

                            if self.current_token_is(TokenKind::TripleDot) {
                                self.next_token(); // consume triple dot

                                let variadic_data_type = self.parse_type_specifier()?;
                                self.next_token();

                                variadic = Some(FuncVariadicParams::Typed(identifier, variadic_data_type));
                                continue;
                            } else {
                                var_type = Some(self.parse_type_specifier()?);
                                self.next_token();
                            }
                        }

                        list.push(FuncParamKind::FuncParam(FuncParam {
                            identifier: Identifier {
                                name: name,
                                span: self.current_token().span.clone(),
                                loc: param_loc.clone(),
                            },
                            ty: var_type,
                            span: Span {
                                start: start,
                                end: self.current_token().span.end,
                            },
                            loc: param_loc,
                        }));
                    }
                }
                _ => {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::ExpectedIdentifier),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                        hint: None,
                    });
                }
            }

            // after reading
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

        if self_modifier_count > 1 {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::SeveralSelfModifierDefinition),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                hint: None,
            });
        }

        self.expect_current(TokenKind::RightParen)?;

        Ok(FuncParams { list, variadic })
    }

    fn parse_expr_statement(&mut self) -> Result<Stmt, Diag> {
        let expr = self.parse_expr(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::Semicolon)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_enum_field(&mut self) -> Result<EnumVariant, Diag> {
        let loc = self.current_token().loc.clone();

        let variant_name = self.parse_identifier()?;
        self.next_token();

        let mut variant_fields: Vec<EnumValuedField> = Vec::new();

        if self.current_token_is(TokenKind::Comma) || self.current_token_is(TokenKind::RightBrace) {
            return Ok(EnumVariant::Identifier(variant_name));
        } else if self.current_token_is(TokenKind::Assign) {
            self.next_token(); // consume assign
            let value = self.parse_expr(Precedence::Lowest)?.0;
            self.next_token(); // consume last token of the expression
            return Ok(EnumVariant::Valued(variant_name, Box::new(value)));
        } else if self.current_token_is(TokenKind::LeftParen) {
            self.next_token(); // consume left paren

            loop {
                if self.current_token_is(TokenKind::RightParen) {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                        hint: Some(String::from(
                            "Consider to add a field to enum variant or remove the parenthesis.",
                        )),
                    });
                }

                let loc = self.current_token().loc.clone();
                let field_ty = self.parse_type_specifier()?;
                self.next_token();

                variant_fields.push(EnumValuedField { field_ty, loc });

                if self.current_token_is(TokenKind::RightParen) {
                    self.next_token();
                    break;
                } else {
                    self.expect_current(TokenKind::Comma)?;
                    continue;
                }
            }
        }

        if variant_fields.is_empty() {
            Ok(EnumVariant::Identifier(variant_name))
        } else {
            Ok(EnumVariant::Variant(variant_name, variant_fields))
        }
    }

    fn parse_union_field(&mut self) -> Result<UnionField, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let identifier = self.parse_identifier()?;
        self.next_token(); // consume identifier

        self.expect_current(TokenKind::Colon)?;

        let type_token = self.parse_type_specifier()?;
        self.next_token();

        let field = UnionField {
            identifier,
            ty: type_token,
            loc,
            span: Span {
                start,
                end: self.current_token().span.end,
            },
        };

        self.expect_current(TokenKind::Semicolon)?;
        Ok(field)
    }

    fn parse_union(&mut self, mut modifiers: UnionModifiers) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.expect_current(TokenKind::Union)?;

        modifiers.repr = self.parse_repr()?;

        let identifier = self.parse_identifier()?;
        self.next_token();

        let generic_params;
        if self.current_token_is(TokenKind::LessThan) {
            generic_params = Some(self.parse_generic_params()?);
        } else {
            generic_params = None;
        }

        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Stmt::Union(Union {
                identifier,
                methods: Vec::new(),
                fields: Vec::new(),
                generic_params,
                modifiers,
                loc,
                span: Span::new(start, self.current_token().span.end),
            }));
        }

        let mut fields: Vec<UnionField> = Vec::new();
        let mut methods: Vec<FuncDef> = Vec::new();

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
                            self.current_token().loc,
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
                TokenKind::Function => {
                    if let Stmt::FuncDef(method) = self.parse_func(FuncModifiers::default())? {
                        self.next_token(); // consume right brace
                        methods.push(method);
                    } else {
                        unreachable!();
                    }
                }
                TokenKind::Identifier { .. } => {
                    let field = self.parse_union_field()?;
                    fields.push(field);
                }
                _ => {
                    let modifiers = self.parse_unresolved_modifiers()?;
                    let loc = self.current_token().loc.clone();

                    if self.current_token_is(TokenKind::Function) {
                        let func_modifiers =
                            modifiers.into_method_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;

                        if let Stmt::FuncDef(method) = self.parse_func(func_modifiers)? {
                            self.next_token(); // consume right brace
                            methods.push(method);
                        } else {
                            return Err(Diag {
                                kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                                level: DiagLevel::Error,
                                location: Some(DiagLoc::new(SourceLoc::from_loc(
                                    self.current_token().loc,
                                    self.file_name.clone(),
                                ))),
                                hint: None,
                            });
                        }
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(Stmt::Union(Union {
            identifier,
            methods,
            fields,
            generic_params,
            modifiers,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    fn parse_enum(&mut self, mut modifiers: EnumModifiers) -> Result<Stmt, Diag> {
        let loc = self.current_token().loc.clone();
        let start = self.current_token().span.start;

        self.next_token(); // parse enum keyword

        modifiers.repr = self.parse_repr()?;

        let enum_name = self.parse_identifier()?;
        self.next_token(); // consume enum name

        let generic_params;
        if self.current_token_is(TokenKind::LessThan) {
            generic_params = Some(self.parse_generic_params()?);
        } else {
            generic_params = None;
        }

        self.expect_current(TokenKind::LeftBrace)?;

        let mut enum_fields: Vec<EnumVariant> = Vec::new();

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Stmt::Enum(Enum {
                identifier: enum_name,
                variants: enum_fields,
                generic_params,
                methods: Vec::new(),
                modifiers,
                loc,
                span: Span::new(start, self.current_token().span.end),
            }));
        }

        enum_fields.push(self.parse_enum_field()?);

        while self.current_token_is(TokenKind::Comma) {
            self.expect_current(TokenKind::Comma)?;

            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }

            enum_fields.push(self.parse_enum_field()?);
            if self.peek_token_is(TokenKind::RightBrace)
                || self.peek_token_is(TokenKind::Function)
                || self.peek_token_is(TokenKind::Extern)
                || self.peek_token_is(TokenKind::Public)
                || self.peek_token_is(TokenKind::Inline)
            {
                break;
            }
        }

        // consume optional comma at the end of the variant
        if self.current_token_is(TokenKind::Comma) {
            self.next_token();
        }

        let mut methods: Vec<FuncDef> = Vec::new();

        loop {
            match self.current_token().kind {
                TokenKind::Function => {
                    if let Stmt::FuncDef(method) = self.parse_func(FuncModifiers::default())? {
                        self.next_token(); // consume right brace
                        methods.push(method);
                    } else {
                        unreachable!();
                    }
                }
                _ => {
                    let modifiers = self.parse_unresolved_modifiers()?;
                    let loc = self.current_token().loc.clone();

                    if self.current_token_is(TokenKind::Function) {
                        let func_modifiers =
                            modifiers.into_method_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;

                        if let Stmt::FuncDef(method) = self.parse_func(func_modifiers)? {
                            self.next_token(); // consume right brace
                            methods.push(method);
                        } else {
                            return Err(Diag {
                                kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                                level: DiagLevel::Error,
                                location: Some(DiagLoc::new(SourceLoc::from_loc(
                                    self.current_token().loc,
                                    self.file_name.clone(),
                                ))),
                                hint: None,
                            });
                        }
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(Stmt::Enum(Enum {
            identifier: enum_name,
            variants: enum_fields,
            generic_params,
            methods,
            modifiers,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    fn parse_struct(&mut self, mut modifiers: StructModifiers, is_packed: bool) -> Result<Stmt, Diag> {
        let loc = self.current_token().loc.clone();
        let struct_start = self.current_token().span.start.clone();

        self.next_token(); // consume struct/bits token

        modifiers.repr = self.parse_repr()?;

        let struct_name = self.parse_identifier()?;
        self.next_token(); // consume struct name

        let generic_params;
        if self.current_token_is(TokenKind::LessThan) {
            generic_params = Some(self.parse_generic_params()?);
        } else {
            generic_params = None;
        }

        let mut impls: Vec<Identifier> = Vec::new();

        if self.current_token_is(TokenKind::Colon) {
            self.next_token();

            loop {
                match self.current_token().kind {
                    TokenKind::LeftBrace => {
                        self.next_token();
                        break;
                    }
                    TokenKind::EOF => {
                        return Err(Diag {
                            kind: Box::new(ParserDiagKind::MissingOpeningBrace),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                            hint: None,
                        });
                    }
                    TokenKind::Identifier { name: inherit_struct } => {
                        self.next_token();
                        impls.push(Identifier {
                            name: inherit_struct,
                            span: self.current_token().span.clone(),
                            loc: loc.clone(),
                        });
                    }
                    TokenKind::Comma => {
                        self.next_token();
                        continue;
                    }
                    _ => {
                        return Err(Diag {
                            kind: Box::new(ParserDiagKind::ExpectedIdentifier),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                            hint: None,
                        });
                    }
                }
            }
        } else {
            self.expect_current(TokenKind::LeftBrace)?;
        }

        let mut fields: Vec<StructField> = Vec::new();
        let mut methods: Vec<FuncDef> = Vec::new();

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
                            self.current_token().loc,
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
                TokenKind::Function => {
                    if let Stmt::FuncDef(method) = self.parse_func(FuncModifiers::default())? {
                        self.next_token(); // consume right brace
                        methods.push(method);
                    } else {
                        unreachable!();
                    }
                }
                TokenKind::Identifier { .. } => {
                    let field = self.parse_struct_field(None)?;
                    fields.push(field);
                }
                _ => {
                    let modifiers = self.parse_unresolved_modifiers()?;
                    let loc = self.current_token().loc.clone();

                    if matches!(self.current_token().kind, TokenKind::Identifier { .. }) {
                        let field_modifiers =
                            modifiers.into_field_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;

                        let field = self.parse_struct_field(Some(field_modifiers.vis))?;
                        fields.push(field);
                    } else {
                        let func_modifiers =
                            modifiers.into_method_modifiers(SourceLoc::from_loc(loc, self.file_name.clone()))?;

                        if let Stmt::FuncDef(method) = self.parse_func(func_modifiers)? {
                            self.next_token(); // consume right brace
                            methods.push(method);
                        } else {
                            return Err(Diag {
                                kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                                level: DiagLevel::Error,
                                location: Some(DiagLoc::new(SourceLoc::from_loc(
                                    self.current_token().loc,
                                    self.file_name.clone(),
                                ))),
                                hint: None,
                            });
                        }
                    }
                }
            }
        }

        Ok(Stmt::Struct(Struct {
            identifier: struct_name,
            generic_params,
            impls,
            modifiers,
            fields,
            methods,
            is_packed,
            loc,
            span: Span {
                start: struct_start,
                end: self.current_token().span.end,
            },
        }))
    }

    fn parse_struct_field(&mut self, vis: Option<Visibility>) -> Result<StructField, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let identifier = self.parse_identifier()?;
        self.next_token(); // consume identifier

        self.expect_current(TokenKind::Colon)?;

        let type_token = self.parse_type_specifier()?;
        self.next_token();

        let field = StructField {
            identifier,
            ty: type_token,
            vis: vis.unwrap_or_default(),
            loc,
            span: Span {
                start,
                end: self.current_token().span.end,
            },
        };

        self.expect_current(TokenKind::Semicolon)?;
        Ok(field)
    }

    fn parse_break(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token();
        if !self.current_token_is(TokenKind::Semicolon) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::MissingSemicolon),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                hint: None,
            });
        } else {
            Ok(Stmt::Break(Break {
                loc,
                span: Span::new(start, self.current_token().span.end),
            }))
        }
    }

    fn parse_continue(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token();
        if !self.current_token_is(TokenKind::Semicolon) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::MissingSemicolon),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                hint: None,
            });
        } else {
            Ok(Stmt::Continue(Continue {
                loc,
                span: Span::new(start, self.current_token().span.end),
            }))
        }
    }

    fn parse_import_module_path(&mut self, module_path: ModulePath) -> Result<ModulePath, Diag> {
        if self.current_token_is(TokenKind::LeftBrace) {
            self.next_token();

            let mut singles: Vec<ModuleSegmentSingle> = Vec::new();

            while !self.current_token_is(TokenKind::RightBrace) {
                // enable aliasing features for a single
                let identifier = self.parse_identifier()?;
                self.next_token();

                let mut renamed: Option<Identifier> = None;

                if self.current_token_is(TokenKind::As) {
                    self.next_token(); // consume as 

                    let renamed_identifier = self.parse_identifier()?;
                    self.next_token();
                    renamed = Some(renamed_identifier);
                }

                singles.push(ModuleSegmentSingle { identifier, renamed });

                if !self.current_token_is(TokenKind::RightBrace) {
                    self.expect_current(TokenKind::Comma)?;
                }
            }

            self.expect_current(TokenKind::RightBrace)?;

            let mut updated_module_path = module_path.clone();
            updated_module_path.segments.push(ModuleSegment::Single(singles));
            Ok(updated_module_path)
        } else {
            Ok(module_path)
        }
    }

    fn parse_interface(&mut self, vis: Visibility) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token();
        let identifier = self.parse_identifier()?;
        self.next_token();
        self.expect_current(TokenKind::LeftBrace)?;

        let mut methods: Vec<FuncDecl> = Vec::new();

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Stmt::Interface(Interface {
                identifier,
                methods,
                loc,
                vis,
                span: Span::new(start, self.current_token().span.end),
            }));
        }

        loop {
            match self.current_token().kind {
                TokenKind::Function => {
                    let func_decl = match self.parse_func(FuncModifiers::default())? {
                        Stmt::FuncDecl(func_decl) => func_decl,
                        _ => {
                            return Err(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                                location: Some(DiagLoc::new(SourceLoc::from_loc(
                                    self.current_token().loc,
                                    self.file_name.clone(),
                                ))),
                                hint: None,
                            });
                        }
                    };

                    methods.push(func_decl);

                    if !self.peek_token_is(TokenKind::RightBrace) {
                        self.expect_current(TokenKind::Semicolon)?;
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        self.expect_peek(TokenKind::RightBrace)?;

        Ok(Stmt::Interface(Interface {
            identifier,
            methods,
            loc,
            vis,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    fn parse_import(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token(); // consume import keyword

        let mut paths: Vec<ModulePath> = Vec::new();

        if self.current_token_is(TokenKind::LeftParen) {
            self.expect_current(TokenKind::LeftParen)?;

            loop {
                let mut module_path = self.parse_module_path()?;
                module_path = self.parse_import_module_path(module_path.clone())?;
                paths.push(module_path);

                match self.current_token().kind {
                    TokenKind::RightParen => {
                        break;
                    }
                    TokenKind::Comma => {
                        self.next_token();

                        if self.current_token_is(TokenKind::RightParen) {
                            break;
                        } else {
                            continue;
                        }
                    }
                    _ => {
                        return Err(Diag {
                            kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind)),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(
                                self.current_token().loc,
                                self.file_name.clone(),
                            ))),
                            hint: None,
                        });
                    }
                }
            }

            self.expect_current(TokenKind::RightParen)?;
        } else {
            let mut module_path = self.parse_module_path()?;
            module_path = self.parse_import_module_path(module_path.clone())?;
            paths = vec![module_path];
        }

        return Ok(Stmt::Import(Import {
            paths,
            span: Span {
                start,
                end: self.current_token().span.end,
            },
            loc,
        }));
    }

    fn parse_for_loop_body(&mut self) -> Result<Box<BlockStmt>, Diag> {
        let loc = self.current_token().loc.clone();

        let body: Box<BlockStmt>;
        if self.current_token_is(TokenKind::LeftBrace) {
            body = Box::new(self.parse_compound_stmt()?);

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            }
        } else {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::MissingOpeningBrace),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                hint: None,
            });
        }
        Ok(body)
    }

    fn parse_foreach(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.end;
        let loc = self.current_token().loc.clone();

        self.next_token(); // consume foreach
        self.expect_current(TokenKind::LeftParen)?;

        let item_identifier = self.parse_identifier()?;
        self.next_token();

        let mut index_identifier: Option<Identifier> = None;

        if self.current_token_is(TokenKind::Comma) {
            self.next_token(); // consume comma
            index_identifier = Some(self.parse_identifier()?);
            self.next_token();
        }

        self.expect_current(TokenKind::In)?;

        let expr = self.parse_expr(Precedence::Lowest)?.0;
        self.next_token();

        self.expect_current(TokenKind::RightParen)?;

        let body = self.parse_compound_stmt()?;

        Ok(Stmt::Foreach(Foreach {
            item: item_identifier,
            index: index_identifier,
            expr,
            body: Box::new(body),
            span: Span::new(start, self.current_token().span.end),
            loc,
        }))
    }

    fn parse_while_loop(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token(); // consume while token
        self.expect_current(TokenKind::LeftParen)?;
        let condition = self.parse_expr(Precedence::Lowest)?.0;
        self.next_token();
        self.expect_current(TokenKind::RightParen)?;

        let body = self.parse_compound_stmt()?;

        Ok(Stmt::While(While {
            condition,
            body: Box::new(body),
            span: Span::new(start, self.current_token().span.end),
            loc,
        }))
    }

    fn parse_for_loop(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token(); // consume for token

        // Check for non-conditional for loop
        if self.current_token_is(TokenKind::LeftBrace) {
            let body: Box<BlockStmt>;
            if self.current_token_is(TokenKind::LeftBrace) {
                body = Box::new(self.parse_compound_stmt()?);

                if self.peek_token_is(TokenKind::Semicolon) {
                    self.next_token();
                }
            } else {
                return Err(Diag {
                    kind: Box::new(ParserDiagKind::MissingOpeningBrace),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                    hint: None,
                });
            }

            return Ok(Stmt::For(For {
                initializer: None,
                condition: None,
                increment: None,
                body,
                span: Span {
                    start,
                    end: self.current_token().span.end,
                },
                loc,
            }));
        }

        self.expect_current(TokenKind::LeftParen)?;

        let mut initializer: Option<Variable> = None;
        if !self.current_token_is(TokenKind::Semicolon) {
            if let Stmt::Variable(var) = self.parse_variable()? {
                initializer = Some(var);
            }
        }
        self.expect_current(TokenKind::Semicolon)?;

        // for loop with only initializer expression
        if self.peek_token_is(TokenKind::LeftBrace) {
            self.expect_current(TokenKind::RightParen)?;

            let body = self.parse_for_loop_body()?;
            return Ok(Stmt::For(For {
                initializer,
                condition: None,
                increment: None,
                body,
                span: Span {
                    start,
                    end: self.current_token().span.end,
                },
                loc,
            }));
        }

        let condition = self.parse_expr(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::Semicolon)?;
        self.next_token();

        let mut increment: Option<Expr> = None;
        if !self.current_token_is(TokenKind::RightParen) {
            increment = Some(self.parse_expr(Precedence::Lowest)?.0);
            self.next_token(); // consume increment token
        }

        self.expect_current(TokenKind::RightParen)?;
        let body = self.parse_for_loop_body()?;

        Ok(Stmt::For(For {
            initializer,
            condition: Some(condition),
            increment,
            body,
            span: Span {
                start,
                end: self.current_token().span.end,
            },
            loc,
        }))
    }

    fn parse_export_pattern(&mut self) -> Result<ExportPattern, Diag> {
        match self.current_token().kind {
            TokenKind::Identifier { .. } => {
                let ident = self.parse_identifier()?;
                Ok(ExportPattern::Identifier(ident))
            }
            TokenKind::LeftParen => {
                self.next_token();
                let mut patterns = Vec::new();

                loop {
                    let pattern = self.parse_export_pattern()?;
                    self.next_token();

                    patterns.push(pattern);

                    match self.current_token().kind {
                        TokenKind::Comma => {
                            self.next_token();
                            continue;
                        }
                        TokenKind::RightParen => {
                            // self.next_token();
                            break;
                        }
                        _ => {
                            return Err(Diag {
                                kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind.clone())),
                                level: DiagLevel::Error,
                                location: Some(DiagLoc::new(SourceLoc::from_loc(
                                    self.current_token().loc.clone(),
                                    self.file_name.clone(),
                                ))),
                                hint: Some("Expected ',' or ')' in tuple pattern.".to_string()),
                            });
                        }
                    }
                }

                Ok(ExportPattern::Tuple(patterns))
            }
            _ => Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind.clone())),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: Some("Expected identifier or '('.".to_string()),
            }),
        }
    }

    fn parse_grouped_tuple_export(&mut self, is_const: bool) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.expect_current(TokenKind::LeftParen)?;

        if self.current_token_is(TokenKind::RightParen) {
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

        let mut items = Vec::new();

        loop {
            let pattern = self.parse_export_pattern()?;
            self.next_token();

            items.push(pattern);

            match self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                    continue;
                }
                TokenKind::RightParen => {
                    break;
                }
                _ => {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::InvalidToken(self.current_token().kind.clone())),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            self.current_token().loc.clone(),
                            self.file_name.clone(),
                        ))),
                        hint: Some("Expected ',' or ')' in tuple export.".to_string()),
                    });
                }
            }
        }

        let pattern = ExportPattern::Tuple(items);

        if self.peek_token_is(TokenKind::Semicolon) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::IncompleteVariableDeclaration),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: None,
            });
        }

        let mut variable_type: Option<TypeSpecifier> = None;
        if self.peek_token_is(TokenKind::Colon) {
            self.next_token(); // consume last token
            self.next_token(); // consume colon
            variable_type = Some(self.parse_type_specifier()?);
        }

        if self.peek_token_is(TokenKind::Semicolon) {
            return Ok(Stmt::ExportTuple(ExportTuple {
                pattern,
                ty: variable_type,
                rhs: None,
                is_const,
                span: Span {
                    start,
                    end: self.current_token().span.end,
                },
                loc,
            }));
        }

        self.expect_peek(TokenKind::Assign)?;
        self.next_token(); // consume assign

        let (expr, span) = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(TokenKind::Semicolon)?;

        Ok(Stmt::ExportTuple(ExportTuple {
            pattern,
            rhs: Some(expr),
            ty: variable_type,
            is_const,
            span: Span { start, end: span.end },
            loc,
        }))
    }

    fn parse_variable(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let is_const;
        if self.current_token_is(TokenKind::Const) {
            self.next_token();
            is_const = true;
        } else {
            self.expect_current(TokenKind::Var)?;
            is_const = false;
        }

        // considered as grouped tuple export
        if self.current_token_is(TokenKind::LeftParen) {
            return self.parse_grouped_tuple_export(is_const);
        }

        let identifier = self.parse_identifier()?;
        self.next_token();

        if self.current_token_is(TokenKind::Semicolon) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::IncompleteVariableDeclaration),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                hint: None,
            });
        }

        let mut variable_type: Option<TypeSpecifier> = None;
        if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume the colon

            variable_type = Some(self.parse_type_specifier()?);
            self.next_token();
        }

        if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Stmt::Variable(Variable {
                identifier,
                ty: variable_type,
                rhs: None,
                is_const,
                span: Span {
                    start,
                    end: self.current_token().span.end,
                },
                loc,
            }));
        }
        self.expect_current(TokenKind::Assign)?;

        let (expr, span) = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(TokenKind::Semicolon)?;

        Ok(Stmt::Variable(Variable {
            identifier,
            rhs: Some(expr),
            ty: variable_type,
            is_const,
            span: Span { start, end: span.end },
            loc,
        }))
    }

    fn parse_func(&mut self, modifiers: FuncModifiers) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.expect_current(TokenKind::Function)?;

        let func_name = self.parse_identifier()?; // export the name of the function
        self.next_token(); // consume the name of the identifier

        let generic_params = if self.current_token_is(TokenKind::LessThan) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        let params = self.parse_func_params()?;

        let return_type: Option<TypeSpecifier>;

        // parse return type
        if self.current_token_is(TokenKind::LeftBrace) {
            return_type = None;
        } else if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Stmt::FuncDecl(FuncDecl {
                identifier: func_name,
                generic_params,
                params,
                return_type: None,
                modifiers,
                renamed_as: None,
                span: Span {
                    start,
                    end: self.current_token().span.end,
                },
                loc,
            }));
        } else if self.current_token_is(TokenKind::As) {
            self.next_token();
            let renamed_as = self.parse_identifier()?;
            self.next_token();

            return Ok(Stmt::FuncDecl(FuncDecl {
                identifier: func_name,
                generic_params,
                params,
                return_type: None,
                modifiers,
                renamed_as: Some(renamed_as),
                span: Span {
                    start,
                    end: self.current_token().span.end,
                },
                loc,
            }));
        } else {
            return_type = Some(self.parse_type_specifier()?);
            self.next_token();
        }

        if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Stmt::FuncDecl(FuncDecl {
                identifier: func_name,
                generic_params,
                params,
                return_type,
                modifiers,
                renamed_as: None,
                span: Span {
                    start,
                    end: self.current_token().span.end,
                },
                loc,
            }));
        } else if self.current_token_is(TokenKind::As) {
            self.next_token();

            // parse renamed func decl
            let renamed_as = self.parse_identifier()?;

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            } else if self.peek_token_is(TokenKind::LeftBrace) {
                return Err(Diag {
                    kind: Box::new(ParserDiagKind::InvalidToken(self.peek_token().kind)),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                    hint: Some(String::from(
                        "FuncDecl does not accept a body. Use a semicolon `;` instead of a body `{ ... }`.",
                    )),
                });
            }

            return Ok(Stmt::FuncDecl(FuncDecl {
                identifier: func_name,
                generic_params,
                params,
                return_type,
                modifiers,
                renamed_as: Some(renamed_as),
                span: Span {
                    start,
                    end: self.current_token().span.end,
                },
                loc,
            }));
        }

        let body = Box::new(self.parse_compound_stmt()?);
        let end = self.current_token().span.end;

        return Ok(Stmt::FuncDef(FuncDef {
            identifier: func_name,
            generic_params,
            params,
            body,
            return_type,
            modifiers,
            span: Span { start, end },
            loc,
        }));
    }

    fn parse_return(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token(); // consume return token

        if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Stmt::Return(Return {
                argument: None,
                span: Span::new(start, self.current_token().span.end),
                loc,
            }));
        }

        let argument = self.parse_expr(Precedence::Lowest)?.0;
        self.next_token();

        if !self.current_token_is(TokenKind::Semicolon) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::MissingSemicolon),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                hint: None,
            });
        }

        let end = self.peek_token().span.end;

        Ok(Stmt::Return(Return {
            argument: Some(argument),
            span: Span { start, end },
            loc,
        }))
    }

    fn parse_global_variable(&mut self, modifiers: UnresolvedModifiers) -> Result<Stmt, Diag> {
        let loc = self.current_token().loc.clone();
        let start = self.current_token().span.start;

        let is_const;
        if self.current_token_is(TokenKind::Const) {
            self.next_token();
            is_const = true;
        } else {
            self.expect_current(TokenKind::Var)?;
            is_const = false;
        }

        let global_var_modifiers =
            modifiers.into_global_var_modifiers(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))?;

        let identifier = self.parse_identifier()?;
        self.next_token();

        let mut type_specifier: Option<TypeSpecifier> = None;
        if self.current_token_is(TokenKind::Colon) {
            self.next_token();
            type_specifier = Some(self.parse_type_specifier()?);
            self.next_token();
        }

        let expr = {
            if self.current_token_is(TokenKind::Assign) {
                self.next_token();
                let expr = Some(self.parse_expr(Precedence::Lowest)?.0);
                self.next_token();
                expr
            } else {
                None
            }
        };

        Ok(Stmt::GlobalVar(GlobalVar {
            identifier,
            type_specifier,
            expr,
            is_const,
            modifiers: global_var_modifiers,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    fn parse_typedef(&mut self, vis: Visibility) -> Result<Stmt, Diag> {
        let loc = self.current_token().loc.clone();
        let start = self.current_token().span.start;

        self.expect_current(TokenKind::Typedef)?;

        let identifier = self.parse_identifier()?;
        self.next_token();

        let generic_params;
        if self.current_token_is(TokenKind::LessThan) {
            generic_params = Some(self.parse_generic_params()?);
        } else {
            generic_params = None;
        }

        self.expect_current(TokenKind::Assign)?;
        let type_specifier = self.parse_type_specifier()?;
        self.next_token();
        Ok(Stmt::Typedef(Typedef {
            vis,
            identifier,
            type_specifier,
            generic_params,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    fn parse_switch(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token();
        self.expect_current(TokenKind::LeftParen)?;
        let operand = self.parse_expr(Precedence::Lowest)?.0;
        self.next_token();
        self.expect_current(TokenKind::RightParen)?;
        self.expect_current(TokenKind::LeftBrace)?;

        let mut cases: Vec<SwitchCase> = Vec::new();
        let mut default_case: Option<BlockStmt> = None;

        fn parse_pattern(this: &mut Parser) -> Result<SwitchCasePattern, Diag> {
            let case_pattern = if this.current_token_is(TokenKind::Dot) {
                this.next_token();
                let identifier = this.parse_identifier()?;
                this.next_token();

                if this.current_token_is(TokenKind::LeftParen) {
                    this.next_token();
                    let mut items: Vec<Identifier> = Vec::new();
                    loop {
                        let item = this.parse_identifier()?;
                        this.next_token();
                        items.push(item);
                        if this.current_token_is(TokenKind::RightParen) {
                            break;
                        } else {
                            this.expect_current(TokenKind::Comma)?;
                        }
                    }
                    this.expect_current(TokenKind::RightParen)?;
                    SwitchCasePattern::EnumVariant(identifier, items)
                } else {
                    SwitchCasePattern::Identifier(identifier)
                }
            } else {
                let start = this.current_token().span.start;
                let loc = this.current_token().loc.clone();
                let expr = this.parse_expr(Precedence::Prefix)?.0;
                this.next_token();

                if this.current_token_is(TokenKind::TripleDot) {
                    // range (exclusive)
                    this.next_token();
                    let lower = expr;
                    let upper = this.parse_expr(Precedence::Prefix)?.0;
                    this.next_token();

                    SwitchCasePattern::Range(Range {
                        lower,
                        upper,
                        inclusive_upper: false,
                        loc,
                        span: Span::new(start, this.current_token().span.end),
                    })
                } else if this.current_token_is(TokenKind::DoubleDot) && this.peek_token_is(TokenKind::Assign) {
                    // range (inclusive)
                    this.next_token();
                    this.next_token();
                    let lower = expr;
                    let upper = this.parse_expr(Precedence::Prefix)?.0;
                    this.next_token();

                    SwitchCasePattern::Range(Range {
                        lower,
                        upper,
                        inclusive_upper: true,
                        loc,
                        span: Span::new(start, this.current_token().span.end),
                    })
                } else {
                    SwitchCasePattern::Expr(expr)
                }
            };
            Ok(case_pattern)
        }

        loop {
            if self.current_token_is(TokenKind::Case) {
                let case_loc = self.current_token().loc.clone();
                let case_start = self.current_token().span.start;
                self.next_token();

                let mut patterns: Vec<SwitchCasePattern> = Vec::new();
                loop {
                    let pattern = parse_pattern(self)?;
                    patterns.push(pattern);

                    if self.current_token_is(TokenKind::Pipe) {
                        self.next_token();
                        continue;
                    } else {
                        break;
                    }
                }

                self.expect_current(TokenKind::FatArrow)?;

                let case_body = self.parse_compound_stmt()?;
                self.next_token();

                cases.push(SwitchCase {
                    patterns,
                    body: case_body,
                    span: Span::new(case_start, self.current_token().span.end),
                    loc: case_loc,
                });
            } else if self.current_token_is(TokenKind::Default) {
                self.next_token();
                self.expect_current(TokenKind::FatArrow)?;
                let case_body = self.parse_compound_stmt()?;
                self.next_token();
                default_case = Some(case_body);
                break;
            } else {
                break;
            }
        }

        if !self.current_token_is(TokenKind::RightBrace) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::MissingClosingBrace),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                hint: None,
            });
        }

        Ok(Stmt::Switch(Switch {
            operand,
            cases,
            default_case,
            span: Span::new(start, self.current_token().span.end),
            loc,
        }))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let mut branches: Vec<If> = Vec::new();
        let mut alternate: Option<Box<BlockStmt>> = None;

        self.expect_current(TokenKind::If)?;
        self.expect_current(TokenKind::LeftParen)?;

        let condition = self.parse_expr(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::RightParen)?;
        self.next_token(); // consume right paren

        let consequent = Box::new(self.parse_compound_stmt()?);

        if self.peek_token_is(TokenKind::Else) {
            self.next_token(); // consume right brace
        }

        while self.current_token_is(TokenKind::Else) {
            self.next_token(); // consume else token

            if self.current_token_is(TokenKind::If) {
                let else_if_start = self.current_token().span.start;
                self.next_token(); // consume if token

                self.expect_current(TokenKind::LeftParen)?;
                let (condition, _) = self.parse_expr(Precedence::Lowest)?;
                self.next_token(); // consume last token of the expression
                self.expect_current(TokenKind::RightParen)?;

                let consequent = Box::new(self.parse_compound_stmt()?);

                if self.peek_token_is(TokenKind::Else) {
                    self.next_token(); // consume else token
                }

                let end = self.current_token().span.end;

                branches.push(If {
                    condition,
                    consequent,
                    branches: Vec::new(),
                    alternate: None,
                    span: Span {
                        start: else_if_start,
                        end,
                    },
                    loc: loc.clone(),
                });
            } else {
                // parse alternate
                alternate = Some(Box::new(self.parse_compound_stmt()?));

                if !(self.current_token_is(TokenKind::RightBrace) || self.current_token_is(TokenKind::EOF)) {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::MissingClosingBrace),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                        hint: None,
                    });
                }
            }
        }

        if !self.current_token_is(TokenKind::RightBrace) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::MissingClosingBrace),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                hint: None,
            });
        }

        let end = self.current_token().span.end;

        Ok(Stmt::If(If {
            condition,
            consequent,
            branches,
            alternate,
            span: Span { start, end },
            loc,
        }))
    }

    fn parse_defer_stmt(&mut self) -> Result<Stmt, Diag> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc;

        self.next_token();
        let stmt = self.parse_statement(false)?;

        Ok(Stmt::Defer(Defer {
            operand: Box::new(stmt),
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }
}
