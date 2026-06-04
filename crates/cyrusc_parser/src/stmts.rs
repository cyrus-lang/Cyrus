// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::Diag;
use crate::Parser;
use crate::diagnostics::ParserDiagKind;
use crate::modifiers::UnresolvedModifiers;
use crate::prec::Precedence;
use cyrusc_ast::abi::Visibility;
use cyrusc_ast::modifiers::EnumModifiers;
use cyrusc_ast::modifiers::FuncModifiers;
use cyrusc_ast::modifiers::StructModifiers;
use cyrusc_ast::modifiers::UnionModifiers;
use cyrusc_ast::*;
use cyrusc_source_loc::Loc;
use cyrusc_tokens::TokenKind;

impl<'source_file> Parser<'source_file> {
    pub(crate) fn parse_stmt(
        &mut self,
        grouped_modifiers: Option<UnresolvedModifiers>,
        toplevel: bool,
    ) -> Result<Vec<ASTStmt>, Diag> {
        if self.current_token_is(TokenKind::At) {
            let mut builtin = self.parse_builtin(toplevel)?;

            if let Builtin::BuiltinFunc(builtin_func) = &mut builtin {
                // reform builtin: syntactic builtin
                if !self.peek_token_is(TokenKind::Semicolon) {
                    self.expect_current(TokenKind::RightParen)?;

                    let stmts = self.parse_stmt(grouped_modifiers, toplevel)?;

                    if !stmts.len() == 1 {
                        return Err(self.error_invalid_token());
                    }

                    let block = BuiltinBlock {
                        name: builtin_func.name.clone(),
                        args: builtin_func.args.clone(),
                        block: Box::new(ASTBlockStmt {
                            stmts,
                            loc: builtin_func.loc,
                        }),
                        is_toplevel: toplevel,
                        loc: builtin_func.loc,
                    };

                    builtin = Builtin::BuiltinBlock(block);
                } else {
                    self.expect_current(TokenKind::RightParen)?;
                }
            }

            return Ok(vec![ASTStmt::Builtin(builtin)]);
        }

        let modifiers = grouped_modifiers.clone().unwrap_or(self.parse_unresolved_modifiers()?);
        let loc = self.current_token().loc;

        if toplevel && self.current_token_is(TokenKind::LeftBrace) {
            if grouped_modifiers.is_some() {
                return Err(self.error_at_current(ParserDiagKind::GroupedModifiersCannotBeNested));
            }

            return self.parse_grouped_modifiers(Some(modifiers), toplevel);
        }

        if self.current_token_is(TokenKind::Module) {
            let module_decl_modifiers = modifiers.into_module_decl_modifiers(loc)?;
            return Ok(vec![self.parse_module_decl(module_decl_modifiers.vis)?]);
        } else if self.current_token_is(TokenKind::Function) {
            let func_modifiers = modifiers.into_func_modifiers(loc)?;
            return Ok(vec![self.parse_func(func_modifiers)?]);
        } else if self.current_token_is(TokenKind::Struct) {
            let struct_modifiers = modifiers.into_struct_modifiers(loc)?;
            return Ok(vec![self.parse_struct(struct_modifiers, false)?]);
        } else if self.current_token_is(TokenKind::Enum) {
            let enum_modifiers = modifiers.into_enum_modifiers(loc)?;
            return Ok(vec![self.parse_enum(enum_modifiers)?]);
        } else if self.current_token_is(TokenKind::Union) {
            let union_modifiers = modifiers.into_union_modifiers(loc)?;
            return Ok(vec![self.parse_union(union_modifiers)?]);
        } else if self.current_token_is(TokenKind::Typedef) {
            let typedef_modifiers = modifiers.into_typedef_modifiers(loc)?;
            return Ok(vec![self.parse_typedef(typedef_modifiers.vis)?]);
        } else if (self.current_token_is(TokenKind::Var) || self.current_token_is(TokenKind::Const)) && toplevel {
            return Ok(vec![self.parse_global_var(modifiers.clone())?]);
        } else if self.current_token_is(TokenKind::Interface) {
            let interface_modifiers = modifiers.into_interface_modifiers(loc)?;
            return Ok(vec![self.parse_interface(interface_modifiers.vis)?]);
        }

        if !toplevel {
            // modifiers should not be used with non-top-level stmts.
            if modifiers != UnresolvedModifiers::default() {}

            let stmt = match self.current_token().kind {
                TokenKind::Var | TokenKind::Const => self.parse_variable(),
                TokenKind::Defer => self.parse_defer(),
                TokenKind::If => self.parse_if(),
                TokenKind::Return => self.parse_return(),
                TokenKind::For => self.parse_for_loop(),
                TokenKind::While => self.parse_while_loop(),
                TokenKind::Foreach => self.parse_foreach(),
                TokenKind::Break => self.parse_break(),
                TokenKind::Continue => self.parse_continue(),
                TokenKind::Switch => self.parse_switch(),
                TokenKind::Goto => self.parse_goto(),
                TokenKind::LeftBrace => {
                    let block_stmt = self.parse_block()?;
                    Ok(ASTStmt::BlockStmt(block_stmt))
                }
                _ => {
                    if matches!(self.current_token().kind, TokenKind::Ident { .. })
                        && self.peek_token_is(TokenKind::Colon)
                    {
                        return Ok(vec![self.parse_label()?]);
                    }

                    self.parse_expr_stmt()
                }
            };

            Ok(vec![stmt?])
        } else {
            match self.current_token().kind {
                TokenKind::Import => Ok(vec![self.parse_import()?]),
                _ => {
                    return Err(self.error_invalid_token());
                }
            }
        }
    }

    fn parse_module_decl(&mut self, vis: Visibility) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.expect_current(TokenKind::Module)?;

        let ident = self.parse_ident()?;
        self.next_token();

        let stmts = self.parse_toplevel_stmts()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::ModuleDecl(ASTModuleDecl {
            ident,
            vis,
            stmts,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    pub(crate) fn parse_builtin(&mut self, toplevel: bool) -> Result<Builtin, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume at

        let ident = self.parse_ident()?;
        self.next_token();

        self.must_be_left_paren()?;

        let args = self.parse_expr_series(TokenKind::RightParen)?;
        self.must_be_right_paren()?;

        if self.peek_token_is(TokenKind::LeftBrace) {
            self.next_token();

            let block = {
                if toplevel {
                    let end = self.current_token().loc.end;

                    ASTBlockStmt {
                        stmts: self.parse_toplevel_stmts()?,
                        loc: Loc::new(self.file_id(), line, column, start, end),
                    }
                } else {
                    self.parse_block()?
                }
            };

            let end = self.current_token().loc.end;

            Ok(Builtin::BuiltinBlock(BuiltinBlock {
                name: ident,
                args,
                block: Box::new(block),
                loc: Loc::new(self.file_id(), line, column, start, end),
                is_toplevel: toplevel,
            }))
        } else {
            let end = self.current_token().loc.end;

            Ok(Builtin::BuiltinFunc(BuiltinFunc {
                name: ident,
                args,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }))
        }
    }

    fn parse_toplevel_stmts(&mut self) -> Result<Vec<ASTStmt>, Diag> {
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Vec::new());
        }

        let mut stmts: Vec<ASTStmt> = Vec::new();

        loop {
            let inner_stmts = self.parse_stmt(None, true)?;
            self.next_token();

            stmts.extend(inner_stmts);

            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }
        }

        self.must_be_right_brace()?;
        Ok(stmts)
    }

    fn parse_grouped_modifiers(
        &mut self,
        grouped_modifiers: Option<UnresolvedModifiers>,
        toplevel: bool,
    ) -> Result<Vec<ASTStmt>, Diag> {
        if !toplevel {
            return Err(self.error_at_current(ParserDiagKind::InvalidGroupedModifiers));
        }

        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Vec::new());
        }

        let mut group_stmts: Vec<ASTStmt> = Vec::new();

        loop {
            let inner_modifiers = self.parse_unresolved_modifiers()?;
            if inner_modifiers != UnresolvedModifiers::default() {
                return Err(self.error_at_current(ParserDiagKind::GroupedModifiersCannotBeNested));
            }

            let stmts = self.parse_stmt(grouped_modifiers.clone(), toplevel)?;
            self.next_token();
            group_stmts.extend(stmts);

            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }
        }

        self.must_be_right_brace()?;
        Ok(group_stmts)
    }

    pub(crate) fn parse_goto(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.expect_current(TokenKind::Goto)?;
        let label = self.parse_ident()?;

        self.expect_peek_semicolon()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Goto(ASTGotoStmt {
            name: label,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    pub(crate) fn parse_label(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let name = self.parse_ident()?;
        self.expect_peek(TokenKind::Colon)?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Label(ASTLabelStmt {
            name,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    pub(crate) fn parse_block(&mut self) -> Result<ASTBlockStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        if self.peek_token_is(TokenKind::EOF) {
            return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
        }

        self.expect_current(TokenKind::LeftBrace)?;

        let mut block_stmt: Vec<ASTStmt> = Vec::new();

        // detect empty block
        if self.current_token_is(TokenKind::RightBrace) {
            let end = self.current_token().loc.end;

            return Ok(ASTBlockStmt {
                stmts: block_stmt,
                loc: Loc::new(self.file_id(), line, column, start, end),
            });
        }

        loop {
            let stmt = self.parse_stmt(None, false)?.first().unwrap().clone();
            block_stmt.push(stmt);

            match self.peek_token().kind {
                TokenKind::RightBrace => break,
                _ => {
                    self.next_token();
                }
            }
        }

        self.expect_peek(TokenKind::RightBrace)?;

        let end = self.current_token().loc.end;

        Ok(ASTBlockStmt {
            stmts: block_stmt,
            loc: Loc::new(self.file_id(), line, column, start, end),
        })
    }

    pub(crate) fn parse_func_params(&mut self) -> Result<FuncParams, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.expect_current(TokenKind::LeftParen)?;

        let params_start_token = self.current_token();

        let mut variadic: Option<FuncVariadicParam> = None;
        let mut list: Vec<FuncParamKind> = Vec::new();
        let mut self_modifier_count: u32 = 0;

        while self.current_token().kind != TokenKind::RightParen {
            match self.current_token().kind {
                TokenKind::TripleDot => {
                    self.next_token(); // consume triple_dot

                    if self.current_token_is(TokenKind::Comma) {
                        return Err(self.error_with_hint(
                            &self.current_token(),
                            ParserDiagKind::InvalidToken(self.current_token().kind),
                            "Fixed parameters must be defined before the vargs.",
                        ));
                    }

                    variadic = Some(FuncVariadicParam::UntypedCStyle);
                    break;
                }
                TokenKind::Ampersand => {
                    let token = self.current_token();
                    self.next_token(); // ampersand

                    let mutability = {
                        if self.current_token_is(TokenKind::Const) {
                            self.next_token();
                            Mutability::Const
                        } else {
                            Mutability::Var
                        }
                    };

                    let ident = self.parse_ident()?;
                    self.next_token(); // consume ident

                    if &ident.value != "self" {
                        return Err(
                            self.error_at_token(&token, ParserDiagKind::ExpectedSelfModifier(ident.value.clone()))
                        );
                    }

                    let end = self.current_token().loc.end;

                    list.push(FuncParamKind::SelfModifier(SelfModifier {
                        kind: SelfModifierKind::Referenced,
                        mutability,
                        loc: Loc::new(self.file_id(), line, column, start, end),
                    }));

                    self_modifier_count += 1;
                }
                TokenKind::Const | TokenKind::Ident(_) => {
                    let start = self.current_token().loc.start;
                    let line = self.current_token().loc.line;

                    let mutability = {
                        if self.current_token_is(TokenKind::Const) {
                            self.next_token();
                            Mutability::Const
                        } else {
                            Mutability::Var
                        }
                    };

                    let ident = self.parse_ident()?;
                    self.next_token(); // consume the ident

                    if ident.value == "self" {
                        let end = self.current_token().loc.end;

                        list.push(FuncParamKind::SelfModifier(SelfModifier {
                            kind: SelfModifierKind::Copied,
                            mutability,
                            loc: Loc::new(self.file_id(), line, column, start, end),
                        }));

                        self_modifier_count += 1;
                    } else {
                        let mut var_type: Option<TypeSpecifier> = None;

                        if self.current_token_is(TokenKind::Colon) {
                            self.next_token(); // consume the colon

                            if self.current_token_is(TokenKind::TripleDot) {
                                self.next_token(); // consume triple dot

                                let variadic_data_type = self.parse_type_specifier()?;
                                self.next_token();

                                variadic = Some(FuncVariadicParam::Typed(ident, variadic_data_type));
                                continue;
                            } else {
                                var_type = Some(self.parse_type_specifier()?);
                                self.next_token();
                            }
                        }

                        let end = self.current_token().loc.end;

                        list.push(FuncParamKind::FuncParam(FuncParam {
                            ident,
                            ty: var_type,
                            mutability,
                            loc: Loc::new(self.file_id(), line, column, start, end),
                        }));
                    }
                }
                _ => {
                    return Err(self.error_at_current(ParserDiagKind::ExpectedIdentifier {
                        got: self.current_token().kind.to_string(),
                    }));
                }
            }

            match &self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                }
                TokenKind::RightParen => {
                    break;
                }
                _ => {
                    return Err(self.error_at_current(ParserDiagKind::MissingComma));
                }
            }
        }

        if self_modifier_count > 1 {
            return Err(self.error_at_token(&params_start_token, ParserDiagKind::SeveralSelfModifierUsed));
        }

        self.expect_current(TokenKind::RightParen)?;

        Ok(FuncParams { list, variadic })
    }

    fn parse_expr_stmt(&mut self) -> Result<ASTStmt, Diag> {
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek_semicolon()?;
        Ok(ASTStmt::Expr(expr))
    }

    pub(crate) fn parse_enum_variant(&mut self) -> Result<EnumVariant, Diag> {
        let variant_name = self.parse_ident()?;
        self.next_token();

        if self.current_token_is(TokenKind::Comma) || self.current_token_is(TokenKind::RightBrace) {
            return Ok(EnumVariant::Unit(variant_name));
        }

        if self.current_token_is(TokenKind::Assign) {
            self.next_token(); // consume assign

            let value = self.parse_expr(Precedence::Lowest)?;
            self.next_token(); // consume last token of the expression

            Ok(EnumVariant::Valued {
                ident: variant_name,
                value: Box::new(value),
            })
        } else if self.current_token_is(TokenKind::LeftParen) {
            let fields = self.parse_enum_tuple_variant()?;

            Ok(EnumVariant::Tuple {
                ident: variant_name,
                fields,
            })
        } else if self.current_token_is(TokenKind::LeftBrace) {
            let fields = self.parse_enum_struct_variant()?;

            Ok(EnumVariant::Struct {
                ident: variant_name,
                fields,
            })
        } else {
            Ok(EnumVariant::Unit(variant_name))
        }
    }

    fn parse_enum_struct_variant(&mut self) -> Result<Vec<EnumVariantStructField>, Diag> {
        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields = Vec::new();

        loop {
            if self.current_token_is(TokenKind::RightBrace) {
                return Err(self.error_with_hint(
                    &self.current_token(),
                    ParserDiagKind::InvalidToken(self.current_token().kind),
                    "Consider to add a field to enum struct variant or remove the braces.",
                ));
            }

            let loc = self.current_token().loc;
            let (line, column, start) = (loc.line, loc.column, loc.start);

            let name = self.parse_ident()?;
            self.next_token();

            self.expect_current(TokenKind::Colon)?;

            let ty = self.parse_type_specifier()?;
            self.next_token();

            let end = self.current_token().loc.end;

            fields.push(EnumVariantStructField {
                name,
                ty,
                loc: Loc::new(self.file_id(), line, column, start, end),
            });

            if self.current_token_is(TokenKind::RightBrace) {
                self.next_token();
                break;
            } else {
                self.expect_current(TokenKind::Comma)?;
                continue;
            }
        }

        Ok(fields)
    }

    fn parse_enum_tuple_variant(&mut self) -> Result<Vec<EnumVariantTupleField>, Diag> {
        self.expect_current(TokenKind::LeftParen)?;

        let mut fields = Vec::new();

        loop {
            if self.current_token_is(TokenKind::RightParen) {
                return Err(self.error_with_hint(
                    &self.current_token(),
                    ParserDiagKind::InvalidToken(self.current_token().kind),
                    "Consider to add a field to enum tuple variant or remove the parenthesis.",
                ));
            }

            let loc = self.current_token().loc;
            let (line, column, start) = (loc.line, loc.column, loc.start);

            let ty = self.parse_type_specifier()?;
            self.next_token();

            let end = self.current_token().loc.end;

            fields.push(EnumVariantTupleField {
                ty,
                loc: Loc::new(self.file_id(), line, column, start, end),
            });

            if self.current_token_is(TokenKind::RightParen) {
                self.next_token();
                break;
            } else {
                self.expect_current(TokenKind::Comma)?;
                continue;
            }
        }

        Ok(fields)
    }

    fn parse_union_field(&mut self) -> Result<UnionField, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let ident = self.parse_ident()?;
        self.next_token(); // consume ident

        self.expect_current(TokenKind::Colon)?;

        let type_token = self.parse_type_specifier()?;
        self.next_token();

        let end = self.current_token().loc.end;

        let field = UnionField {
            name: ident,
            ty: type_token,
            loc: Loc::new(self.file_id(), line, column, start, end),
        };

        self.expect_semicolon()?;

        Ok(field)
    }

    // FIXME: Make method_parsing helper methods.
    fn parse_union(&mut self, modifiers: UnionModifiers) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.expect_current(TokenKind::Union)?;

        let ident = self.parse_ident()?;
        self.next_token();

        let align = self.parse_align_specifier()?;

        let generic_params = {
            if self.current_token_is(TokenKind::LessThan) {
                self.parse_generic_params()?
            } else {
                GenericParams::new()
            }
        };

        let impls = self.parse_object_impls()?;

        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            let end = self.current_token().loc.end;

            return Ok(ASTStmt::Union(ASTUnionStmt {
                ident,
                methods: Vec::new(),
                fields: Vec::new(),
                generic_params,
                align,
                impls,
                modifiers,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }

        let mut fields: Vec<UnionField> = Vec::new();
        let mut methods: Vec<ASTFuncDefStmt> = Vec::new();

        loop {
            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }

            if self.current_token_is(TokenKind::EOF) {
                return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
            }

            if matches!(self.current_token().kind, TokenKind::Ident { .. }) {
                let field = self.parse_union_field()?;
                fields.push(field);
                continue;
            }

            if self.current_token_is(TokenKind::Function) {
                let func_modifiers = FuncModifiers::default();
                let method = self.parse_method(func_modifiers)?;
                methods.push(method);
                continue;
            }

            let modifiers = self.parse_unresolved_modifiers()?;
            let loc = self.current_token().loc;

            if self.current_token_is(TokenKind::Function) {
                let func_modifiers = modifiers.into_method_modifiers(loc)?;
                let method = self.parse_method(func_modifiers)?;
                methods.push(method);
                continue;
            }

            break;
        }

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Union(ASTUnionStmt {
            ident,
            methods,
            fields,
            generic_params,
            modifiers,
            align,
            impls,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_enum(&mut self, modifiers: EnumModifiers) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // parse enum keyword

        let ident = self.parse_ident()?;
        self.next_token(); // consume enum name

        let tag_type = self.parse_enum_tag_type()?;
        let align = self.parse_align_specifier()?;

        let generic_params = {
            if self.current_token_is(TokenKind::LessThan) {
                self.parse_generic_params()?
            } else {
                GenericParams::new()
            }
        };

        let impls = self.parse_object_impls()?;

        self.expect_current(TokenKind::LeftBrace)?;

        let mut variants: Vec<EnumVariant> = Vec::new();

        if self.current_token_is(TokenKind::RightBrace) {
            let end = self.current_token().loc.end;

            return Ok(ASTStmt::Enum(ASTEnumStmt {
                ident,
                variants,
                tag_type: tag_type,
                generic_params,
                methods: Vec::new(),
                align,
                modifiers,
                impls,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }

        variants.push(self.parse_enum_variant()?);

        while self.current_token_is(TokenKind::Comma) {
            self.expect_current(TokenKind::Comma)?;

            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }

            variants.push(self.parse_enum_variant()?);
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

        let mut methods: Vec<ASTFuncDefStmt> = Vec::new();

        loop {
            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }

            if self.current_token_is(TokenKind::EOF) {
                return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
            }

            if self.current_token_is(TokenKind::Function) {
                let func_modifiers = FuncModifiers::default();
                let method = self.parse_method(func_modifiers)?;
                methods.push(method);
                continue;
            }

            let loc = self.current_token().loc;
            let modifiers = self.parse_unresolved_modifiers()?;

            if self.current_token_is(TokenKind::Function) {
                let func_modifiers = modifiers.into_method_modifiers(loc)?;
                let method = self.parse_method(func_modifiers)?;
                methods.push(method);
                continue;
            }

            break;
        }

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Enum(ASTEnumStmt {
            ident,
            variants,
            tag_type: tag_type,
            generic_params,
            methods,
            modifiers,
            align,
            impls,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_struct(&mut self, modifiers: StructModifiers, is_packed: bool) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);
        let loc = self.current_token().loc;

        self.next_token(); // consume struct/bits token

        let struct_name = self.parse_ident()?;
        self.next_token(); // consume struct name

        let align = self.parse_align_specifier()?;

        let generic_params = {
            if self.current_token_is(TokenKind::LessThan) {
                self.parse_generic_params()?
            } else {
                GenericParams::new()
            }
        };

        let impls = self.parse_object_impls()?;

        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields: Vec<StructField> = Vec::new();
        let mut methods: Vec<ASTFuncDefStmt> = Vec::new();

        loop {
            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }

            if matches!(self.current_token().kind, TokenKind::Ident { .. }) {
                let field = self.parse_struct_field(None)?;
                fields.push(field);
                continue;
            }

            if self.current_token_is(TokenKind::EOF) {
                return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
            }

            let modifiers = self.parse_unresolved_modifiers()?;

            if matches!(self.current_token().kind, TokenKind::Ident { .. }) {
                let field_modifiers = modifiers.into_field_modifiers(loc)?;
                let field = self.parse_struct_field(Some(field_modifiers.vis))?;
                fields.push(field);
                continue;
            }

            let func_modifiers = modifiers.into_method_modifiers(loc)?;
            let method = self.parse_method(func_modifiers)?;
            methods.push(method);
        }

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Struct(ASTStructStmt {
            ident: struct_name,
            generic_params,
            impls,
            modifiers,
            fields,
            methods,
            align,
            is_packed,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_method(&mut self, modifiers: FuncModifiers) -> Result<ASTFuncDefStmt, Diag> {
        if let ASTStmt::FuncDef(func_def) = self.parse_func(modifiers)? {
            self.next_token(); // consume right brace

            Ok(func_def)
        } else {
            Err(self.error_at_current(ParserDiagKind::MethodMustHaveABody))
        }
    }

    fn parse_struct_field(&mut self, vis: Option<Visibility>) -> Result<StructField, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let ident = self.parse_ident()?;
        self.next_token(); // consume ident

        self.expect_current(TokenKind::Colon)?;

        let type_token = self.parse_type_specifier()?;
        self.next_token();

        let end = self.current_token().loc.end;

        let field = StructField {
            name: ident,
            ty: type_token,
            vis: vis.unwrap_or_default(),
            loc: Loc::new(self.file_id(), line, column, start, end),
        };

        self.expect_semicolon()?;
        Ok(field)
    }

    fn parse_object_impls(&mut self) -> Result<Vec<ImplementInterface>, Diag> {
        let mut impls = Vec::new();

        if self.current_token_is(TokenKind::Colon) {
            self.next_token();

            loop {
                let loc = self.current_token().loc;
                let (line, column, start) = (loc.line, loc.column, loc.start);

                let type_spec = self.parse_type_specifier()?;
                self.next_token();

                let end = self.current_token().loc.end;

                impls.push(ImplementInterface {
                    ty: type_spec,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                });

                if self.current_token_is(TokenKind::Comma) {
                    continue;
                } else {
                    break;
                }
            }
        }

        Ok(impls)
    }

    fn parse_break(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume break
        self.must_be_semicolon()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Break(ASTBreakStmt {
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_continue(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume continue
        self.must_be_semicolon()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Continue(ASTContinueStmt {
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_import_module_path(&mut self, module_path: ModulePath) -> Result<ModulePath, Diag> {
        if self.current_token_is(TokenKind::LeftBrace) {
            self.next_token();

            let mut singles: Vec<ModuleSegmentSingle> = Vec::new();

            while !self.current_token_is(TokenKind::RightBrace) {
                // enable aliasing features for a single
                let ident = self.parse_ident()?;
                self.next_token();

                let mut renamed: Option<Ident> = None;

                if self.current_token_is(TokenKind::As) {
                    self.next_token(); // consume as 

                    let renamed_identifier = self.parse_ident()?;
                    self.next_token();
                    renamed = Some(renamed_identifier);
                }

                singles.push(ModuleSegmentSingle { ident, renamed });

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

    fn parse_interface(&mut self, vis: Visibility) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token();

        let ident = self.parse_ident()?;
        self.next_token();

        let generic_params = {
            if self.current_token_is(TokenKind::LessThan) {
                self.parse_generic_params()?
            } else {
                GenericParams::new()
            }
        };

        self.expect_current(TokenKind::LeftBrace)?;

        let mut methods: Vec<ASTFuncDeclStmt> = Vec::new();

        if self.current_token_is(TokenKind::RightBrace) {
            let end = self.current_token().loc.end;

            return Ok(ASTStmt::Interface(ASTInterfaceStmt {
                ident,
                methods,
                generic_params,
                vis,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }

        loop {
            match self.current_token().kind {
                TokenKind::Function => {
                    let func_decl = match self.parse_func(FuncModifiers::default())? {
                        ASTStmt::FuncDecl(func_decl) => func_decl,
                        _ => {
                            return Err(self.error_invalid_token());
                        }
                    };

                    methods.push(func_decl);

                    if !self.peek_token_is(TokenKind::RightBrace) {
                        self.expect_semicolon()?;
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        self.expect_peek(TokenKind::RightBrace)?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Interface(ASTInterfaceStmt {
            ident,
            methods,
            generic_params,
            vis,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_import(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

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
                        return Err(self.error_invalid_token());
                    }
                }
            }

            self.expect_current(TokenKind::RightParen)?;
        } else {
            let mut module_path = self.parse_module_path()?;
            module_path = self.parse_import_module_path(module_path.clone())?;
            paths = vec![module_path];
        }

        self.must_be_semicolon()?;

        let end = self.current_token().loc.end;

        return Ok(ASTStmt::Import(ASTImportStmt {
            paths,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }));
    }

    fn parse_for_loop_body(&mut self) -> Result<Box<ASTBlockStmt>, Diag> {
        let body: Box<ASTBlockStmt>;
        if self.current_token_is(TokenKind::LeftBrace) {
            body = Box::new(self.parse_block()?);

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            }
        } else {
            return Err(self.error_at_current(ParserDiagKind::MissingOpeningBrace));
        }
        Ok(body)
    }

    fn parse_foreach(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume foreach
        self.expect_current(TokenKind::LeftParen)?;

        let item_identifier = self.parse_ident()?;
        self.next_token();

        let mut index_identifier: Option<Ident> = None;

        if self.current_token_is(TokenKind::Comma) {
            self.next_token(); // consume comma
            index_identifier = Some(self.parse_ident()?);
            self.next_token();
        }

        self.expect_current(TokenKind::In)?;

        let expr = self.parse_expr(Precedence::Lowest)?;
        self.next_token();

        self.expect_current(TokenKind::RightParen)?;

        let body = self.parse_block()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Foreach(ASTForeachStmt {
            item: item_identifier,
            index: index_identifier,
            expr,
            body: Box::new(body),
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_while_loop(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume while token
        self.expect_current(TokenKind::LeftParen)?;
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.next_token();
        self.expect_current(TokenKind::RightParen)?;

        let body = self.parse_block()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::While(ASTWhileStmt {
            condition,
            body: Box::new(body),
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_for_loop(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume for token

        // Check for non-conditional for loop
        if self.current_token_is(TokenKind::LeftBrace) {
            let body: Box<ASTBlockStmt>;
            if self.current_token_is(TokenKind::LeftBrace) {
                body = Box::new(self.parse_block()?);

                if self.peek_token_is(TokenKind::Semicolon) {
                    self.next_token();
                }
            } else {
                return Err(self.error_at_current(ParserDiagKind::MissingOpeningBrace));
            }

            let end = self.current_token().loc.end;

            return Ok(ASTStmt::For(ASTForStmt {
                initializer: None,
                condition: None,
                increment: None,
                body,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }

        self.expect_current(TokenKind::LeftParen)?;

        let mut initializer: Option<ASTVarStmt> = None;
        if !self.current_token_is(TokenKind::Semicolon) {
            if let ASTStmt::Variable(var) = self.parse_variable()? {
                initializer = Some(var);
            }
        }

        self.expect_semicolon()?;

        // for loop with only initializer expression
        if self.peek_token_is(TokenKind::LeftBrace) {
            self.expect_current(TokenKind::RightParen)?;

            let body = self.parse_for_loop_body()?;

            let end = self.current_token().loc.end;

            return Ok(ASTStmt::For(ASTForStmt {
                initializer,
                condition: None,
                increment: None,
                body,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }

        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek_semicolon()?;
        self.next_token();

        let mut increment: Option<ASTExpr> = None;
        if !self.current_token_is(TokenKind::RightParen) {
            increment = Some(self.parse_expr(Precedence::Lowest)?);
            self.next_token(); // consume increment token
        }

        self.expect_current(TokenKind::RightParen)?;
        let body = self.parse_for_loop_body()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::For(ASTForStmt {
            initializer,
            condition: Some(condition),
            increment,
            body,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_export_tuple_pattern(&mut self) -> Result<ExportPattern, Diag> {
        let loc = self.current_token().loc;
        let mutability = self.parse_mutability();

        let kind = match &self.current_token().kind {
            TokenKind::Underscore => ExportPatternKind::Ignore,
            TokenKind::Ident { .. } => {
                let ident = self.parse_ident()?;

                ExportPatternKind::Ident(ident)
            }
            TokenKind::LeftParen => {
                self.next_token();

                let mut patterns = Vec::new();

                if !matches!(self.current_token().kind, TokenKind::RightParen) {
                    loop {
                        let pattern = self.parse_export_tuple_pattern()?;
                        self.next_token();

                        patterns.push(pattern);

                        match self.current_token().kind {
                            TokenKind::Comma => {
                                self.next_token();
                            }
                            TokenKind::RightParen => {
                                break;
                            }
                            _ => {
                                return Err(self.error_with_hint(
                                    &self.current_token(),
                                    ParserDiagKind::InvalidToken(self.current_token().kind.clone()),
                                    "Expected ',' or ')' in tuple pattern.",
                                ));
                            }
                        }
                    }
                }

                self.must_be_right_paren()?;
                ExportPatternKind::Tuple(patterns)
            }
            _ => {
                return Err(self.error_with_hint(
                    &self.current_token(),
                    ParserDiagKind::InvalidToken(self.current_token().kind.clone()),
                    "Expected identifier, '_' or '(' for destructuring pattern.",
                ));
            }
        };

        // optional type annotation
        let ty = {
            if matches!(self.peek_token().kind, TokenKind::Colon) {
                self.next_token(); // consume last token
                self.next_token(); // consume colon

                let type_spec = self.parse_type_specifier()?;
                // self.next_token();

                Some(type_spec)
            } else {
                None
            }
        };

        Ok(ExportPattern {
            kind,
            ty,
            mutability,
            loc,
        })
    }

    fn parse_export_tuple(&mut self, is_const: bool) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        // parse the whole pattern (which must start with left paren)
        let pattern = self.parse_export_tuple_pattern()?;
        self.next_token();

        if !matches!(pattern.kind, ExportPatternKind::Tuple(_)) {
            return Err(self.error_at_current(ParserDiagKind::InvalidToken(self.current_token().kind.clone())));
        }

        // optional initializer
        let rhs = {
            if self.current_token_is(TokenKind::Assign) {
                self.next_token();
                Some(self.parse_expr(Precedence::Lowest)?)
            } else {
                None
            }
        };

        self.expect_peek_semicolon()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::ExportTuple(ASTExportTupleStmt {
            pattern,
            rhs,
            is_const,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_variable(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

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
            return self.parse_export_tuple(is_const);
        }

        let ident = self.parse_ident()?;
        self.next_token();

        if self.current_token_is(TokenKind::Semicolon) {
            return Err(self.error_at_current_with_hint(
                ParserDiagKind::IncompleteVariableDeclaration,
                "Expected type annotation or initializer after variable name.",
            ));
        }

        let mut variable_type: Option<TypeSpecifier> = None;
        if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume the colon

            variable_type = Some(self.parse_type_specifier()?);
            self.next_token();
        }

        if self.current_token_is(TokenKind::Semicolon) {
            let end = self.current_token().loc.end;

            return Ok(ASTStmt::Variable(ASTVarStmt {
                ident,
                ty: variable_type,
                rhs: None,
                is_const,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }
        self.expect_current(TokenKind::Assign)?;

        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek_semicolon()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Variable(ASTVarStmt {
            ident,
            rhs: Some(expr),
            ty: variable_type,
            is_const,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_func(&mut self, modifiers: FuncModifiers) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.expect_current(TokenKind::Function)?;

        let func_name = self.parse_ident()?; // export the name of the function
        self.next_token(); // consume the name of the ident

        let generic_params = {
            if self.current_token_is(TokenKind::LessThan) {
                self.parse_generic_params()?
            } else {
                GenericParams::new()
            }
        };

        let params = self.parse_func_params()?;

        let ret_type: Option<TypeSpecifier>;

        // parse return type
        if self.current_token_is(TokenKind::LeftBrace) {
            ret_type = None;
        } else if self.current_token_is(TokenKind::Semicolon) {
            let end = self.current_token().loc.end;

            return Ok(ASTStmt::FuncDecl(ASTFuncDeclStmt {
                ident: func_name,
                generic_params,
                params,
                ret_type: None,
                modifiers,
                renamed_as: None,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        } else if self.current_token_is(TokenKind::As) {
            self.next_token(); // consume as

            let renamed_as = self.parse_ident()?;
            self.next_token();

            let end = self.current_token().loc.end;

            return Ok(ASTStmt::FuncDecl(ASTFuncDeclStmt {
                ident: func_name,
                generic_params,
                params,
                ret_type: None,
                modifiers,
                renamed_as: Some(renamed_as),
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        } else {
            ret_type = Some(self.parse_type_specifier()?);
            self.next_token();
        }

        if self.current_token_is(TokenKind::Semicolon) {
            let end = self.current_token().loc.end;

            return Ok(ASTStmt::FuncDecl(ASTFuncDeclStmt {
                ident: func_name,
                generic_params,
                params,
                ret_type,
                modifiers,
                renamed_as: None,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        } else if self.current_token_is(TokenKind::As) {
            self.next_token();

            // parse renamed func decl
            let renamed_as = self.parse_ident()?;

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            } else if self.peek_token_is(TokenKind::LeftBrace) {
                return Err(self.error_with_hint(
                    &self.peek_token(),
                    ParserDiagKind::InvalidToken(self.peek_token().kind),
                    "Function declaration does not accept a body. Use a semicolon ';' instead of a body '{ ... }'.",
                ));
            }

            let end = self.current_token().loc.end;

            return Ok(ASTStmt::FuncDecl(ASTFuncDeclStmt {
                ident: func_name,
                generic_params,
                params,
                ret_type,
                modifiers,
                renamed_as: Some(renamed_as),
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }

        let body = Box::new(self.parse_block()?);

        let end = self.current_token().loc.end;

        return Ok(ASTStmt::FuncDef(ASTFuncDefStmt {
            ident: func_name,
            generic_params,
            params,
            body,
            ret_type,
            modifiers,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }));
    }

    fn parse_return(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume return token

        if self.current_token_is(TokenKind::Semicolon) {
            let end = self.current_token().loc.end;

            return Ok(ASTStmt::Return(ASTReturnStmt {
                argument: None,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }

        let argument = self.parse_expr(Precedence::Lowest)?;
        self.next_token();

        self.must_be_semicolon()?;

        let end = self.peek_token().loc.end;

        Ok(ASTStmt::Return(ASTReturnStmt {
            argument: Some(argument),
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_global_var(&mut self, modifiers: UnresolvedModifiers) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let is_const = {
            if self.current_token_is(TokenKind::Const) {
                self.next_token();
                true
            } else {
                self.expect_current(TokenKind::Var)?;
                false
            }
        };

        let global_var_modifiers = modifiers.into_global_var_modifiers(loc)?;

        let ident = self.parse_ident()?;
        self.next_token();

        let mut type_spec: Option<TypeSpecifier> = None;
        if self.current_token_is(TokenKind::Colon) {
            self.next_token();
            type_spec = Some(self.parse_type_specifier()?);
            self.next_token();
        }

        let expr = {
            if self.current_token_is(TokenKind::Assign) {
                self.next_token();
                let expr = Some(self.parse_expr(Precedence::Lowest)?);
                self.next_token();
                expr
            } else {
                None
            }
        };

        self.must_be_semicolon()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::GlobalVar(ASTGlobalVarStmt {
            ident,
            type_spec,
            expr,
            is_const,
            modifiers: global_var_modifiers,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_typedef(&mut self, vis: Visibility) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.expect_current(TokenKind::Typedef)?;

        let ident = self.parse_ident()?;
        self.next_token();

        let generic_params = {
            if self.current_token_is(TokenKind::LessThan) {
                self.parse_generic_params()?
            } else {
                GenericParams::new()
            }
        };

        self.expect_current(TokenKind::Assign)?;

        let type_spec = self.parse_type_specifier()?;
        self.next_token();

        self.must_be_semicolon()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Typedef(ASTTypedefStmt {
            vis,
            ident,
            type_spec,
            generic_params,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    pub fn parse_switch_pattern(&mut self) -> Result<SwitchCasePattern, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        if self.current_token_is(TokenKind::Underscore) {
            let loc = self.current_token().loc;
            self.next_token();
            return Ok(SwitchCasePattern {
                kind: SwitchCasePatternKind::Wildcard,
                loc,
            });
        }

        // enum variant pattern
        if self.current_token_is(TokenKind::Dot) {
            let loc = self.current_token().loc;
            let (line, column, start) = (loc.line, loc.column, loc.start);

            self.next_token(); // consume dot

            let variant = self.parse_ident()?;
            self.next_token();

            // .Variant(a, b, _)
            if self.current_token_is(TokenKind::LeftParen) {
                self.next_token();

                let mut items = Vec::new();

                if !self.current_token_is(TokenKind::RightParen) {
                    loop {
                        let pattern = self.parse_switch_pattern()?;
                        items.push(pattern);

                        if self.current_token_is(TokenKind::Comma) {
                            self.next_token();
                            continue;
                        }

                        break;
                    }
                }

                self.expect_current(TokenKind::RightParen)?;

                let end = self.current_token().loc.end;

                return Ok(SwitchCasePattern {
                    kind: SwitchCasePatternKind::EnumTupleVariant { variant, items },
                    loc: Loc::new(self.file_id(), line, column, start, end),
                });
            }

            // .Variant { a, b: x, c: _, .. }
            if self.current_token_is(TokenKind::LeftBrace) {
                self.next_token();

                let mut items = Vec::new();
                let mut has_rest = false;

                while !self.current_token_is(TokenKind::RightBrace) {
                    // rest pattern
                    if self.current_token_is(TokenKind::DoubleDot) {
                        has_rest = true;
                        self.next_token();
                        break;
                    }

                    let field_loc = self.current_token().loc;
                    let field_name = self.parse_ident()?;
                    self.next_token();

                    let pattern = if self.current_token_is(TokenKind::Colon) {
                        self.next_token();
                        self.parse_switch_pattern()?
                    } else {
                        // shorthand: { a } == { a: a }
                        SwitchCasePattern {
                            kind: SwitchCasePatternKind::Binding(field_name.clone()),
                            loc: field_loc,
                        }
                    };

                    items.push(SwitchCaseEnumStructPatternField {
                        name: field_name,
                        pattern,
                    });

                    if self.current_token_is(TokenKind::Comma) {
                        self.next_token();
                        continue;
                    }

                    break;
                }

                self.expect_current(TokenKind::RightBrace)?;

                let end = self.current_token().loc.end;

                return Ok(SwitchCasePattern {
                    kind: SwitchCasePatternKind::EnumStructVariant {
                        variant,
                        items,
                        has_rest,
                    },
                    loc: Loc::new(self.file_id(), line, column, start, end),
                });
            }

            let end = self.current_token().loc.end;

            return Ok(SwitchCasePattern {
                kind: SwitchCasePatternKind::EnumUnit(variant),
                loc: Loc::new(self.file_id(), line, column, start, end),
            });
        }

        if matches!(self.current_token().kind, TokenKind::Ident { .. }) {
            let ident = self.parse_ident()?;
            self.next_token();

            return Ok(SwitchCasePattern {
                kind: SwitchCasePatternKind::Binding(ident),
                loc,
            });
        }

        // expression or range pattern
        let expr = self.parse_expr(Precedence::Prefix)?;
        self.next_token();

        // range exclusive:  a..b
        if self.current_token_is(TokenKind::TripleDot) {
            self.next_token();
            let upper = self.parse_expr(Precedence::Prefix)?;
            self.next_token();

            let end = self.current_token().loc.end;

            return Ok(SwitchCasePattern {
                kind: SwitchCasePatternKind::Range(Range {
                    lower: expr,
                    upper,
                    inclusive_upper: false,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }),
                loc,
            });
        }

        // range inclusive:  a..=b
        if self.current_token_is(TokenKind::DoubleDot) && self.peek_token_is(TokenKind::Assign) {
            self.next_token(); // ..
            self.next_token(); // =

            let upper = self.parse_expr(Precedence::Prefix)?;
            self.next_token();

            let end = self.current_token().loc.end;

            return Ok(SwitchCasePattern {
                kind: SwitchCasePatternKind::Range(Range {
                    lower: expr,
                    upper,
                    inclusive_upper: true,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }),
                loc: Loc::new(self.file_id(), line, column, start, end),
            });
        }

        let end = self.current_token().loc.end;

        // fallback literal / expr pattern
        Ok(SwitchCasePattern {
            kind: SwitchCasePatternKind::Expr(expr),
            loc: Loc::new(self.file_id(), line, column, start, end),
        })
    }

    fn parse_switch(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token();
        self.expect_current(TokenKind::LeftParen)?;

        let operand = self.parse_expr(Precedence::Lowest)?;
        self.next_token();

        self.expect_current(TokenKind::RightParen)?;
        self.expect_current(TokenKind::LeftBrace)?;

        let mut cases: Vec<SwitchCase> = Vec::new();
        let mut default_case: Option<ASTBlockStmt> = None;

        loop {
            if self.current_token_is(TokenKind::Case) {
                let loc = self.current_token().loc;
                let (line, column, start) = (loc.line, loc.column, loc.start);

                self.next_token();

                let mut patterns: Vec<SwitchCasePattern> = Vec::new();
                loop {
                    let pattern = self.parse_switch_pattern()?;
                    patterns.push(pattern);

                    if self.current_token_is(TokenKind::Pipe) {
                        self.next_token();
                        continue;
                    } else {
                        break;
                    }
                }

                self.expect_current(TokenKind::FatArrow)?;

                let case_body = self.parse_block()?;
                self.next_token();

                let end = self.current_token().loc.end;

                cases.push(SwitchCase {
                    patterns,
                    body: case_body,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                });
            } else if self.current_token_is(TokenKind::Default) {
                self.next_token();
                self.expect_current(TokenKind::FatArrow)?;

                let case_body = self.parse_block()?;
                self.next_token();

                default_case = Some(case_body);
                break;
            } else {
                break;
            }
        }

        self.must_be_right_brace()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Switch(ASTSwitchStmt {
            operand,
            cases,
            default_case,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_if(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let mut branches: Vec<ASTIfStmt> = Vec::new();
        let mut alternate: Option<Box<ASTBlockStmt>> = None;

        self.expect_current(TokenKind::If)?;
        self.expect_current(TokenKind::LeftParen)?;

        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RightParen)?;
        self.next_token(); // consume right paren

        let consequent = Box::new(self.parse_block()?);

        if self.peek_token_is(TokenKind::Else) {
            self.next_token(); // consume right brace
        }

        while self.current_token_is(TokenKind::Else) {
            self.next_token(); // consume else token

            if self.current_token_is(TokenKind::If) {
                let start = self.current_token().loc.start;

                self.next_token(); // consume if token
                self.expect_current(TokenKind::LeftParen)?;

                let cond = self.parse_expr(Precedence::Lowest)?;
                self.next_token(); // consume last token of the expression

                self.expect_current(TokenKind::RightParen)?;

                let consequent = Box::new(self.parse_block()?);

                if self.peek_token_is(TokenKind::Else) {
                    self.next_token(); // consume else token
                }

                let end = self.current_token().loc.end;

                branches.push(ASTIfStmt {
                    condition: cond,
                    else_block: None,
                    then_block: consequent,
                    branches: Vec::new(),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                });
            } else {
                // parse else block
                alternate = Some(Box::new(self.parse_block()?));

                if !(self.current_token_is(TokenKind::RightBrace) || self.current_token_is(TokenKind::EOF)) {
                    return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
                }
            }
        }

        self.must_be_right_brace()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::If(ASTIfStmt {
            condition,
            then_block: consequent,
            branches,
            else_block: alternate,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_defer(&mut self) -> Result<ASTStmt, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token();
        let stmt = self.parse_stmt(None, false)?.first().unwrap().clone();

        self.must_be_semicolon()?;

        let end = self.current_token().loc.end;

        Ok(ASTStmt::Defer(ASTDeferStmt {
            operand: Box::new(stmt),
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }
}
