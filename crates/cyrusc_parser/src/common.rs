// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::Diag;
use crate::Parser;
use crate::diagnostics::ParserDiagKind;
use crate::prec::Precedence;
use cyrusc_ast::abi::ReprAttr;
use cyrusc_ast::*;
use cyrusc_source_loc::Loc;
use cyrusc_tokens::PRIMITIVE_TYPES;
use cyrusc_tokens::TokenKind;
use cyrusc_tokens::literals::LiteralKind;

#[derive(Debug, Clone)]
pub(crate) struct TypeArgStartDetail {
    pub(crate) includes_type_args: bool,
    pub(crate) is_array_init: bool,
}

impl<'source_file> Parser<'source_file> {
    /// Parses an identifier token.
    pub(crate) fn parse_ident(&mut self) -> Result<Ident, Diag> {
        let token = self.current_token();

        if let TokenKind::Ident(ident) = token.kind {
            Ok(Ident {
                value: ident,
                loc: token.loc,
            })
        } else {
            Err(self.error_at_token(
                &token,
                ParserDiagKind::ExpectedIdentifier {
                    got: token.kind.to_string(),
                },
            ))
        }
    }

    /// Parses an integer literal that must not have a suffix.
    ///
    /// Used in contexts where type suffixes on integers are forbidden.
    pub(crate) fn parse_integer_without_suffix(&self) -> Result<i128, Diag> {
        let token = self.current_token();

        if let TokenKind::Literal(literal) = &token.kind {
            if let LiteralKind::Integer(value, suffix) = &literal.kind {
                if suffix.is_none() {
                    return Ok(value.as_int());
                }
            }
        }

        Err(self.error_at_current(ParserDiagKind::IntegerSuffixNotAllowed))
    }

    /// Parses a string literal that must not have a prefix.
    ///
    /// Used in contexts where string prefixes like `c"..."` or `b"..."` are forbidden.
    pub(crate) fn parse_string_without_prefix(&mut self) -> Result<String, Diag> {
        let token = self.current_token();

        if let TokenKind::Literal(literal) = &token.kind {
            if let LiteralKind::String(value, prefix) = &literal.kind {
                if prefix.is_none() {
                    return Ok(value.clone());
                }
            }
        }

        Err(self.error_at_token(&token, ParserDiagKind::StringPrefixNotAllowed))
    }

    /// Determines whether a token can begin a type specifier in the grammar.
    pub(crate) fn is_type_specifier_base_token(&mut self, token_kind: &TokenKind) -> bool {
        if PRIMITIVE_TYPES.contains(token_kind) {
            true
        } else if let TokenKind::Ident { .. } = token_kind {
            true
        } else {
            matches!(token_kind, TokenKind::Const)
        }
    }

    /// Parses a complete type specifier, handling pointers, arrays, and generic types.
    ///
    /// This is the heart of parsing types in our language.
    pub(crate) fn parse_type_specifier(&mut self) -> Result<TypeSpecifier, Diag> {
        let mut base_type = self.parse_base_type_token()?;

        let loc = base_type.loc();
        let (line, column, start) = (loc.line, loc.column, loc.start);

        loop {
            if self.peek_token_is(TokenKind::Asterisk) {
                self.next_token();
                base_type = TypeSpecifier::Deref(Box::new(base_type));
            } else if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token();
                base_type = self.parse_array_type(base_type)?;
            } else if self.peek_token_is(TokenKind::LessThan) {
                // handle generic type arguments

                self.next_token(); // consume less than
                let type_args = self.parse_type_args()?;

                let end = self.current_token().loc.end;

                base_type = TypeSpecifier::GenericInst(GenericInst {
                    base: Box::new(base_type),
                    type_args,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                });
            } else {
                break;
            }
        }

        Ok(base_type)
    }

    /// Parses generic parameters enclosed in `<...>`.
    ///
    /// Used in declarations like structs, enums, unions, typedefs, and functions.
    pub(crate) fn parse_generic_params(&mut self) -> Result<GenericParams, Diag> {
        self.expect_current(TokenKind::LessThan)?;

        let mut generic_params: GenericParams = GenericParams::new();

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

    /// Parses a single generic parameter with optional bounds and default type.
    ///
    /// Examples:
    /// - `T` (simple)
    /// - `T: Display` (with bound)
    /// - `T = int32` (with default type)
    /// - `T: Debug = int32` (with both bound and default)
    fn parse_generic_param(&mut self) -> Result<GenericParam, Diag> {
        let param_name = self.parse_ident()?;
        self.next_token();

        let bounds = if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume colon
            Some(self.parse_bounds()?)
        } else {
            None
        };

        let default = if self.current_token_is(TokenKind::Assign) {
            self.next_token(); // consume assign

            let ty = self.parse_type_specifier()?;
            self.next_token();

            Some(ty)
        } else {
            None
        };

        Ok(GenericParam {
            param_name,
            bounds,
            default,
        })
    }

    /// Parses a list of trait bounds separated by `+`.
    fn parse_bounds(&mut self) -> Result<Vec<Bound>, Diag> {
        let mut list: Vec<Bound> = Vec::new();

        loop {
            let loc = self.current_token().loc;
            let (line, column, start) = (loc.line, loc.column, loc.start);

            let ty = self.parse_type_specifier()?;
            self.next_token();

            let end = self.current_token().loc.end;

            list.push(Bound {
                type_spec: ty,
                loc: Loc::new(self.file_id(), line, column, start, end),
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

    /// Parses a list of type arguments inside `<...>` for generic instantiations.
    ///
    /// Supports both positional (`<T, U>`) and named (`<Key = T, Value = U>`) arguments.
    pub(crate) fn parse_type_args(&mut self) -> Result<TypeArgs, Diag> {
        self.expect_current(TokenKind::LessThan)?;

        let mut args = Vec::new();

        loop {
            if self.current_token_is(TokenKind::Underscore) {
                self.next_token();

                args.push(TypeArg::Infer);
            } else {
                let ty = self.parse_type_specifier()?;
                self.next_token();

                args.push(TypeArg::Type(ty));
            }

            match self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                    continue;
                }
                TokenKind::GreaterThan => {
                    break;
                }
                _ => {
                    return Err(self.error_at_current(ParserDiagKind::ExpectedToken(TokenKind::GreaterThan)));
                }
            }
        }

        Ok(TypeArgs(args))
    }

    /// Determines if the current position starts a type argument list.
    ///
    /// This function resolves the ambiguity between `<` as a generic opener vs. a comparison operator.
    /// It's crucial for correctly parsing expressions like `x < y > z` (comparison) vs `T<U>` (generics).
    pub(crate) fn is_type_arg_start(&mut self, last_parsed_expr: ASTExpr) -> TypeArgStartDetail {
        // do we even have a '<' token at the current position?
        // if there's no '<', this can't possibly be the start of type arguments
        if !self.peek_token_is(TokenKind::LessThan) {
            return TypeArgStartDetail {
                includes_type_args: false,
                is_array_init: false,
            };
        }

        // is the expression before the '<' path-like?
        // type arguments can only appear after identifiers or path expressions.
        // valid: `Record<T>`, `module::Symbol<int64>`
        // invalid: `5 < T >` (5 is not path-like), `(a + b) < T >` (expression not path-like)
        if !self.current_expr_is_path_like(last_parsed_expr) {
            return TypeArgStartDetail {
                includes_type_args: false,
                is_array_init: false,
            };
        }

        // now we need to determine if this is genuine type arguments
        // or something that just looks like it (e.g., comparison operators).
        let mut i = 1;
        let mut depth = 0;

        while let Some(token) = self.peek_n_token(i) {
            match token.kind {
                TokenKind::LessThan => {
                    // nested generic opening: `A<B<C>>`
                    // increment depth to track we're now inside another level
                    depth += 1;

                    // check that it's disqualified until the first greater-then token or not.
                    let mut j = 2;
                    while let Some(peek_token) = self.peek_n_token(j)
                        && peek_token.kind != TokenKind::GreaterThan
                    {
                        if self.token_disqualifies_type_arg(&peek_token.kind) {
                            return TypeArgStartDetail {
                                includes_type_args: false,
                                is_array_init: false,
                            };
                        }

                        j += 1;
                    }
                }
                TokenKind::GreaterThan => {
                    depth -= 1;

                    // we've found the closing '>'. Now we need to examine what
                    // comes after it to determine the exact syntactic construct.
                    if depth == 0 {
                        // distinguish array init and struct init when type args used
                        let after_greater = self.peek_n_token(i + 1);

                        if let Some(next_token) = after_greater {
                            // `GenericType<T>[N]` - array of a generic type
                            // this pattern occurs when we have a generic type followed by array initialization brackets.
                            // example 1: `Vec<int>[5]` - creates an array of 5 Vec<int> elements
                            // example 2: `Option<String>[10]` - array of 10 Option<String> elements
                            // the array size `N` can be any constant expression.
                            if next_token.kind == TokenKind::LeftBracket {
                                // we need to determine if this is genuinely an array initialization
                                // or something else. For example:
                                // - `Vec<int>[5]` is array init (creates array of size 5)
                                // - `Type<T>[U]` could be generic with array type parameter
                                // the `check_for_array_init_after_generic` method examines the tokens
                                // after the `[` to make this determination.
                                let is_array_init = self.check_for_array_init_after_generic(i + 1);

                                return TypeArgStartDetail {
                                    includes_type_args: true, // yes, there were type arguments
                                    is_array_init,            // true if this is array initialization
                                };
                            }
                            // `Type<T> { ... }` or `Type<T>.member(...)`
                            // both patterns are valid after a generic type expression:
                            // - `{ ... }` indicates a struct or enum variant initialization with generics
                            // - `.` indicates a field or method access on a generic type (e.g., `Object<int>.member(10)`)
                            // this branch ensures such cases are recognized as valid generic constructs, not disqualified comparisons.
                            else if next_token.kind == TokenKind::LeftBrace || next_token.kind == TokenKind::Dot {
                                return TypeArgStartDetail {
                                    includes_type_args: true, // yes, there were type arguments before the braces
                                    is_array_init: false, // not an array initialization - it's struct initialization
                                };
                            }
                            // `Type<T>(...)` - function call with generics
                            // this pattern occurs when we have a generic type followed by parentheses.
                            //
                            // example: `parse<i32>("123")` - function call with explicit type parameter
                            // example: `factory<String>()` - factory function returning generic type
                            else if next_token.kind == TokenKind::LeftParen {
                                return TypeArgStartDetail {
                                    includes_type_args: true, // Yes, there were type arguments before the parens
                                    is_array_init: false,     // Not an array initialization - it's either tuple
                                                              // struct init or function call with generics
                                };
                            }

                            // this is the key logic that distinguishes:
                            // 1. type args: `Record<int[2]>` followed by nothing or an identifier
                            // 2. comparison chain: `x < y > 5` where `5` disqualifies as type argument
                            //
                            // if the token after '>' would disqualify type arguments,
                            // then this was actually a comparison expression, not generics.
                            if self.token_disqualifies_type_arg(&next_token.kind) {
                                return TypeArgStartDetail {
                                    includes_type_args: false,
                                    is_array_init: false,
                                };
                            }
                        }

                        return TypeArgStartDetail {
                            includes_type_args: true,
                            is_array_init: false,
                        };
                    }
                }
                TokenKind::Semicolon | TokenKind::EOF => {
                    return TypeArgStartDetail {
                        includes_type_args: false,
                        is_array_init: false,
                    };
                }
                _ => {}
            }

            i += 1;
        }

        return TypeArgStartDetail {
            includes_type_args: true,
            is_array_init: false,
        };
    }

    /// Determines if a generic type followed by brackets is an array initialization.
    ///
    /// Checks whether `GenericType<T>[N]` is an array of generics (true) or
    /// a generic with an array type parameter (false). Looks for `{` after brackets
    /// to identify struct initialization.
    fn check_for_array_init_after_generic(&mut self, start_idx: usize) -> bool {
        let mut i = start_idx;
        let mut bracket_depth = 0;

        // first, skip through all array brackets
        while let Some(token) = self.peek_n_token(i) {
            match token.kind {
                TokenKind::LeftBracket => {
                    bracket_depth += 1;
                    i += 1;

                    // skip everything inside the brackets
                    while let Some(inner_token) = self.peek_n_token(i) {
                        if inner_token.kind == TokenKind::RightBracket {
                            bracket_depth -= 1;
                            i += 1;
                            break;
                        }
                        i += 1;
                    }
                }
                _ => {
                    // once we're past all array brackets (depth == 0)
                    // check if the next token is {
                    if bracket_depth == 0 {
                        return self
                            .peek_n_token(i)
                            .map(|t| t.kind == TokenKind::LeftBrace)
                            .unwrap_or(false);
                    }
                    i += 1;
                }
            }
        }

        false
    }

    /// Determines if a token that appears immediately after `>`
    /// invalidates the interpretation of `<...>` as type arguments.
    ///
    /// This is part of the `<` vs comparison disambiguation logic.
    /// If the token can legally follow a *type expression*, then it
    /// must NOT disqualify generic type arguments.
    ///
    /// Returns `true` when the token makes `<...>` impossible to be generics.
    fn token_disqualifies_type_arg(&mut self, kind: &TokenKind) -> bool {
        match kind {
            // ------------------------------------------------------------
            // Tokens that are VALID after a type expression
            // ------------------------------------------------------------

            // Path continuation: `Type<T>.member`
            TokenKind::Dot => false,

            // Function / tuple / constructor call: `Type<T>(...)`
            TokenKind::LeftParen => false,

            // Struct or enum variant initialization: `Type<T> { ... }`
            TokenKind::LeftBrace => false,

            // Array type or array init: `Type<T>[N]`
            TokenKind::LeftBracket | TokenKind::RightBracket => false,

            // Another generic layer or nested type
            TokenKind::LessThan | TokenKind::GreaterThan | TokenKind::Comma => false,

            TokenKind::Asterisk => false,

            TokenKind::Underscore => false,

            // ------------------------------------------------------------
            // Tokens that CANNOT follow a type expression
            // These indicate `<` was a comparison operator.
            // ------------------------------------------------------------
            TokenKind::Literal(_) => true,

            TokenKind::Assign => true,

            TokenKind::DoubleColon => true,

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

            TokenKind::ThinArrow
            | TokenKind::FatArrow
            | TokenKind::DoubleQuote
            | TokenKind::SingleQuote
            | TokenKind::DoubleDot
            | TokenKind::TripleDot => true,

            TokenKind::True | TokenKind::False | TokenKind::Null => true,

            TokenKind::RightParen => false,
            TokenKind::RightBrace => false,
            TokenKind::Semicolon => false,

            // ------------------------------------------------------------
            // Fallback
            // ------------------------------------------------------------
            other => !self.is_type_specifier_base_token(other),
        }
    }

    /// Parses function type parameters inside `(...)`.
    fn parse_func_type_params(&mut self) -> Result<FuncTypeParams, Diag> {
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
                _ => return Err(self.error_invalid_token()),
            }
        }

        self.expect_current(TokenKind::RightParen)?;

        Ok(FuncTypeParams { list, variadic })
    }

    fn parse_func_type(&mut self) -> Result<TypeSpecifier, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume function

        let params = self.parse_func_type_params()?;
        let ret = self.parse_type_specifier()?;

        let end = self.current_token().loc.end;

        Ok(TypeSpecifier::FuncType(Box::new(FuncType {
            params,
            ret_type: Box::new(ret),
            vis_opt: None,
            loc: Loc::new(self.file_id(), line, column, start, end),
        })))
    }

    pub(crate) fn parse_mutability(&mut self) -> Option<Mutability> {
        if self.current_token_is(TokenKind::Var) {
            self.next_token();
            Some(Mutability::Var)
        } else if self.current_token_is(TokenKind::Const) {
            self.next_token();
            Some(Mutability::Const)
        } else {
            None
        }
    }

    fn parse_tuple_type(&mut self) -> Result<TypeSpecifier, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.expect_current(TokenKind::LeftParen)?;

        let mut type_list: Vec<TypeSpecifier> = Vec::new();

        if self.current_token_is(TokenKind::RightParen) {
            let end = self.current_token().loc.end;

            return Ok(TypeSpecifier::Tuple(TupleType {
                type_list,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }

        loop {
            let ty = self.parse_type_specifier()?;
            type_list.push(ty);
            self.next_token();

            if self.current_token_is(TokenKind::Comma) {
                self.next_token(); // consume comma
                continue;
            }

            break;
        }

        self.must_be_right_paren()?;

        if type_list.len() <= 1 {
            return Err(self.error_at_current_with_hint(
                ParserDiagKind::SingleElementTupleType,
                "If you only need a single element, remove the tuple syntax and use the type directly.",
            ));
        }

        let end = self.current_token().loc.end;

        Ok(TypeSpecifier::Tuple(TupleType {
            type_list,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_base_type_token(&mut self) -> Result<TypeSpecifier, Diag> {
        let token = self.current_token().clone();

        match token.kind {
            // plain types
            ref token_kind if PRIMITIVE_TYPES.contains(&token_kind) => Ok(TypeSpecifier::TypeToken(token)),

            TokenKind::Const => {
                self.next_token(); // consume const
                let inner_type = self.parse_base_type_token()?;
                Ok(TypeSpecifier::Const(Box::new(inner_type)))
            }

            TokenKind::Repr => {
                let repr_attr = self.parse_repr_attr(token)?.unwrap();

                if self.current_token_is(TokenKind::Struct) {
                    self.parse_unnamed_struct_type(Some(repr_attr))
                } else if self.current_token_is(TokenKind::Union) {
                    self.parse_unnamed_union_type(Some(repr_attr))
                } else if self.current_token_is(TokenKind::Enum) {
                    self.parse_unnamed_enum_type(Some(repr_attr))
                } else {
                    todo!();
                }
            }

            TokenKind::Struct => self.parse_unnamed_struct_type(None),
            TokenKind::Union => self.parse_unnamed_union_type(None),
            TokenKind::Enum => self.parse_unnamed_enum_type(None),
            TokenKind::Function => self.parse_func_type(),
            TokenKind::LeftParen => self.parse_tuple_type(),

            TokenKind::Ident(ref ident) => {
                if self.peek_token_is(TokenKind::DoubleColon) {
                    let module_import = self.parse_module_import()?;
                    Ok(TypeSpecifier::ModuleImport(module_import))
                } else {
                    if ident == "Self" {
                        Ok(TypeSpecifier::SelfType(SelfType { loc: token.loc }))
                    } else {
                        Ok(TypeSpecifier::Ident(Ident {
                            value: {
                                if let TokenKind::Ident(ident) = token.kind {
                                    ident
                                } else {
                                    unreachable!()
                                }
                            },
                            loc: token.loc,
                        }))
                    }
                }
            }

            TokenKind::At => {
                let builtin = self.parse_builtin(false)?;

                Ok(TypeSpecifier::Builtin(builtin))
            }

            _ => Err(self.error_at_current(ParserDiagKind::ExpectedExprOrStmt)),
        }
    }

    fn parse_array_type(&mut self, base_type: TypeSpecifier) -> Result<TypeSpecifier, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let mut dims: Vec<ArrayCapacity> = Vec::new();

        // Parse consecutive array dimensions: `int[3][4]` -> dims = [3, 4]
        while self.current_token_is(TokenKind::LeftBracket) {
            let array_capacity = self.parse_array_capacity()?;

            // If another dimension follows `[3][4]`, consume the closing bracket
            // to position the parser at the next `[`. Without this, we'd be stuck at the `]`.
            if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token(); // consume `]`
            }

            dims.push(array_capacity);
        }

        let end = self.current_token().loc.end;

        // Build the nested array type from the inside out:
        // Start with base type `int`, then wrap with outer dimensions.
        //
        // Example: `int[3][4]` builds as:
        // 1. `type_spec = int`
        // 2. `type_spec = Array(4, element=int)`
        // 3. `type_spec = Array(3, element=Array(4, element=int))`
        let mut type_spec = base_type.clone();

        for array_capacity in dims.iter().rev() {
            type_spec = TypeSpecifier::Array(ArrayType {
                size: array_capacity.clone(),
                element_type: Box::new(type_spec),
                loc: Loc::new(self.file_id(), line, column, start, end),
            });
        }

        Ok(type_spec)
    }

    fn parse_array_capacity(&mut self) -> Result<ArrayCapacity, Diag> {
        self.expect_current(TokenKind::LeftBracket)?;

        if self.current_token_is(TokenKind::RightBracket) {
            return Ok(ArrayCapacity::Dynamic);
        }

        let capacity = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RightBracket)?;

        Ok(ArrayCapacity::Fixed(Box::new(capacity)))
    }

    fn parse_unnamed_struct_type(&mut self, repr_attr: Option<ReprAttr>) -> Result<TypeSpecifier, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume struct 

        let align = self.parse_align_specifier()?;

        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields: Vec<UnnamedStructTypeField> = Vec::new();

        loop {
            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }

            if self.current_token_is(TokenKind::EOF) {
                return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
            }

            if matches!(self.current_token().kind, TokenKind::Ident { .. }) {
                let loc = self.current_token().loc;
                let (line, column, start) = (loc.line, loc.column, loc.start);

                let field_name = self.parse_ident()?;
                self.next_token(); // consume ident

                self.expect_current(TokenKind::Colon)?;

                let field_type_specifier = self.parse_type_specifier()?;
                self.next_token();

                let end = self.current_token().loc.end;

                fields.push(UnnamedStructTypeField {
                    name: field_name,
                    ty: field_type_specifier,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                });

                if self.current_token_is(TokenKind::RightBrace) {
                    break;
                } else {
                    self.expect_current(TokenKind::Comma)?;
                }

                continue;
            }

            return Err(self.error_invalid_token());
        }

        let end = self.current_token().loc.end;

        Ok(TypeSpecifier::UnnamedStruct(UnnamedStructType {
            fields,
            repr_attr,
            align,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_unnamed_union_type(&mut self, repr_attr: Option<ReprAttr>) -> Result<TypeSpecifier, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume union 

        let align = self.parse_align_specifier()?;

        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields: Vec<UnnamedUnionTypeField> = Vec::new();

        loop {
            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }

            if self.current_token_is(TokenKind::EOF) {
                return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
            }

            if matches!(self.current_token().kind, TokenKind::Ident { .. }) {
                let loc = self.current_token().loc;
                let (line, column, start) = (loc.line, loc.column, loc.start);

                let field_name = self.parse_ident()?;
                self.next_token(); // consume ident

                self.expect_current(TokenKind::Colon)?;

                let field_type_specifier = self.parse_type_specifier()?;
                self.next_token();

                let end = self.current_token().loc.end;

                fields.push(UnnamedUnionTypeField {
                    ident: field_name,
                    field_type: field_type_specifier,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                });

                if self.current_token_is(TokenKind::RightBrace) {
                    break;
                } else {
                    self.expect_current(TokenKind::Comma)?;
                }

                continue;
            }

            return Err(self.error_invalid_token());
        }

        let end = self.current_token().loc.end;

        Ok(TypeSpecifier::UnnamedUnion(UnnamedUnionType {
            fields,
            repr_attr,
            align,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_unnamed_enum_type(&mut self, repr_attr: Option<ReprAttr>) -> Result<TypeSpecifier, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // parse enum keyword

        let tag_type = self.parse_enum_tag_type()?.map(Box::new);
        let align = self.parse_align_specifier()?;

        self.expect_current(TokenKind::LeftBrace)?;

        let mut enum_fields = Vec::new();

        if self.current_token_is(TokenKind::RightBrace) {
            let end = self.current_token().loc.end;

            return Ok(TypeSpecifier::UnnamedEnum(UnnamedEnumType {
                variants: enum_fields,
                repr_attr,
                tag_type: tag_type,
                align,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }

        enum_fields.push(self.parse_enum_variant()?);

        while self.current_token_is(TokenKind::Comma) {
            self.expect_current(TokenKind::Comma)?;

            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }

            enum_fields.push(self.parse_enum_variant()?);
            if self.peek_token_is(TokenKind::RightBrace) {
                break;
            }
        }

        // consume optional comma at the end of the variant
        if self.current_token_is(TokenKind::Comma) {
            self.next_token();
        }

        let end = self.current_token().loc.end;

        Ok(TypeSpecifier::UnnamedEnum(UnnamedEnumType {
            variants: enum_fields,
            tag_type: tag_type,
            repr_attr,
            align,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    /// Checks if the parsed expression is a path or symbol.
    ///
    /// Path-like expressions include identifiers and module-qualified paths like `foo` or `std::foo`.
    fn current_expr_is_path_like(&self, last_parsed_expr: ASTExpr) -> bool {
        matches!(last_parsed_expr, ASTExpr::Ident(..) | ASTExpr::ModuleImport(..))
    }
}
