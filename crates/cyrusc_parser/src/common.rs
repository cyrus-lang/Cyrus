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

impl<'diag, 'source_file> Parser<'diag, 'source_file> {
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
                    return Ok(*value);
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

        let start = base_type.loc().start;
        let line = base_type.loc().line;

        loop {
            if self.peek_token_is(TokenKind::Asterisk) {
                self.next_token();
                base_type = TypeSpecifier::Deref(Box::new(base_type));
            } else if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token(); // consume '['
                base_type = self.parse_array_type(base_type)?;
            } else if self.peek_token_is(TokenKind::LessThan) {
                // handle generic type arguments

                self.next_token(); // consume less than
                let type_args = self.parse_type_arg_list()?;

                let end = self.current_token().loc.end;

                base_type = TypeSpecifier::GenericInst(GenericInst {
                    base: Box::new(base_type),
                    type_args,
                    loc: Loc::new(self.file_id(), line, start, end),
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

    /// Parses a list of trait bounds separated by `+`.
    fn parse_bounds(&mut self) -> Result<Vec<Bound>, Diag> {
        let mut list: Vec<Bound> = Vec::new();

        loop {
            let symbol = self.parse_ident()?;
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

    /// Parses a list of type arguments inside `<...>` for generic instantiations.
    ///
    /// Supports both positional (`<T, U>`) and named (`<Key = T, Value = U>`) arguments.
    pub(crate) fn parse_type_arg_list(&mut self) -> Result<Vec<TypeArg>, Diag> {
        self.expect_current(TokenKind::LessThan)?;

        let mut args = Vec::new();

        loop {
            if matches!(self.current_token().kind, TokenKind::Ident { .. }) && self.peek_token_is(TokenKind::Assign) {
                let key = self.parse_ident()?;
                self.next_token(); // consume ident
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
                    break;
                }
                _ => {
                    return Err(self.error_invalid_token());
                }
            }
        }

        Ok(args)
    }

    /// Determines if the current position starts a type argument list.
    ///
    /// This function resolves the ambiguity between `<` as a generic opener vs. a comparison operator.
    /// It's crucial for correctly parsing expressions like `x < y > z` (comparison) vs `T<U>` (generics).
    pub(crate) fn is_type_arg_start(&mut self, last_parsed_expr: Expr) -> TypeArgStartDetail {
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
                            // example 3: `Record<V=int[2]>[3]` - array of 3 Record<V=int[2]> instances
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
                                    includes_type_args: true, // Yes, there were type arguments
                                    is_array_init,            // True if this is array initialization
                                };
                            }
                            // `Type<T> { ... }` - Struct/tuple initialization with generics
                            // this pattern occurs when we have a generic type followed by a struct initializer.
                            // example 1: `Record<V = int[2]> { key: 10, value: 20 }` - Record struct with generic V
                            // example 2: `Point<f32> { x: 1.0, y: 2.0 }` - Point struct with f32 generic
                            // example 3: `Option<String> { value: "hello" }` - Option enum variant initialization
                            // the braces contain field initializers for the struct or enum variant.
                            else if next_token.kind == TokenKind::LeftBrace {
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
                            // 1. Type arguments: `Record<V=int[2]>` followed by nothing or an identifier
                            // 2. Comparison chain: `x < y > 5` where `5` disqualifies as type argument
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

    /// Determines if a token after `>` would invalidate type arguments.
    ///
    /// Used to distinguish generics from comparison expressions.
    /// Returns true if the token cannot follow a type argument list.
    fn token_disqualifies_type_arg(&mut self, kind: &TokenKind) -> bool {
        match kind {
            // allowed tokens
            TokenKind::Ident { .. } => false,

            TokenKind::UIntPtr
            | TokenKind::IntPtr
            | TokenKind::ISize
            | TokenKind::USize
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

            TokenKind::Struct | TokenKind::Union | TokenKind::Enum | TokenKind::Interface => true,

            TokenKind::True | TokenKind::False | TokenKind::Null => true,

            TokenKind::Macro | TokenKind::In | TokenKind::As => true,

            TokenKind::LeftBrace | TokenKind::RightBrace => true,

            TokenKind::Semicolon => true,

            TokenKind::Typedef | TokenKind::Typecast | TokenKind::SizeOf => true,

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
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        self.next_token(); // consume function

        let params = self.parse_func_type_params()?;
        let ret = self.parse_type_specifier()?;

        let end = self.current_token().loc.end;

        Ok(TypeSpecifier::FuncType(Box::new(FuncType {
            params,
            return_type: Box::new(ret),
            vis_opt: None,
            loc: Loc::new(self.file_id(), line, start, end),
        })))
    }

    fn parse_tuple(&mut self) -> Result<TypeSpecifier, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        self.expect_current(TokenKind::LeftParen)?;

        let mut type_list: Vec<TypeSpecifier> = Vec::new();

        loop {
            let ty = self.parse_type_specifier()?;
            type_list.push(ty);

            if self.current_token().kind == TokenKind::Comma {
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
            loc: Loc::new(self.file_id(), line, start, end),
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
            TokenKind::LeftParen => self.parse_tuple(),

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

            _ => Err(self.error_at_current(ParserDiagKind::InvalidTypeToken(token.kind))),
        }
    }

    fn parse_array_type(&mut self, base_type: TypeSpecifier) -> Result<TypeSpecifier, Diag> {
        let start = base_type.loc().start;
        let line = base_type.loc().line;

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
        // 1. `type_specifier = int`
        // 2. `type_specifier = Array(4, element=int)`
        // 3. `type_specifier = Array(3, element=Array(4, element=int))`
        let mut type_specifier = base_type.clone();

        for array_capacity in dims.iter().rev() {
            type_specifier = TypeSpecifier::Array(ArrayType {
                size: array_capacity.clone(),
                element_type: Box::new(type_specifier),
                loc: Loc::new(self.file_id(), line, start, end),
            });
        }

        Ok(type_specifier)
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
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

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
                let start = self.current_token().loc.start;
                let line = self.current_token().loc.line;

                let field_name = self.parse_ident()?;
                self.next_token(); // consume ident

                self.expect_current(TokenKind::Colon)?;

                let field_type_specifier = self.parse_type_specifier()?;
                self.next_token();

                let end = self.current_token().loc.end;

                fields.push(UnnamedStructTypeField {
                    field_name,
                    field_ty: field_type_specifier,
                    loc: Loc::new(self.file_id(), line, start, end),
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
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    fn parse_unnamed_union_type(&mut self, repr_attr: Option<ReprAttr>) -> Result<TypeSpecifier, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

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
                let start = self.current_token().loc.start;
                let line = self.current_token().loc.line;

                let field_name = self.parse_ident()?;
                self.next_token(); // consume ident

                self.expect_current(TokenKind::Colon)?;

                let field_type_specifier = self.parse_type_specifier()?;
                self.next_token();

                let end = self.current_token().loc.end;

                fields.push(UnnamedUnionTypeField {
                    field_name,
                    field_ty: field_type_specifier,
                    loc: Loc::new(self.file_id(), line, start, end),
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
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    fn parse_unnamed_enum_field(&mut self) -> Result<UnnamedEnumVariant, Diag> {
        let variant_name = self.parse_ident()?;
        self.next_token();

        let mut variant_fields: Vec<UnnamedEnumValuedField> = Vec::new();

        if self.current_token_is(TokenKind::Comma) || self.current_token_is(TokenKind::RightBrace) {
            return Ok(UnnamedEnumVariant::Ident(variant_name));
        } else if self.current_token_is(TokenKind::Assign) {
            self.next_token(); // consume assign

            let value = self.parse_expr(Precedence::Lowest)?;
            self.next_token(); // consume last token of the expression

            return Ok(UnnamedEnumVariant::Valued(variant_name, Box::new(value)));
        } else if self.current_token_is(TokenKind::LeftParen) {
            self.next_token(); // consume left paren

            loop {
                if self.current_token_is(TokenKind::RightParen) {
                    return Err(self.error_with_hint(
                        &self.current_token(),
                        ParserDiagKind::InvalidToken(self.current_token().kind),
                        "Consider to add a field to enum variant or remove the parenthesis.",
                    ));
                }

                let start = self.current_token().loc.start;
                let line = self.current_token().loc.line;

                let ty = self.parse_type_specifier()?;
                self.next_token();

                let end = self.current_token().loc.end;

                variant_fields.push(UnnamedEnumValuedField {
                    ty,
                    loc: Loc::new(self.file_id(), line, start, end),
                });

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
            Ok(UnnamedEnumVariant::Ident(variant_name))
        } else {
            Ok(UnnamedEnumVariant::Variant(variant_name, variant_fields))
        }
    }

    fn parse_unnamed_enum_type(&mut self, repr_attr: Option<ReprAttr>) -> Result<TypeSpecifier, Diag> {
        let line = self.current_token().loc.line;
        let start = self.current_token().loc.start;

        self.next_token(); // parse enum keyword

        let tag_type = self.parse_enum_tag_type()?.map(Box::new);
        let align = self.parse_align_specifier()?;

        self.expect_current(TokenKind::LeftBrace)?;

        let mut enum_fields: Vec<UnnamedEnumVariant> = Vec::new();

        if self.current_token_is(TokenKind::RightBrace) {
            let end = self.current_token().loc.end;

            return Ok(TypeSpecifier::UnnamedEnum(UnnamedEnumType {
                variants: enum_fields,
                repr_attr,
                tag_type: tag_type,
                align,
                loc: Loc::new(self.file_id(), line, start, end),
            }));
        }

        enum_fields.push(self.parse_unnamed_enum_field()?);

        while self.current_token_is(TokenKind::Comma) {
            self.expect_current(TokenKind::Comma)?;

            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }

            enum_fields.push(self.parse_unnamed_enum_field()?);
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
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    /// Checks if the parsed expression is a path or symbol.
    ///
    /// Path-like expressions include identifiers and module-qualified paths like `foo` or `std::foo`.
    fn current_expr_is_path_like(&self, last_parsed_expr: Expr) -> bool {
        matches!(last_parsed_expr, Expr::Ident(..) | Expr::ModuleImport(..))
    }
}
