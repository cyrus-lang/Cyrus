// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{Parser, diagnostics::ParserDiagKind};
use cyrusc_ast::{
    ASTStmt,
    abi::{Callconv, Inlining},
    attrs::{Attr, AttrKind},
    modifiers::FuncModifiers,
};
use cyrusc_diagcentral::Diag;
use cyrusc_source_loc::Loc;
use cyrusc_tokens::TokenKind;
use fx_hash::{FxHashSet, FxHashSetExt};

#[macro_export]
macro_rules! find_attr {
    ($attrs:expr, $pattern:pat) => {{
        $attrs.iter().find_map(|attr| {
            if matches!(attr.kind, $pattern) {
                Some((attr.to_string(), attr.loc))
            } else {
                None
            }
        })
    }};
    ($attrs:expr, $pattern:pat if $cond:expr) => {{
        $attrs.iter().find_map(|attr| {
            if matches!(attr.kind, $pattern) && $cond {
                Some((attr.to_string(), attr.loc))
            } else {
                None
            }
        })
    }};
    ($attrs:expr, |$attr:ident| $pred:expr) => {{
        $attrs.iter().find_map(|$attr| {
            if $pred {
                Some(($attr.to_string(), $attr.loc))
            } else {
                None
            }
        })
    }};
}
#[macro_export]
macro_rules! attr_legit {
    ($($attr_opt:expr),+ => $cond:expr, $invalid_fn:expr) => {{
        $(
            if let Some((attr, loc)) = $attr_opt
                && !$cond
            {
                $invalid_fn(attr, loc)?;
            }
        )+
    }};
}

impl<'source_file> Parser<'source_file> {
    pub(crate) fn parse_attr(&mut self) -> Result<Attr, Diag> {
        self.expect_current(TokenKind::LeftBracket)?;
        self.expect_current(TokenKind::LeftBracket)?;

        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let ident = self.parse_ident()?;
        self.next_token();

        let attr = match ident.value.as_str() {
            "inline" => {
                let inline_attr = self.parse_inline_attr()?;
                let end = self.current_token().loc.end;

                Attr {
                    kind: AttrKind::Inline(inline_attr),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }
            }
            "naked" => {
                let end = self.current_token().loc.end;

                Attr {
                    kind: AttrKind::Naked,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }
            }
            "callconv" => {
                let callconv = self.parse_callconv()?;
                let end = self.current_token().loc.end;

                Attr {
                    kind: AttrKind::Callconv(callconv),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }
            }
            _ => {
                return Err(self.error_at_loc(
                    ParserDiagKind::InvalidAttribute("Invalid attribute.".to_string()),
                    ident.loc,
                ));
            }
        };

        self.expect_current(TokenKind::RightBracket)?;
        self.must_be_left_bracket()?;
        Ok(attr)
    }

    fn parse_callconv(&mut self) -> Result<Callconv, Diag> {
        self.expect_current(TokenKind::LeftParen)?;

        let ident = self.parse_ident()?;
        self.next_token();

        let callconv = match ident.value.as_str() {
            "c" => Callconv::C,
            "naked" => Callconv::Naked,
            "interrupt" => Callconv::Interrupt,
            "fast" => Callconv::Fast,
            "cold" => Callconv::Cold,
            "aapcs" => Callconv::Aapcs,
            "stdcall" => Callconv::Stdcall,
            "fastcall" => Callconv::Fastcall,
            "thiscall" => Callconv::Thiscall,
            "vectorcall" => Callconv::Vectorcall,
            "sysv64" => Callconv::SysV64,
            "win64" => Callconv::Win64,
            "system" => Callconv::System,
            _ => {
                return Err(self.error_at_loc(
                    ParserDiagKind::InvalidModifier("Invalid callconv used in attribute.".to_string()),
                    ident.loc,
                ));
            }
        };

        self.expect_current(TokenKind::RightParen)?;
        Ok(callconv)
    }

    fn parse_inline_attr(&mut self) -> Result<Inlining, Diag> {
        if self.current_token_is(TokenKind::LeftParen) {
            self.next_token();

            let ident = self.parse_ident()?;
            self.next_token();

            let inline_attr = match ident.value.as_str() {
                "always" => Inlining::Always,
                "never" => Inlining::Never,
                _ => {
                    return Err(self.error_at_loc(
                        ParserDiagKind::InvalidAttribute(
                            "Invalid inline attribute. Expected 'always' or 'never'.".to_string(),
                        ),
                        ident.loc,
                    ));
                }
            };

            self.expect_current(TokenKind::RightParen)?;
            return Ok(inline_attr);
        }

        Ok(Inlining::Hint)
    }

    pub(crate) fn stmt_accepts_attrs(&self, stmt: &ASTStmt, attrs: &[Attr]) -> Result<(), Diag> {
        let is_func = matches!(stmt, ASTStmt::FuncDecl(_) | ASTStmt::FuncDef(_));
        let is_global_var = matches!(stmt, ASTStmt::GlobalVar(_));

        let is_inline = find_attr!(attrs, AttrKind::Inline(_));
        let is_naked = find_attr!(attrs, AttrKind::Naked);
        let is_callconv = find_attr!(attrs, AttrKind::Callconv(_));

        let invalid = |attr: String, loc: Loc| -> Result<(), Diag> {
            return Err(self.error_at_loc(ParserDiagKind::AttributeCannotBeAppliedTo(attr), loc));
        };

        // Allowed only in function
        attr_legit!(is_inline, is_naked, is_callconv => is_func, invalid);
        
        // Allowed only in global var
        // attr_legit!(is_inline, is_naked, is_callconv => is_func, invalid);

        Ok(())
    }

    pub(crate) fn check_attrs(&self, attrs: &[Attr]) -> Result<(), Diag> {
        let mut seen = FxHashSet::new();

        let dup = |attr: &Attr| -> Result<(), Diag> {
            return Err(self.error_at_loc(ParserDiagKind::DuplicateAttribute(attr.kind.to_string()), attr.loc));
        };

        for attr in attrs {
            if matches!(attr.kind, AttrKind::Inline(_)) && !seen.insert("inline") {
                dup(attr)?;
            }
            if matches!(attr.kind, AttrKind::Naked) && !seen.insert("naked") {
                dup(attr)?;
            }
            if matches!(attr.kind, AttrKind::Callconv(_)) && !seen.insert("callconv") {
                dup(attr)?;
            }
        }

        Ok(())
    }

    pub(crate) fn finalize_attrs(&self, stmt: &mut ASTStmt, attrs: &[Attr]) {
        for attr in attrs {
            match stmt {
                ASTStmt::FuncDef(func_def) => {
                    self.apply_attrs_on_func_modifiers(attr, &mut func_def.modifiers);
                }
                ASTStmt::FuncDecl(func_decl) => {
                    self.apply_attrs_on_func_modifiers(attr, &mut func_decl.modifiers);
                }
                ASTStmt::Struct(struct_stmt) => todo!(),
                ASTStmt::Enum(enum_stmt) => todo!(),
                ASTStmt::Union(union_stmt) => todo!(),
                ASTStmt::GlobalVar(global_var) => todo!(),

                _ => continue,
            }
        }
    }

    fn apply_attrs_on_func_modifiers(&self, attr: &Attr, modifiers: &mut FuncModifiers) {
        match &attr.kind {
            AttrKind::Inline(inline_attr) => {
                modifiers.inline = Some(*inline_attr);
            }
            AttrKind::Callconv(callconv) => {
                modifiers.callconv = Some(*callconv);
            }
            AttrKind::Naked => todo!(),
        }
    }
}
