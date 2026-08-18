// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{Parser, diagnostics::ParserDiagKind};
use cyrusc_ast::{
    ASTStmt,
    abi::{Callconv, Inlining, OptionalFlag, Prologue, ReprAttr, ReprAttrKind, ReprKind},
    attrs::{Attr, AttrKind, LinkAttr},
    modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers},
};
use cyrusc_diagcentral::{Diag, DiagLevel};
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
            if !$cond && let Some((attr, loc)) = &$attr_opt {
                $invalid_fn(attr.clone(), *loc)?;
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
            "thread_local" => {
                let end = self.current_token().loc.end;

                Attr {
                    kind: AttrKind::ThreadLocal,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }
            }
            "link" => {
                let link_attr = self.parse_link_attr()?;
                let end = self.current_token().loc.end;

                Attr {
                    kind: AttrKind::Link(link_attr),
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
            "nosanitize" => {
                let name = self.parse_attr_strval()?;
                let end = self.current_token().loc.end;

                Attr {
                    kind: AttrKind::NoSanitize(name),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }
            }
            "section" => {
                let name = self.parse_attr_strval()?;
                let end = self.current_token().loc.end;

                Attr {
                    kind: AttrKind::Section(name),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }
            }
            "repr" => {
                let repr = self.parse_repr()?;
                let end = self.current_token().loc.end;

                Attr {
                    kind: AttrKind::Repr(repr),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }
            }
            "cold" => {
                let end = self.current_token().loc.end;

                Attr {
                    kind: AttrKind::Cold,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }
            }
            "hot" => {
                let end = self.current_token().loc.end;

                Attr {
                    kind: AttrKind::Hot,
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

    fn parse_link_attr(&mut self) -> Result<LinkAttr, Diag> {
        self.expect_current(TokenKind::LeftParen)?;

        let ident = self.parse_ident()?;
        self.next_token();

        let link_attr = match ident.value.as_str() {
            "once" => LinkAttr::LinkOnce,
            "weak" => LinkAttr::Weak,
            _ => {
                return Err(self.error_at_loc(
                    ParserDiagKind::InvalidModifier("Invalid link kind in attribute.".to_string()),
                    ident.loc,
                ));
            }
        };

        self.expect_current(TokenKind::RightParen)?;
        Ok(link_attr)
    }

    fn parse_repr(&mut self) -> Result<ReprAttr, Diag> {
        self.expect_current(TokenKind::LeftParen)?;

        let mut repr = ReprAttr::new();

        let mut try_parse = || -> Result<(), Diag> {
            if self.current_token_is(TokenKind::RightParen) {
                return Ok(());
            }

            let ident = self.parse_ident()?;
            self.next_token();

            if ident.value == "packed" {
                if let Err(err) = repr.add(ReprAttrKind::Packed, ident.loc) {
                    return Err(self.error_at_loc(ParserDiagKind::InvalidModifier(err), ident.loc));
                }
            } else {
                let kind = match ident.value.as_str() {
                    "c" => ReprKind::C,
                    "cyrus" => ReprKind::Cyrus,
                    "transparent" => ReprKind::Transparent,
                    _ => {
                        return Err(self.error_at_loc(
                            ParserDiagKind::InvalidModifier("Invalid repr kind in attribute.".to_string()),
                            ident.loc,
                        ));
                    }
                };

                if let Err(err) = repr.add(ReprAttrKind::Kind(kind), ident.loc) {
                    return Err(self.error_at_loc(ParserDiagKind::InvalidModifier(err), ident.loc));
                }
            }

            if !self.current_token_is(TokenKind::RightParen) {
                self.expect_current(TokenKind::Comma)?;
            }

            Ok(())
        };

        try_parse()?;
        try_parse()?;

        self.expect_current(TokenKind::RightParen)?;
        Ok(repr)
    }

    fn parse_attr_strval(&mut self) -> Result<String, Diag> {
        self.expect_current(TokenKind::LeftParen)?;

        let name = self.parse_string_without_prefix()?;
        self.next_token();

        self.expect_current(TokenKind::RightParen)?;
        Ok(name)
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
        let is_struct = matches!(stmt, ASTStmt::Struct(_));
        let is_union = matches!(stmt, ASTStmt::Union(_));
        let is_enum = matches!(stmt, ASTStmt::Enum(_));

        let is_opt = find_attr!(attrs, AttrKind::OptNone | AttrKind::OptSize);
        let is_thread_local = find_attr!(attrs, AttrKind::ThreadLocal);
        let is_nosanitize = find_attr!(attrs, AttrKind::NoSanitize(_));
        let is_callconv = find_attr!(attrs, AttrKind::Callconv(_));
        let is_section = find_attr!(attrs, AttrKind::Section(_));
        let is_inline = find_attr!(attrs, AttrKind::Inline(_));
        let is_repr = find_attr!(attrs, AttrKind::Repr(_));
        let is_link = find_attr!(attrs, AttrKind::Link(_));
        let is_naked = find_attr!(attrs, AttrKind::Naked);
        let is_cold = find_attr!(attrs, AttrKind::Cold);
        let is_hot = find_attr!(attrs, AttrKind::Hot);

        let invalid = |attr: String, loc: Loc| -> Result<(), Diag> {
            return Err(self.error_at_loc(ParserDiagKind::AttributeCannotBeAppliedTo(attr), loc));
        };

        attr_legit!(
            is_opt,
            is_callconv,
            is_inline,
            is_naked,
            is_nosanitize,
            is_section,
            is_cold,
            is_hot,
            is_link
            => is_func, invalid
        );

        attr_legit!(is_section, is_thread_local => is_global_var, invalid);

        attr_legit!(is_repr => is_struct || is_union || is_enum, invalid);

        for attr in attrs {
            if let AttrKind::Repr(repr) = &attr.kind {
                if is_struct {
                    self.validate_struct_repr(repr)?;
                }
                if is_enum {
                    self.validate_enum_repr(repr, attr.loc)?;
                }
                if is_union {
                    self.validate_union_repr(repr, attr.loc)?;
                }
            }
        }

        Ok(())
    }

    pub(crate) fn check_attrs(&self, attrs: &[Attr]) -> Result<(), Diag> {
        let mut seen = FxHashSet::new();

        let dup = |attr: &Attr| -> Result<(), Diag> {
            return Err(self.error_at_loc(ParserDiagKind::DuplicateAttribute(attr.kind.to_string()), attr.loc));
        };

        for attr in attrs {
            match attr.kind {
                AttrKind::OptNone | AttrKind::OptSize => {
                    if !seen.insert("optimize") {
                        dup(attr)?;
                    }
                }
                AttrKind::NoSanitize(_) => {
                    if !seen.insert("nosanitize") {
                        dup(attr)?;
                    }
                }
                AttrKind::Section(_) => {
                    if !seen.insert("section") {
                        dup(attr)?;
                    }
                }
                AttrKind::Callconv(_) => {
                    if !seen.insert("callconv") {
                        dup(attr)?;
                    }
                }
                AttrKind::Inline(_) => {
                    if !seen.insert("inline") {
                        dup(attr)?;
                    }
                }
                AttrKind::Naked => {
                    if !seen.insert("naked") {
                        dup(attr)?;
                    }
                }
                AttrKind::Cold => {
                    if !seen.insert("cold") {
                        dup(attr)?;
                    }
                }
                AttrKind::Hot => {
                    if !seen.insert("hot") {
                        dup(attr)?;
                    }
                }
                AttrKind::Repr(_) => {
                    if !seen.insert("repr") {
                        dup(attr)?;
                    }
                }
                AttrKind::Link(_) => {
                    if !seen.insert("link") {
                        dup(attr)?;
                    }
                }
                AttrKind::ThreadLocal => {
                    if !seen.insert("thread_local") {
                        dup(attr)?;
                    }
                }
            }
        }

        // TODO Check hot/cold not be used together

        Ok(())
    }

    // Finalize & Validate

    fn validate_struct_repr(&self, repr: &ReprAttr) -> Result<(), Diag> {
        if let Some((kind, loc)) = repr.kind()
            && repr.is_packed()
            && matches!(kind, ReprKind::Transparent)
        {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "repr(transparent) cannot be combined with repr(packed).".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: Some("Remove either 'packed' or 'transparent'.".to_string()),
            });
        }
        Ok(())
    }

    fn validate_union_repr(&self, repr: &ReprAttr, loc: Loc) -> Result<(), Diag> {
        if repr.is_packed() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Packed layout is not supported for unions.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: Some(
                    "Packed unions can cause unaligned field accesses. Use repr(C) with manual padding if needed."
                        .to_string(),
                ),
            });
        }

        if let Some((kind, loc)) = repr.kind() {
            match kind {
                ReprKind::C | ReprKind::Cyrus => {}
                ReprKind::Transparent => {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::InvalidModifier(
                            "'repr(transparent)' is not valid for unions.".to_string(),
                        )),
                        level: DiagLevel::Error,
                        loc: Some(loc),
                        hint: None,
                    });
                }
            }
        }

        Ok(())
    }

    fn validate_enum_repr(&self, repr: &ReprAttr, loc: Loc) -> Result<(), Diag> {
        // Packed enums not allowed
        if repr.is_packed() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "repr(packed) is not supported for enums.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: Some("Packed enums are not safe. Use repr(C) with a fixed-size tag field instead.".to_string()),
            });
        }

        // Only `repr(c)`, `repr(cyrus)` allowed
        if let Some((kind, loc)) = repr.kind() {
            match kind {
                ReprKind::C | ReprKind::Cyrus => { /* valid */ }
                ReprKind::Transparent => {
                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::InvalidModifier(
                            "'repr(transparent)' is not valid for enums.".to_string(),
                        )),
                        level: DiagLevel::Error,
                        loc: Some(loc),
                        hint: None,
                    });
                }
            }
        }

        Ok(())
    }

    pub(crate) fn finalize_attrs(&self, stmt: &mut ASTStmt, attrs: &[Attr]) {
        for attr in attrs {
            match stmt {
                ASTStmt::GlobalVar(global_var) => {
                    self.apply_attrs_on_global_var_modifiers(attr, &mut global_var.modifiers);
                }
                ASTStmt::FuncDef(func_def) => {
                    self.apply_attrs_on_func_modifiers(attr, &mut func_def.modifiers);
                }
                ASTStmt::FuncDecl(func_decl) => {
                    self.apply_attrs_on_func_modifiers(attr, &mut func_decl.modifiers);
                }
                ASTStmt::Struct(struct_stmt) => {
                    self.apply_attrs_on_struct_modifiers(attr, &mut struct_stmt.modifiers);
                }
                ASTStmt::Enum(enum_stmt) => {
                    self.apply_attrs_on_enum_modifiers(attr, &mut enum_stmt.modifiers);
                }
                ASTStmt::Union(union_stmt) => {
                    self.apply_attrs_on_union_modifiers(attr, &mut union_stmt.modifiers);
                }
                _ => continue,
            }
        }
    }

    // Apply

    fn apply_attrs_on_struct_modifiers(&self, attr: &Attr, modifiers: &mut StructModifiers) {
        match &attr.kind {
            AttrKind::Repr(repr_attr) => {
                modifiers.repr_attr = Some(repr_attr.clone());
            }
            _ => unreachable!(),
        }
    }

    fn apply_attrs_on_enum_modifiers(&self, attr: &Attr, modifiers: &mut EnumModifiers) {
        match &attr.kind {
            AttrKind::Repr(repr_attr) => {
                modifiers.repr_attr = Some(repr_attr.clone());
            }
            _ => unreachable!(),
        }
    }

    fn apply_attrs_on_union_modifiers(&self, attr: &Attr, modifiers: &mut UnionModifiers) {
        match &attr.kind {
            AttrKind::Repr(repr_attr) => {
                modifiers.repr_attr = Some(repr_attr.clone());
            }
            _ => unreachable!(),
        }
    }

    fn apply_attrs_on_global_var_modifiers(&self, attr: &Attr, modifiers: &mut GlobalVarModifiers) {
        match &attr.kind {
            AttrKind::Section(name) => {
                modifiers.section = Some(name.clone());
            }
            // AttrKind::Weak
            AttrKind::Link(link_attr) => {
                if *link_attr == LinkAttr::Weak {
                    modifiers.weak = true;
                }
                if *link_attr == LinkAttr::LinkOnce {
                    modifiers.link_once = true;
                }
            }
            // AttrKind::Weak
            AttrKind::ThreadLocal => {
                modifiers.thread_local = true;
            }
            _ => unreachable!(),
        }
    }

    fn apply_attrs_on_func_modifiers(&self, attr: &Attr, modifiers: &mut FuncModifiers) {
        match &attr.kind {
            AttrKind::NoSanitize(name) => {
                modifiers
                    .optional_flags
                    .push(OptionalFlag::NoSanitize(name.to_string()));
            }
            AttrKind::Inline(inline_attr) => {
                modifiers.inline = Some(*inline_attr);
            }
            AttrKind::Callconv(callconv) => {
                modifiers.callconv = Some(*callconv);
            }
            AttrKind::Naked => {
                modifiers.prologue = Some(Prologue::Naked);
            }
            AttrKind::Section(name) => {
                modifiers.section = Some(name.to_string());
            }
            AttrKind::Cold => {
                modifiers.optional_flags.push(OptionalFlag::Cold);
            }
            AttrKind::Hot => {
                modifiers.optional_flags.push(OptionalFlag::Hot);
            }
            AttrKind::OptNone => {
                modifiers.optional_flags.push(OptionalFlag::OptNone);
            }
            AttrKind::OptSize => {
                modifiers.optional_flags.push(OptionalFlag::OptSize);
            }
            AttrKind::Link(link_attr) => {
                if *link_attr == LinkAttr::Weak {
                    modifiers.weak = true;
                }
                if *link_attr == LinkAttr::LinkOnce {
                    modifiers.link_once = true;
                }
            }
            _ => unreachable!(),
        }
    }
}
