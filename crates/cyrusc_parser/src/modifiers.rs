// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{Parser, diagnostics::ParserDiagKind};
use cyrusc_ast::TypeSpecifier;
use cyrusc_ast::abi::{
    CallConv, ExportKind, Inlining, Linkage, OptionalFlag, Prologue, ReprAttr, ReprAttrKind, ReprKind, SectionAttr,
    Visibility, validate_flags,
};
use cyrusc_ast::modifiers::{EnumModifiers, FuncModifiers, GlobalVarModifiers, StructModifiers, UnionModifiers};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_tokens::{Token, TokenKind};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct UnresolvedModifiers {
    pub visibility: Option<Visibility>,
    pub linkage: Option<Linkage>,
    pub inline: Option<Inlining>,
    pub prologue: Option<Prologue>,
    pub export: Option<ExportKind>,
    pub callconv: Option<CallConv>,
    pub optional_flags: Vec<OptionalFlag>,
    pub placement: Vec<SectionAttr>,
    pub repr_attr: Option<ReprAttr>,
}

#[derive(Debug, Clone)]
pub(crate) struct InterfaceModifiers {
    pub(crate) vis: Visibility,
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleDeclModifiers {
    pub(crate) vis: Visibility,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedefModifiers {
    pub(crate) vis: Visibility,
}

#[derive(Debug, Clone)]
pub(crate) struct FieldModifiers {
    pub vis: Visibility,
}

impl<'source_file> Parser<'source_file> {
    pub(crate) fn parse_unresolved_modifiers(&mut self) -> Result<UnresolvedModifiers, Diag> {
        let mut mods = UnresolvedModifiers {
            visibility: None,
            linkage: None,
            inline: None,
            prologue: None,
            export: None,
            callconv: None,
            repr_attr: None,
            optional_flags: Vec::new(),
            placement: Vec::new(),
        };

        loop {
            let token = self.current_token().clone();
            let mut consumed = false;

            macro_rules! try_set_once {
                ($field:ident, $parser_method:ident, $err_msg:expr) => {
                    if let Some(value) = self.$parser_method(token.clone()) {
                        if mods.$field.is_some() {
                            return Err(self.error_at_token(
                                &token,
                                ParserDiagKind::InvalidModifier($err_msg.to_string()),
                            ));
                        }
                        mods.$field = Some(value);
                        consumed = true;
                    }
                };
            }

            macro_rules! try_set_once_result {
                ($field:ident, $parser_method:ident, $err_msg:expr) => {
                    match self.$parser_method(token.clone())? {
                        Some(value) => {
                            if mods.$field.is_some() {
                                return Err(self.error_at_token(
                                    &token,
                                    ParserDiagKind::InvalidModifier($err_msg.to_string()),
                                ));
                            }
                            mods.$field = Some(value);
                            consumed = true;
                        }
                        None => {}
                    }
                };
            }

            try_set_once!(visibility, parse_vis, "Visibility modifier already specified.");
            try_set_once!(inline, parse_inlining, "Inlining modifier already specified.");
            try_set_once!(prologue, parse_prologue, "Prologue modifier already specified.");
            try_set_once!(export, parse_export_kind, "Export kind already specified.");
            try_set_once_result!(linkage, parse_linkage, "Linkage modifier already specified.");
            try_set_once_result!(callconv, parse_callconv, "Call convention already specified.");
            try_set_once_result!(repr_attr, parse_repr_attr, "Repr already specified.");

            match self.parse_optional_flag(token.clone()) {
                Ok(optional_flag) => {
                    if let Some(flag) = optional_flag {
                        mods.optional_flags.push(flag);
                        consumed = true;
                    }
                }
                Err(diag) => {
                    return Err(self.error_at_token(&token, ParserDiagKind::InvalidModifier(diag.kind.to_string())));
                }
            }

            if let Err(err) = validate_flags(&mods.optional_flags) {
                return Err(self.error_at_token(&token, ParserDiagKind::InvalidModifier(err)));
            }

            if let Ok(Some(section)) = self.parse_placement(token.clone()) {
                mods.placement.push(SectionAttr(section));
                consumed = true;
            }

            if !consumed {
                break;
            }
        }

        Ok(mods)
    }

    pub(crate) fn parse_enum_tag_type(&mut self) -> Result<Option<TypeSpecifier>, Diag> {
        if !self.current_token_is(TokenKind::LeftParen) {
            return Ok(None);
        }

        self.next_token(); // consume left paren
        let tag_type = self.parse_type_specifier()?;
        self.next_token();
        self.expect_current(TokenKind::RightParen)?;

        Ok(Some(tag_type))
    }

    pub(crate) fn parse_align_specifier(&mut self) -> Result<Option<usize>, Diag> {
        if !self.current_token().kind.is_ident_str("align") {
            return Ok(None);
        }

        self.next_token(); // consume ident align
        self.expect_current(TokenKind::LeftParen)?;
        let align = self.parse_integer_without_suffix()?;
        self.next_token();
        self.expect_current(TokenKind::RightParen)?;

        Ok(Some(align.try_into().unwrap()))
    }

    pub(crate) fn parse_repr_attr(&mut self, token: Token) -> Result<Option<ReprAttr>, Diag> {
        if token.kind != TokenKind::Repr {
            return Ok(None);
        }

        self.next_token(); // consume repr
        self.expect_current(TokenKind::LeftParen)?;

        let mut repr_attr = ReprAttr::new();

        loop {
            let repr_str = self.parse_ident()?;
            self.next_token();

            if repr_str.value == "packed" {
                if let Err(err) = repr_attr.add(ReprAttrKind::Packed) {
                    return Err(self.error_at_token(&token, ParserDiagKind::InvalidModifier(err.to_string())));
                }
            } else {
                match ReprAttr::try_kind_from_str(&repr_str.value) {
                    Ok(repr_kind) => {
                        if let Err(err) = repr_attr.add(ReprAttrKind::Kind(repr_kind)) {
                            return Err(self.error_at_token(&token, ParserDiagKind::InvalidModifier(err.to_string())));
                        }
                    }
                    Err(err) => {
                        return Err(self.error_at_token(&token, ParserDiagKind::InvalidModifier(err.to_string())));
                    }
                }
            }

            if !self.current_token_is(TokenKind::Comma) {
                break;
            } else {
                self.next_token();
                continue;
            }
        }

        self.next_token();
        Ok(Some(repr_attr))
    }

    pub(crate) fn parse_placement(&mut self, token: Token) -> Result<Option<String>, Diag> {
        if matches!(token.kind, TokenKind::Section) {
            self.next_token();
            self.expect_current(TokenKind::LeftParen)?;
            let section_name = self.parse_string_without_prefix()?;
            self.next_token();
            self.expect_current(TokenKind::RightParen)?;
            Ok(Some(section_name))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn parse_optional_flag(&mut self, token: Token) -> Result<Option<OptionalFlag>, Diag> {
        match token.kind {
            TokenKind::NoReturn => {
                self.next_token();
                Ok(Some(OptionalFlag::NoReturn))
            }
            TokenKind::NoUnwind => {
                self.next_token();
                Ok(Some(OptionalFlag::NoUnwind))
            }
            TokenKind::Cold => {
                self.next_token();
                Ok(Some(OptionalFlag::Cold))
            }
            TokenKind::Hot => {
                self.next_token();
                Ok(Some(OptionalFlag::Hot))
            }
            TokenKind::OptSize => {
                self.next_token();
                Ok(Some(OptionalFlag::OptSize))
            }
            TokenKind::OptNone => {
                self.next_token();
                Ok(Some(OptionalFlag::OptNone))
            }
            TokenKind::NoSanitize => {
                self.next_token();
                self.expect_current(TokenKind::LeftParen)?;
                let arg = self.parse_string_without_prefix()?;
                self.next_token();
                self.expect_current(TokenKind::RightParen)?;
                Ok(Some(OptionalFlag::NoSanitize(arg)))
            }
            _ => Ok(None),
        }
    }

    pub(crate) fn parse_callconv(&mut self, token: Token) -> Result<Option<CallConv>, Diag> {
        if matches!(token.kind, TokenKind::Callconv) {
            self.expect_current(TokenKind::Callconv)?;
            self.expect_current(TokenKind::LeftParen)?;

            let callconv_str = self.parse_ident()?;
            self.next_token();

            self.expect_current(TokenKind::RightParen)?;

            match CallConv::try_from(callconv_str.as_string()) {
                Ok(callconv) => Ok(Some(callconv)),
                Err(call_conv_err) => {
                    return Err(self.error_at_token(&token, ParserDiagKind::InvalidModifier(call_conv_err.to_string())));
                }
            }
        } else {
            Ok(None)
        }
    }

    pub(crate) fn parse_export_kind(&mut self, token: Token) -> Option<ExportKind> {
        if matches!(token.kind, TokenKind::DllExport) {
            self.next_token();
            Some(ExportKind::DllExport)
        } else if matches!(token.kind, TokenKind::DllImport) {
            self.next_token();
            Some(ExportKind::DllImport)
        } else {
            None
        }
    }

    pub(crate) fn parse_prologue(&mut self, token: Token) -> Option<Prologue> {
        if matches!(token.kind, TokenKind::Naked) {
            self.next_token();
            Some(Prologue::Naked)
        } else {
            None
        }
    }

    pub(crate) fn parse_inlining(&mut self, token: Token) -> Option<Inlining> {
        if matches!(token.kind, TokenKind::Inline) {
            self.next_token();
            Some(Inlining::Inline)
        } else if matches!(token.kind, TokenKind::NoInline) {
            self.next_token();
            Some(Inlining::NoInline)
        } else if matches!(token.kind, TokenKind::AlwaysInline) {
            self.next_token();
            Some(Inlining::AlwaysInline)
        } else {
            None
        }
    }

    pub(crate) fn parse_linkage(&mut self, token: Token) -> Result<Option<Linkage>, Diag> {
        if matches!(token.kind, TokenKind::Extern) {
            self.next_token();

            if matches!(self.current_token().kind, TokenKind::Literal(..)) {
                let extern_abi = self.parse_string_without_prefix()?;
                self.next_token();

                let Ok(callconv) = CallConv::try_from(extern_abi.clone()) else {
                    return Err(self.error_at_token(&token, ParserDiagKind::InvalidABI(extern_abi)));
                };

                return Ok(Some(Linkage::Extern(Some(callconv))));
            }

            Ok(Some(Linkage::Extern(None)))
        } else if matches!(token.kind, TokenKind::Weak) {
            self.next_token();
            Ok(Some(Linkage::Weak))
        } else if matches!(token.kind, TokenKind::LinkOnce) {
            self.next_token();
            Ok(Some(Linkage::LinkOnce))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn parse_vis(&mut self, token: Token) -> Option<Visibility> {
        if matches!(token.kind, TokenKind::Public) {
            self.next_token();
            Some(Visibility::Public)
        } else {
            None
        }
    }
}

impl UnresolvedModifiers {
    pub(crate) fn into_module_decl_modifiers(self, loc: Loc) -> Result<ModuleDeclModifiers, Diag> {
        let vis = self.visibility.unwrap_or_default();

        if self.linkage.is_some()
            || self.inline.is_some()
            || self.prologue.is_some()
            || self.export.is_some()
            || self.callconv.is_some()
            || self.repr_attr.is_some()
            || !self.optional_flags.is_empty()
            || !self.placement.is_empty()
        {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Module declarations can only have visibility modifiers.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        Ok(ModuleDeclModifiers { vis })
    }

    pub(crate) fn into_func_modifiers(self, loc: Loc) -> Result<FuncModifiers, Diag> {
        let vis = self.visibility.unwrap_or_default();

        if !self.placement.is_empty() && self.placement.len() > 1 {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Multiple section placements are not allowed for functions.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        let section = self.placement.get(0).cloned();

        let func_modifiers = FuncModifiers {
            vis,
            linkage: self.linkage,
            inline: self.inline,
            prologue: self.prologue,
            export: self.export,
            callconv: self.callconv,
            optional_flags: self.optional_flags,
            placement: self.placement,
            section,
        };

        if func_modifiers.export.is_some() && matches!(func_modifiers.inline, Some(Inlining::AlwaysInline)) {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Function cannot be both exported and always inlined.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        if self.repr_attr.is_some() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Attribute 'repr' cannot be applied to functions.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: Some("only data types like structs and enums can have a 'repr' attribute.".to_string()),
            });
        }

        Ok(func_modifiers)
    }

    pub(crate) fn into_struct_modifiers(self, loc: Loc) -> Result<StructModifiers, Diag> {
        let vis = self.visibility.unwrap_or_default();

        if self.linkage.is_some()
            || self.inline.is_some()
            || self.prologue.is_some()
            || self.export.is_some()
            || self.callconv.is_some()
            || !self.optional_flags.is_empty()
            || !self.placement.is_empty()
        {
            return Err(Diag {
            kind: Box::new(ParserDiagKind::InvalidModifier(
                "Only visibility and repr attributes are allowed on struct declarations."
                    .to_string(),
            )),
            level: DiagLevel::Error,
            loc: Some(loc),
            hint: Some(
                "Remove unsupported modifiers such as linkage, export, inline, callconv, flags, or section placement."
                    .to_string(),
            ),
        });
        }

        if let Some(repr) = &self.repr_attr {
            if repr.is_packed() {
                if let Some(kind) = repr.kind() {
                    if matches!(kind, ReprKind::Transparent) {
                        return Err(Diag {
                            kind: Box::new(ParserDiagKind::InvalidModifier(
                                "repr(transparent) cannot be combined with repr(packed).".to_string(),
                            )),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some("Remove either 'packed' or 'transparent'.".to_string()),
                        });
                    }
                }
            }
        }

        Ok(StructModifiers {
            vis,
            repr_attr: self.repr_attr,
        })
    }

    pub(crate) fn into_enum_modifiers(self, loc: Loc) -> Result<EnumModifiers, Diag> {
        let vis = self.visibility.unwrap_or_default();

        // Enums only allow: visibility + repr_attr.
        if self.linkage.is_some()
            || self.inline.is_some()
            || self.prologue.is_some()
            || self.export.is_some()
            || self.callconv.is_some()
            || !self.optional_flags.is_empty()
            || !self.placement.is_empty()
        {
            return Err(Diag {
            kind: Box::new(ParserDiagKind::InvalidModifier(
                "Only visibility and repr attributes are allowed on enum declarations."
                    .to_string(),
            )),
            level: DiagLevel::Error,
            loc: Some(loc),
            hint: Some(
                "Remove unsupported modifiers such as inline, export, callconv, section placement, or optional flags."
                    .to_string(),
            ),
        });
        }

        if let Some(repr) = &self.repr_attr {
            // packed enums not allowed
            if repr.is_packed() {
                return Err(Diag {
                    kind: Box::new(ParserDiagKind::InvalidModifier(
                        "repr(packed) is not supported for enums.".to_string(),
                    )),
                    level: DiagLevel::Error,
                    loc: Some(loc),
                    hint: Some(
                        "Packed enums are not safe. Use repr(C) with a fixed-size tag field instead.".to_string(),
                    ),
                });
            }

            // only repr(c) & repr(cyrus) allowed
            if let Some(kind) = repr.kind() {
                match kind {
                    ReprKind::C | ReprKind::Cyrus => { /* valid */ }
                    ReprKind::Transparent => {
                        return Err(Diag {
                            kind: Box::new(ParserDiagKind::InvalidModifier(
                                "repr(transparent) is not valid for enums.".to_string(),
                            )),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some(
                                "Transparent representation applies only to structs with exactly one field."
                                    .to_string(),
                            ),
                        });
                    }
                }
            }
        }

        Ok(EnumModifiers {
            vis,
            repr_attr: self.repr_attr,
        })
    }

    pub(crate) fn into_union_modifiers(self, loc: Loc) -> Result<UnionModifiers, Diag> {
        let vis = self.visibility.unwrap_or_default();

        // Unions only allow visibility and repr
        if self.linkage.is_some()
            || self.inline.is_some()
            || self.prologue.is_some()
            || self.export.is_some()
            || self.callconv.is_some()
            || !self.optional_flags.is_empty()
            || !self.placement.is_empty()
        {
            return Err(Diag {
            kind: Box::new(ParserDiagKind::InvalidModifier(
                "Only visibility and repr attributes are allowed on union declarations."
                    .to_string(),
            )),
            level: DiagLevel::Error,
            loc: Some(loc),
            hint: Some(
                "Remove unsupported modifiers such as linkage, inline, export, callconv, flags, or section placement."
                    .to_string(),
            ),
        });
        }

        // Validate repr attribute
        if let Some(repr) = &self.repr_attr {
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

            if let Some(kind) = repr.kind() {
                match kind {
                    ReprKind::C | ReprKind::Cyrus => {}
                    ReprKind::Transparent => {
                        return Err(Diag {
                            kind: Box::new(ParserDiagKind::InvalidModifier(
                                "repr(transparent) is not valid for unions.".to_string(),
                            )),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some(
                                "Transparent representation requires exactly one field and is only valid for structs."
                                    .to_string(),
                            ),
                        });
                    }
                }
            }
        }

        Ok(UnionModifiers {
            vis,
            repr_attr: self.repr_attr,
        })
    }

    pub(crate) fn into_global_var_modifiers(self, loc: Loc) -> Result<GlobalVarModifiers, Diag> {
        let vis = self.visibility.unwrap_or_default();

        if self.inline.is_some() || self.prologue.is_some() || self.callconv.is_some() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Invalid modifier for global variable declaration.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        if self.placement.len() > 1 {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Multiple section placements are not allowed for global variables.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        if self.repr_attr.is_some() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Global variables cannot have repr.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: Some("Use 'align(n)' instead if you need alignment.".to_string()),
            });
        }

        if !self.optional_flags.is_empty() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Global variables cannot have optional flags.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        let section = self.placement.get(0).cloned();

        Ok(GlobalVarModifiers {
            vis,
            linkage: self.linkage.clone(),
            export: self.export,
            section,
            placement: self.placement,
            weak: matches!(self.linkage, Some(Linkage::Weak)),
        })
    }

    pub(crate) fn into_interface_modifiers(self, loc: Loc) -> Result<InterfaceModifiers, Diag> {
        let vis = self.visibility.unwrap_or_default();

        if self.linkage.is_some()
            || self.inline.is_some()
            || self.prologue.is_some()
            || self.export.is_some()
            || self.callconv.is_some()
            || self.repr_attr.is_some()
            || !self.optional_flags.is_empty()
            || !self.placement.is_empty()
        {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Interfaces can only have visibility modifiers.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        Ok(InterfaceModifiers { vis })
    }

    pub(crate) fn into_typedef_modifiers(self, loc: Loc) -> Result<TypedefModifiers, Diag> {
        let vis = self.visibility.unwrap_or_default();

        if self.linkage.is_some()
            || self.inline.is_some()
            || self.prologue.is_some()
            || self.export.is_some()
            || self.repr_attr.is_some()
            || self.callconv.is_some()
            || !self.optional_flags.is_empty()
            || !self.placement.is_empty()
        {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Typedefs can only have visibility modifiers.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        Ok(TypedefModifiers { vis })
    }

    pub(crate) fn into_method_modifiers(self, loc: Loc) -> Result<FuncModifiers, Diag> {
        let vis = self.visibility.unwrap_or_default();

        if self.export.is_some() || self.linkage.is_some() || !self.placement.is_empty() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Methods cannot use export, linkage, or section placement.".into(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        let linkage = vis.is_public().then_some(Linkage::Extern(None));

        Ok(FuncModifiers {
            vis,
            inline: self.inline,
            prologue: self.prologue,
            callconv: self.callconv,
            optional_flags: self.optional_flags,
            linkage,
            export: None,
            section: None,
            placement: Vec::new(),
        })
    }

    pub(crate) fn into_field_modifiers(self, loc: Loc) -> Result<FieldModifiers, Diag> {
        let vis = self.visibility.unwrap_or_default();

        if self.linkage.is_some()
            || self.inline.is_some()
            || self.export.is_some()
            || self.callconv.is_some()
            || self.prologue.is_some()
            || self.repr_attr.is_some()
            || !self.optional_flags.is_empty()
            || !self.placement.is_empty()
        {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Only visibility modifier allowed for fields.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        Ok(FieldModifiers { vis })
    }
}
