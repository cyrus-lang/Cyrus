// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{Parser, diagnostics::ParserDiagKind};
use cyrusc_ast::TypeSpecifier;
use cyrusc_ast::abi::{Extern, Visibility};
use cyrusc_ast::modifiers::*;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_tokens::{Token, TokenKind};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct UnresolvedModifiers {
    pub vis: Option<Visibility>,
    pub extrn: Option<Extern>,
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
        let mut modifiers = UnresolvedModifiers { vis: None, extrn: None };

        loop {
            let token = self.current_token().clone();
            let mut consumed = false;

            macro_rules! try_set_once {
                ($field:ident, $parser_method:ident, $err_msg:expr) => {
                    if let Some(value) = self.$parser_method(token.clone()) {
                        if modifiers.$field.is_some() {
                            return Err(self.error_at_token(
                                &token,
                                ParserDiagKind::InvalidModifier($err_msg.to_string()),
                            ));
                        }
                        modifiers.$field = Some(value);
                        consumed = true;
                    }
                };
            }

            macro_rules! try_set_once_result {
                ($field:ident, $parser_method:ident, $err_msg:expr) => {
                    match self.$parser_method(token.clone())? {
                        Some(value) => {
                            if modifiers.$field.is_some() {
                                return Err(self.error_at_token(
                                    &token,
                                    ParserDiagKind::InvalidModifier($err_msg.to_string()),
                                ));
                            }
                            modifiers.$field = Some(value);
                            consumed = true;
                        }
                        None => {}
                    }
                };
            }

            try_set_once!(vis, parse_vis, "Visibility modifier already specified.");
            try_set_once_result!(extrn, parse_extern, "Linkage modifier already specified.");

            if !consumed {
                break;
            }
        }

        Ok(modifiers)
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

    pub(crate) fn parse_extern(&mut self, token: Token) -> Result<Option<Extern>, Diag> {
        if matches!(token.kind, TokenKind::Extern) {
            self.next_token();

            let abi = self.parse_string_without_prefix()?;
            self.next_token();

            let extrn = match abi.to_lowercase().as_str() {
                "c" => Extern::C,
                "cyrus" => Extern::Cyrus,
                _ => {
                    return Err(self.error_at_token(&token, ParserDiagKind::InvalidABI(abi)));
                }
            };

            return Ok(Some(extrn));
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
    pub(crate) fn into_module_decl_modifiers(&self, loc: Loc) -> Result<ModuleDeclModifiers, Diag> {
        let vis = self.vis.unwrap_or_default();

        if self.extrn.is_some() {
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

    pub(crate) fn into_func_modifiers(&self) -> Result<FuncModifiers, Diag> {
        let vis = self.vis.unwrap_or_default();

        let func_modifiers = FuncModifiers {
            vis,
            extrn: self.extrn,
            ..Default::default()
        };

        Ok(func_modifiers)
    }

    pub(crate) fn into_struct_modifiers(&self, loc: Loc) -> Result<StructModifiers, Diag> {
        let vis = self.vis.unwrap_or_default();

        if self.extrn.is_some() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Only visibility modifier is allowed on struct declarations.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        Ok(StructModifiers { vis, repr_attr: None })
    }

    pub(crate) fn into_enum_modifiers(&self, loc: Loc) -> Result<EnumModifiers, Diag> {
        let vis = self.vis.unwrap_or_default();

        if self.extrn.is_some() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Only visibility modifier is allowed on enum declarations.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        Ok(EnumModifiers { vis, repr_attr: None })
    }

    pub(crate) fn into_union_modifiers(&self, loc: Loc) -> Result<UnionModifiers, Diag> {
        let vis = self.vis.unwrap_or_default();

        if self.extrn.is_some() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Only visibility modifier is allowed on union declarations.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        Ok(UnionModifiers { vis, repr_attr: None })
    }

    pub(crate) fn into_global_var_modifiers(self) -> Result<GlobalVarModifiers, Diag> {
        let vis = self.vis.unwrap_or_default();

        Ok(GlobalVarModifiers {
            vis,
            extrn: self.extrn,
            ..Default::default()
        })
    }

    pub(crate) fn into_interface_modifiers(&self, loc: Loc) -> Result<InterfaceModifiers, Diag> {
        let vis = self.vis.unwrap_or_default();

        if self.extrn.is_some() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Interfaces can only have visibility modifier.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        Ok(InterfaceModifiers { vis })
    }

    pub(crate) fn into_typedef_modifiers(&self, loc: Loc) -> Result<TypedefModifiers, Diag> {
        let vis = self.vis.unwrap_or_default();

        if self.extrn.is_some() {
            return Err(Diag {
                kind: Box::new(ParserDiagKind::InvalidModifier(
                    "Typedef can only have visibility modifier.".to_string(),
                )),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: None,
            });
        }

        Ok(TypedefModifiers { vis })
    }

    pub(crate) fn into_method_modifiers(&self) -> Result<FuncModifiers, Diag> {
        let vis = self.vis.unwrap_or_default();

        Ok(FuncModifiers {
            vis,
            ..Default::default()
        })
    }

    pub(crate) fn into_field_modifiers(&self, loc: Loc) -> Result<FieldModifiers, Diag> {
        let vis = self.vis.unwrap_or_default();

        if self.extrn.is_some() {
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
