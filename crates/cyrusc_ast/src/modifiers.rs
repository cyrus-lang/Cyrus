// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::abi::{Callconv, Extern, Inlining, OptionalFlag, Prologue, ReprAttr, ReprKind, Visibility};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncModifiers {
    pub vis: Visibility,
    pub link_name: Option<String>,
    pub extrn: Option<Extern>,
    pub inline: Option<Inlining>,
    pub prologue: Option<Prologue>,
    pub callconv: Option<Callconv>,
    pub optional_flags: Vec<OptionalFlag>,
    pub section: Option<String>,
    pub link_once: bool,
    pub weak: bool,
}

impl Default for FuncModifiers {
    fn default() -> Self {
        Self {
            link_name: None,
            extrn: None,
            inline: None,
            prologue: None,
            callconv: None,
            section: None,
            optional_flags: Vec::new(),
            vis: Visibility::default(),
            link_once: false,
            weak: false,
        }
    }
}

impl FuncModifiers {
    pub fn validate(&self) -> Result<(), String> {
        if let (Some(Prologue::Naked), Some(_)) = (self.prologue, self.inline) {
            return Err("Cannot combine 'naked' prologue with any inline modifier.".into());
        }

        if let Some(Prologue::Naked) = self.prologue {
            if let Some(cc) = &self.callconv {
                if *cc != Callconv::Naked {
                    return Err("Naked prologue must use callconv(naked).".into());
                }
            } else {
                return Err("Naked prologue requires callconv(naked).".into());
            }
        }

        Ok(())
    }

    pub fn add_optional_flag(&mut self, flag: OptionalFlag) {
        self.optional_flags.push(flag);
    }
}

#[derive(Debug, Clone)]
pub struct StructModifiers {
    pub vis: Visibility,
    pub repr_attr: Option<ReprAttr>,
}

impl Default for StructModifiers {
    fn default() -> Self {
        Self {
            vis: Visibility::default(),
            repr_attr: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumModifiers {
    pub vis: Visibility,
    pub repr_attr: Option<ReprAttr>,
}

impl Default for EnumModifiers {
    fn default() -> Self {
        Self {
            vis: Visibility::default(),
            repr_attr: None,
        }
    }
}

impl EnumModifiers {
    pub fn validate(&self) -> Result<(), String> {
        if let Some(repr_attr) = &self.repr_attr {
            if let Some((kind, _)) = repr_attr.kind() {
                match kind {
                    ReprKind::C | ReprKind::Cyrus => {
                        if repr_attr.is_packed() {
                            return Err("Cannot combine 'packed' with enum layout.".into());
                        }
                    }
                    ReprKind::Transparent => {
                        return Err("Repr 'transparent' cannot be applied to enums.".into());
                    }
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct UnionModifiers {
    pub vis: Visibility,
    pub repr_attr: Option<ReprAttr>,
}

impl Default for UnionModifiers {
    fn default() -> Self {
        Self {
            vis: Visibility::default(),
            repr_attr: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVarModifiers {
    pub vis: Visibility,
    pub link_name: Option<String>,
    pub extrn: Option<Extern>,
    pub section: Option<String>,
    pub thread_local: bool,
    pub weak: bool,
    pub link_once: bool,
}

impl Default for GlobalVarModifiers {
    fn default() -> Self {
        Self {
            vis: Visibility::default(),
            link_name: None,
            extrn: None,
            section: None,
            thread_local: false,
            weak: false,
            link_once: false,
        }
    }
}
