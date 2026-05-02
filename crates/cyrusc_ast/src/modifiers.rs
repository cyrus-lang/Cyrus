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

use crate::abi::{
    CallConv, ExportKind, Inlining, Linkage, OptionalFlag, Prologue, ReprAttr, ReprKind, SectionAttr, Visibility,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncModifiers {
    pub linkage: Option<Linkage>,
    pub inline: Option<Inlining>,
    pub prologue: Option<Prologue>,
    pub export: Option<ExportKind>,
    pub callconv: Option<CallConv>,
    pub optional_flags: Vec<OptionalFlag>,
    pub placement: Vec<SectionAttr>,
    pub section: Option<SectionAttr>,
    pub vis: Visibility,
}

impl Default for FuncModifiers {
    fn default() -> Self {
        Self {
            linkage: None,
            inline: None,
            prologue: None,
            export: None,
            callconv: None,
            section: None,
            optional_flags: Vec::new(),
            placement: Vec::new(),
            vis: Visibility::default(),
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
                if *cc != CallConv::Naked {
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

    pub fn add_placement(&mut self, section: SectionAttr) {
        self.placement.push(section);
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
            if let Some(kind) = repr_attr.kind() {
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
    pub linkage: Option<Linkage>,
    pub export: Option<ExportKind>,
    pub section: Option<SectionAttr>,
    pub placement: Vec<SectionAttr>,
    pub weak: bool,
}

impl Default for GlobalVarModifiers {
    fn default() -> Self {
        Self {
            vis: Visibility::default(),
            linkage: None,
            export: None,
            section: None,
            placement: Vec::new(),
            weak: false,
        }
    }
}
