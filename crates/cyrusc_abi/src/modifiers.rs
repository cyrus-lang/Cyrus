// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use crate::{
    callconv::CallConv, export::ExportKind, flags::OptionalFlag, inline::Inlining, linkage::Linkage,
    placement::SectionAttr, prologue::Prologue, repr::ReprAttr, visibility::Visibility,
};

#[derive(Debug, Clone)]
pub struct FuncModifiers {
    pub vis: Visibility,
    pub linkage: Option<Linkage>,
    pub inline: Option<Inlining>,
    pub prologue: Option<Prologue>,
    pub export: Option<ExportKind>,
    pub callconv: Option<CallConv>,
    pub optional_flags: Vec<OptionalFlag>,
    pub placement: Vec<SectionAttr>,
    pub section: Option<SectionAttr>,
}

impl Default for FuncModifiers {
    fn default() -> Self {
        Self {
            vis: Visibility::default(),
            linkage: None,
            inline: None,
            prologue: None,
            export: None,
            callconv: None,
            section: None,
            optional_flags: Vec::new(),
            placement: Vec::new(),
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
    pub linkage: Option<Linkage>,
    pub export: Option<ExportKind>,
    pub repr: Option<ReprAttr>,
    pub packed: bool,
    pub section: Option<SectionAttr>,
    pub optional_flags: Vec<OptionalFlag>,
}

impl Default for StructModifiers {
    fn default() -> Self {
        Self {
            vis: Visibility::default(),
            linkage: None,
            export: None,
            repr: None,
            packed: false,
            section: None,
            optional_flags: Vec::new(),
        }
    }
}

impl StructModifiers {
    pub fn validate(&self) -> Result<(), String> {
        if self.repr.is_some() && self.packed {
            // this is allowed in most languages, but let's keep it open for custom validation
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct EnumModifiers {
    pub vis: Visibility,
    pub export: Option<ExportKind>,
    pub repr: Option<ReprAttr>,
    pub section: Option<SectionAttr>,
    pub optional_flags: Vec<OptionalFlag>,
}

impl Default for EnumModifiers {
    fn default() -> Self {
        Self {
            vis: Visibility::default(),
            export: None,
            repr: None,
            section: None,
            optional_flags: Vec::new(),
        }
    }
}

impl EnumModifiers {
    pub fn validate(&self) -> Result<(), String> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct UnionModifiers {
    pub vis: Visibility,
    pub linkage: Option<Linkage>,
    pub export: Option<ExportKind>,
    pub repr: Option<ReprAttr>,
    pub section: Option<SectionAttr>,
    pub optional_flags: Vec<OptionalFlag>,
}

impl Default for UnionModifiers {
    fn default() -> Self {
        Self {
            vis: Visibility::default(),
            linkage: None,
            export: None,
            repr: None,
            section: None,
            optional_flags: Vec::new(),
        }
    }
}

impl UnionModifiers {
    pub fn validate(&self) -> Result<(), String> {
        Ok(())
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
    pub optional_flags: Vec<OptionalFlag>,
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
            optional_flags: Vec::new(),
        }
    }
}

impl GlobalVarModifiers {
    pub fn validate(&self) -> Result<(), String> {
        if self.weak && self.linkage == None {
            return Err("Global variable cannot be both weak and internal.".into());
        }
        Ok(())
    }
}
