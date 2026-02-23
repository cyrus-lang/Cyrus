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

use crate::mangler::{ABINameMangler, C_ABI, CYRUS_ABI};
use std::collections::HashSet;

macro_rules! define_call_convs {
    ($( $variant:ident => $str:expr ),* $(,)?) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum CallConv {
            $( $variant ),*
        }

        #[derive(Debug, Clone)]
        pub struct ParseCallConvError(pub String);

        impl std::fmt::Display for ParseCallConvError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "Invalid call convention: '{}'.", self.0)
            }
        }

        impl std::error::Error for ParseCallConvError {}

        impl std::convert::TryFrom<String> for CallConv {
            type Error = ParseCallConvError;

            fn try_from(value: String) -> Result<Self, Self::Error> {
                CallConv::try_from(value.as_str())
            }
        }

        impl std::convert::TryFrom<&str> for CallConv {
            type Error = ParseCallConvError;

            fn try_from(value: &str) -> Result<Self, Self::Error> {
                match value.to_lowercase().as_str() {
                    $( $str => Ok(CallConv::$variant), )*
                    other => Err(ParseCallConvError(other.to_string())),
                }
            }
        }

        impl std::fmt::Display for CallConv {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $( CallConv::$variant => write!(f, "{}", $str), )*
                }
            }
        }
    };
}

define_call_convs! {
    C => "c",
    Naked => "naked",
    Interrupt => "interrupt",
    Fast => "fast",
    Cold => "cold",
    Aapcs => "aapcs",
    Stdcall => "stdcall",
    Fastcall => "fastcall",
    Thiscall => "thiscall",
    Vectorcall => "vectorcall",
    SysV64 => "sysv64",
    Win64 => "win64",
    System => "system",
}

impl Default for CallConv {
    fn default() -> Self {
        Self::System
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportKind {
    DllImport,
    DllExport,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OptionalFlag {
    NoReturn,
    NoUnwind,
    Cold,
    Hot,
    OptSize,
    OptNone,
    NoSanitize(String),
}

pub fn validate_flags(flags: &[OptionalFlag]) -> Result<Vec<OptionalFlag>, String> {
    let mut seen = HashSet::new();
    let mut has_opt_size = false;
    let mut has_opt_none = false;
    let mut has_hot = false;
    let mut has_cold = false;

    for flag in flags {
        match flag {
            OptionalFlag::NoSanitize(name) => {
                if !seen.insert(OptionalFlag::NoSanitize(name.clone())) {
                    return Err(format!("Duplicate nosanitize flag: '{}'.", name));
                }
            }
            OptionalFlag::OptSize => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate optsize flag.".into());
                }
                has_opt_size = true;
            }
            OptionalFlag::OptNone => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate optnone flag.".into());
                }
                has_opt_none = true;
            }
            OptionalFlag::Hot => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate hot flag.".into());
                }
                has_hot = true;
            }
            OptionalFlag::Cold => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate cold flag.".into());
                }
                has_cold = true;
            }
            _ => {
                if !seen.insert(flag.clone()) {
                    return Err(format!("Duplicate flag: {:?}", flag));
                }
            }
        }
    }

    if has_opt_size && has_opt_none {
        return Err("Cannot use both 'optsize' and 'optnone' flags together.".into());
    }
    if has_hot && has_cold {
        return Err("Cannot use both 'hot' and 'cold' flags together.".into());
    }

    Ok(flags.to_vec())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Inlining {
    Inline,
    NoInline,
    AlwaysInline,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Linkage {
    Extern(Option<CallConv>),
    Weak,
    LinkOnce,
}

impl Linkage {
    pub fn abi_mangler(&self) -> &'static dyn ABINameMangler {
        match self {
            Linkage::Extern(call_conv_opt) => match call_conv_opt {
                Some(call_conv) => match call_conv {
                    CallConv::C
                    | CallConv::Stdcall
                    | CallConv::Fastcall
                    | CallConv::Thiscall
                    | CallConv::Vectorcall
                    | CallConv::SysV64
                    | CallConv::Win64
                    | CallConv::System => &*C_ABI,

                    CallConv::Naked | CallConv::Interrupt | CallConv::Fast | CallConv::Cold | CallConv::Aapcs => {
                        &*CYRUS_ABI
                    }
                },
                None => &*C_ABI,
            },
            Linkage::Weak => &*CYRUS_ABI,
            Linkage::LinkOnce => &*CYRUS_ABI,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SectionAttr(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReprKind {
    C,
    Cyrus,
    Transparent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReprAttrKind {
    Kind(ReprKind),
    Align(u32),
}

#[derive(Debug, Clone)]
pub struct ReprAttr {
    pub items: Vec<ReprAttrKind>,
}

impl ReprAttr {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn push(&mut self, item: ReprAttrKind) {
        self.items.push(item);
    }

    pub fn try_kind_from_str(s: &str) -> Result<ReprKind, String> {
        match s.to_lowercase().as_str() {
            "c" => Ok(ReprKind::C),
            "cyrus" => Ok(ReprKind::Cyrus),
            "transparent" => Ok(ReprKind::Transparent),
            _ => Err(format!("Unknown repr kind '{}'.", s)),
        }
    }

    pub fn align(&self) -> Option<u32> {
        self.items
            .iter()
            .find_map(|i| if let ReprAttrKind::Align(v) = i { Some(*v) } else { None })
    }

    pub fn kind(&self) -> Option<ReprKind> {
        self.items.iter().find_map(|i| {
            if let ReprAttrKind::Kind(k) = i {
                Some(k.clone())
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Private
    }
}

impl Visibility {
    pub fn is_private(&self) -> bool {
        matches!(self, Visibility::Private)
    }

    pub fn is_public(&self) -> bool {
        matches!(self, Visibility::Public)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prologue {
    Naked,
}

impl Prologue {
    pub fn conflicts_with_inline(&self) -> bool {
        matches!(self, Prologue::Naked)
    }
}
