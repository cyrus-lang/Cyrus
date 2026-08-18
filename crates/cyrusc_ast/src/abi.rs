// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use core::fmt;
use std::collections::HashSet;

use cyrusc_source_loc::Loc;

macro_rules! define_call_convs {
    ($( $variant:ident => $str:expr ),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Callconv {
            $( $variant ),*
        }

        #[derive(Debug, Clone)]
        pub struct ParseCallConvError(pub String);

        impl std::fmt::Display for ParseCallConvError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "Invalid call convention '{}'.", self.0)
            }
        }

        impl std::error::Error for ParseCallConvError {}

        impl std::convert::TryFrom<String> for Callconv {
            type Error = ParseCallConvError;

            fn try_from(value: String) -> Result<Self, Self::Error> {
                Callconv::try_from(value.as_str())
            }
        }

        impl std::convert::TryFrom<&str> for Callconv {
            type Error = ParseCallConvError;

            fn try_from(value: &str) -> Result<Self, Self::Error> {
                match value.to_lowercase().as_str() {
                    $( $str => Ok(Callconv::$variant), )*
                    other => Err(ParseCallConvError(other.to_string())),
                }
            }
        }

        impl std::fmt::Display for Callconv {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $( Callconv::$variant => $str.fmt(f), )*
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Extern {
    C,
    Cyrus,
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
                    return Err(format!("Duplicate nosanitize flag '{}'.", name));
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
                    return Err(format!("Duplicate flag '{:?}'.", flag));
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
    Hint,
    Never,
    Always,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ReprKind {
    C,
    Cyrus,
    Transparent,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ReprAttrKind {
    Kind(ReprKind),
    Packed,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReprAttr {
    pub items: Vec<(ReprAttrKind, Loc)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prologue {
    Naked,
}

impl Default for Callconv {
    fn default() -> Self {
        Self::System
    }
}

impl ReprAttr {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn add(&mut self, item: ReprAttrKind, loc: Loc) -> Result<(), String> {
        // check for duplicate kind
        if let ReprAttrKind::Kind(_) = &item {
            if let Some(_) = self.kind() {
                return Err("Multiple repr kinds specified.".into());
            }
        }

        // check for duplicate packed
        if let ReprAttrKind::Packed = item {
            if self.is_packed() {
                return Err("Duplicate packed modifier.".into());
            }
            self.items.push((ReprAttrKind::Packed, loc));
            return Ok(());
        }

        if let ReprAttrKind::Kind(kind) = item {
            self.items.push((ReprAttrKind::Kind(kind), loc));
        }

        Ok(())
    }

    pub fn is_packed(&self) -> bool {
        for item in &self.items {
            if let ReprAttrKind::Packed = item.0 {
                return true;
            }
        }
        false
    }

    pub fn try_kind_from_str(s: &str) -> Result<ReprKind, String> {
        match s.to_lowercase().as_str() {
            "c" => Ok(ReprKind::C),
            "cyrus" => Ok(ReprKind::Cyrus),
            "transparent" => Ok(ReprKind::Transparent),
            _ => Err(format!("Unknown repr kind '{}'.", s)),
        }
    }

    pub fn kind(&self) -> Option<(ReprKind, Loc)> {
        self.items.iter().find_map(|(attr, loc)| {
            if let ReprAttrKind::Kind(kind) = attr {
                Some((kind.clone(), *loc))
            } else {
                None
            }
        })
    }
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Private
    }
}

impl Visibility {
    pub fn is_private(&self) -> bool {
        *self == Visibility::Private
    }

    pub fn is_public(&self) -> bool {
        *self == Visibility::Public
    }
}

impl Prologue {
    pub fn conflicts_with_inline(&self) -> bool {
        matches!(self, Prologue::Naked)
    }
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Visibility::Public => write!(f, "pub"),
            Visibility::Private => write!(f, ""),
        }
    }
}

impl fmt::Display for ReprAttr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let items_fmt = self
            .items
            .iter()
            .map(|(item, _)| item.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "repr({})", items_fmt)
    }
}

impl fmt::Display for ReprAttrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReprAttrKind::Packed => write!(f, "packed"),
            ReprAttrKind::Kind(repr_kind) => repr_kind.fmt(f),
        }
    }
}

impl fmt::Display for ReprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReprKind::C => write!(f, "c"),
            ReprKind::Cyrus => write!(f, "cyrus"),
            ReprKind::Transparent => write!(f, "transparent"),
        }
    }
}
