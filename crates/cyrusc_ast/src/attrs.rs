// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::abi::{Callconv, Inlining};
use cyrusc_source_loc::Loc;
use std::fmt;

pub struct Attr {
    pub kind: AttrKind,
    pub loc: Loc,
}

pub enum AttrKind {
    Inline(Inlining),
    Callconv(Callconv),
    NoSanitize(String),
    Naked,
    Cold,
    Hot,
    OptNone,
    OptSize,
}

impl fmt::Display for Attr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Display for AttrKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[[")?;
        match self {
            AttrKind::Inline(inline_attr) => {
                write!(f, "inline")?;

                match inline_attr {
                    Inlining::Always => write!(f, "(always)")?,
                    Inlining::Never => write!(f, "(never)")?,
                    Inlining::Hint => {}
                }
            }
            AttrKind::Callconv(callconv) => write!(f, "callconv({callconv})")?,
            AttrKind::NoSanitize(name) => write!(f, "nosanitize(\"{name}\")")?,
            AttrKind::OptNone => write!(f, "optimize(none)")?,
            AttrKind::OptSize => write!(f, "optimize(size)")?,
            AttrKind::Cold => write!(f, "cold")?,
            AttrKind::Hot => write!(f, "hot")?,
            AttrKind::Naked => write!(f, "naked")?,
        };
        write!(f, "]]")?;
        Ok(())
    }
}
