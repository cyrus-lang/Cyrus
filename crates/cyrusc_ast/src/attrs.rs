// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::abi::{Callconv, Inlining, ReprAttr};
use cyrusc_source_loc::Loc;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Attr {
    pub kind: AttrKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum AttrKind {
    Link(LinkAttr),
    Inline(Inlining),
    Callconv(Callconv),
    NoSanitize(String),
    Section(String),
    Repr(ReprAttr),
    ThreadLocal,
    Naked,
    Cold,
    Hot,
    OptNone,
    OptSize,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LinkAttr {
    Weak,
    LinkOnce,
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
            AttrKind::Section(name) => write!(f, "section({name})")?,
            AttrKind::Repr(repr_attr) => {
                write!(f, "repr(")?;
                repr_attr
                    .items
                    .iter()
                    .map(|(item, _)| item.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, ")")?;
            }
            AttrKind::ThreadLocal => write!(f, "thread_local")?,
            AttrKind::Link(link_attr) => {
                write!(f, "link")?;
                write!(f, "(")?;
                match link_attr {
                    LinkAttr::Weak => write!(f, "weak")?,
                    LinkAttr::LinkOnce => write!(f, "once")?,
                };
                write!(f, ")")?;
            }
        };
        write!(f, "]]")?;
        Ok(())
    }
}
