// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

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
