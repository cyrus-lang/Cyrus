// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use crate::callconv::CallConv;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Linkage {
    Extern(Option<CallConv>),
    Weak,
    LinkOnce,
}

impl Linkage {
    pub fn is_exclusive(&self) -> bool {
        true
    }
}
