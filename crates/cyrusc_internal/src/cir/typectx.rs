// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{abi::layout::ABITypeLayout, cir::types::CIRType};
use fx_hash::{FxHashMap, FxHashMapExt};

pub type CIRTypeContextID = usize;

pub struct CIRTypeContext {
    // Lazily evaluated to prevent infinite recursion when storing recursive types.
    types: Vec<Option<CIRType>>,

    // Cache computed layouts of types.
    layouts: FxHashMap<CIRType, ABITypeLayout>,
}

impl CIRTypeContext {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            layouts: FxHashMap::new(),
        }
    }
}
