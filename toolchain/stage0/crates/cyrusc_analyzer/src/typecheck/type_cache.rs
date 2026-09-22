// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_typed_ast::{decls::DeclID, types::SemaType};
use fx_hash::{FxHashMap, FxHashMapExt};
use smallvec::SmallVec;

#[derive(Clone)]
pub enum ResolutionState {
    Resolving,
    Resolved(SemaType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ActiveFrame {
    pub decl: DeclID,
    pub indirection: u8,
}

pub enum TypeCacheEnterResult {
    Entered,
    AlreadyResolving(ActiveFrame),
}

pub struct TypeCache {
    states: FxHashMap<DeclID, ResolutionState>,
    stack: SmallVec<[ActiveFrame; 16]>,
}

impl TypeCache {
    pub fn new() -> Self {
        Self {
            states: FxHashMap::new(),
            stack: SmallVec::new(),
        }
    }

    pub fn get(&self, decl: DeclID) -> Option<&SemaType> {
        match self.states.get(&decl) {
            Some(ResolutionState::Resolved(ty)) => Some(ty),
            _ => None,
        }
    }

    pub fn enter(&mut self, decl: DeclID, indirection: u8) -> TypeCacheEnterResult {
        if let Some(frame) = self.stack.iter().rev().find(|f| f.decl == decl) {
            return TypeCacheEnterResult::AlreadyResolving(*frame);
        }

        self.states.insert(decl, ResolutionState::Resolving);
        self.stack.push(ActiveFrame { decl, indirection });

        TypeCacheEnterResult::Entered
    }

    pub fn leave(&mut self, decl: DeclID, ty: SemaType) {
        debug_assert_eq!(self.stack.last().unwrap().decl, decl);

        self.stack.pop();

        self.states.insert(decl, ResolutionState::Resolved(ty));
    }
}
