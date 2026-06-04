// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{abi::types::ABIType, cir::types::CIRType};

#[derive(Debug, Clone)]
pub struct ABIFunctionInfo {
    pub params_types: Vec<ABIType>,
    pub params_infos: Vec<ABIArgInfo>,
    pub ret_info: ABIRetInfo,
}

#[derive(Debug, Clone)]
pub struct ABIRetInfo {
    pub abi_type: ABIType,
    pub kind: ABIRetInfoKind,
    pub cir_ret_type: Box<CIRType>,
}

#[derive(Debug, Clone)]
pub enum ABIRetInfoKind {
    Direct { coerce_to: Option<ABIType> },
    DirectPair { lo: ABIType, hi: ABIType },
    Indirect { sret: bool },
    Ignore,
}

#[derive(Debug, Clone)]
pub struct ABIArgInfo {
    /// Parameter index range (for LLVM function arguments)
    pub param_index_start: u16,
    pub param_index_end: u16,

    pub kind: ABIArgKind,
    pub attrs: ABIArgAttrs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ABIArgAttrs {
    /// Passed in register
    pub by_reg: bool,
    /// Zero extend (for bool/small ints)
    pub zero_ext: bool,
    /// Sign extend
    pub sign_ext: bool,
    /// Realign argument
    pub realign: bool,
    /// Pass by value (indirect)
    pub by_val: bool,
}

#[derive(Debug, Clone)]
pub enum ABIArgKind {
    /// Direct register passing (maybe coerced)
    Direct { coerce_to: Option<ABIType> },

    /// Passed in a pair of registers (lo/hi)
    DirectPair { lo: ABIType, hi: ABIType },

    /// Coerced to a different type and passed directly
    DirectCoerce { ty: ABIType },

    /// Expanded into multiple arguments
    Expand {
        /// How to expand
        kind: ExpandKind,
    },

    /// Integer extension
    Extend { signed: bool },

    /// Indirect passing (by pointer)
    Indirect {
        /// Required alignment
        align: u32,
        /// Type to pass indirectly
        ty: ABIType,
    },

    /// Ignored argument
    Ignore,
}

#[derive(Debug, Clone)]
pub enum ExpandKind {
    /// Simple expansion (default)
    Simple,

    /// Coerced expansion with offset info
    Coerced {
        offset_hi: u8,
        packed: bool,
        lo: ABIType,
        hi: ABIType,
    },

    /// Struct expansion with field count
    Struct { field_count: u8 },
}

impl ABIArgInfo {
    #[inline]
    pub fn expand(kind: ExpandKind) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Expand { kind },
            attrs: ABIArgAttrs::default(),
        }
    }

    #[inline]
    pub fn direct() -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Direct { coerce_to: None },
            attrs: ABIArgAttrs::default(),
        }
    }

    #[inline]
    pub fn direct_coerce(ty: ABIType) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::DirectCoerce { ty },
            attrs: ABIArgAttrs::default(),
        }
    }

    #[inline]
    pub fn direct_pair(lo: ABIType, hi: ABIType) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::DirectPair { lo, hi },
            attrs: ABIArgAttrs::default(),
        }
    }

    #[inline]
    pub fn indirect(ty: ABIType, alignment: u32) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Indirect { ty, align: alignment },
            attrs: ABIArgAttrs::default(),
        }
    }

    #[inline]
    pub fn extend(signed: bool) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Extend { signed },
            attrs: ABIArgAttrs::default(),
        }
    }

    #[inline]
    pub fn ignore() -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Ignore,
            attrs: ABIArgAttrs::default(),
        }
    }

    #[inline]
    pub fn with_attrs(mut self, attrs: ABIArgAttrs) -> Self {
        self.attrs = attrs;
        self
    }

    #[inline]
    pub fn with_indices(mut self, start: u16, end: u16) -> Self {
        self.param_index_start = start;
        self.param_index_end = end;
        self
    }

    #[inline]
    pub fn is_direct(&self) -> bool {
        matches!(
            self.kind,
            ABIArgKind::Direct { .. } | ABIArgKind::DirectCoerce { .. } | ABIArgKind::DirectPair { .. }
        )
    }

    #[inline]
    pub fn is_indirect(&self) -> bool {
        matches!(self.kind, ABIArgKind::Indirect { .. })
    }

    #[inline]
    pub fn is_indirect_by_val(&self) -> bool {
        self.is_indirect() && self.attrs.by_val
    }

    #[inline]
    pub fn is_ignore(&self) -> bool {
        matches!(self.kind, ABIArgKind::Ignore)
    }
}

impl ABIRetInfoKind {
    #[inline]
    pub fn is_ignore(&self) -> bool {
        match self {
            ABIRetInfoKind::Ignore => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_indirect(&self) -> bool {
        match self {
            ABIRetInfoKind::Indirect { .. } => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_indirect_sret(&self) -> bool {
        match self {
            ABIRetInfoKind::Indirect { sret } => *sret,
            _ => false,
        }
    }
}

impl Default for ABIArgAttrs {
    fn default() -> Self {
        Self {
            by_reg: false,
            zero_ext: false,
            sign_ext: false,
            realign: false,
            by_val: false,
        }
    }
}
