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

use crate::abi::types::ABIType;

#[derive(Debug, Clone)]
pub struct ABIFunctionInfo {
    pub params_types: Vec<ABIType>,
    pub params_infos: Vec<ABIArgInfo>,
    pub ret_info: ABIRetInfo,
}

#[derive(Debug, Clone)]
pub struct ABIRetInfo {
    pub ty: ABIType,
    pub kind: ABIRetInfoKind,
}

#[derive(Debug, Clone)]
pub enum ABIRetInfoKind {
    Direct { coerce_to: Option<ABIType> },
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
    pub fn expand(kind: ExpandKind) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Expand { kind },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn direct() -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Direct { coerce_to: None },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn direct_coerce(ty: ABIType) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::DirectCoerce { ty },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn direct_pair(lo: ABIType, hi: ABIType) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::DirectPair { lo, hi },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn indirect(ty: ABIType, alignment: u32) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Indirect { ty, align: alignment },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn extend(signed: bool) -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Extend { signed },
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn ignore() -> Self {
        Self {
            param_index_start: 0,
            param_index_end: 0,
            kind: ABIArgKind::Ignore,
            attrs: ABIArgAttrs::default(),
        }
    }

    pub fn with_attrs(mut self, attrs: ABIArgAttrs) -> Self {
        self.attrs = attrs;
        self
    }

    pub fn with_indices(mut self, start: u16, end: u16) -> Self {
        self.param_index_start = start;
        self.param_index_end = end;
        self
    }

    pub fn is_direct(&self) -> bool {
        matches!(
            self.kind,
            ABIArgKind::Direct { .. } | ABIArgKind::DirectCoerce { .. } | ABIArgKind::DirectPair { .. }
        )
    }

    pub fn is_indirect(&self) -> bool {
        matches!(self.kind, ABIArgKind::Indirect { .. })
    }

    pub fn is_indirect_by_val(&self) -> bool {
        self.is_indirect() && self.attrs.by_val
    }

    pub fn is_ignore(&self) -> bool {
        matches!(self.kind, ABIArgKind::Ignore)
    }
}

impl ABIRetInfoKind {
    pub fn is_indirect(&self) -> bool {
        match self {
            ABIRetInfoKind::Indirect { .. } => true,
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
