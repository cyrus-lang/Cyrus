// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#[derive(Debug, Clone)]
pub struct ABITypeLayout {
    pub size: u32,
    pub align: u32,
    pub field_offsets: Vec<ABIFieldOffsetInfo>,

    #[cfg(debug_assertions)]
    pub is_aggregate: bool,
}

#[derive(Debug, Clone)]
pub enum ABIFieldOffsetInfo {
    Normal {
        index: u32,
        offset: u32,
        original_index: usize,
    },
    Padding {
        index: u32,
        offset: u32,
        size: u32,
    },
}

impl ABITypeLayout {
    pub fn normal(size: u32, align: u32, field_offsets: Vec<ABIFieldOffsetInfo>) -> Self {
        Self {
            size,
            align,
            field_offsets,

            #[cfg(debug_assertions)]
            is_aggregate: false,
        }
    }

    pub fn aggregate(size: u32, align: u32, field_offsets: Vec<ABIFieldOffsetInfo>) -> Self {
        Self {
            size,
            align,
            field_offsets,

            #[cfg(debug_assertions)]
            is_aggregate: true,
        }
    }

    pub fn lookup_field_index(&self, original_index: usize) -> Option<u32> {
        self.field_offsets
            .iter()
            .find(|field_offset| match field_offset.original_index() {
                Some(i) => i == original_index,
                None => false,
            })
            .map(|field_offset| field_offset.index())
    }

    pub fn lookup_field_index_at_offset(&self, offset: u32) -> Option<usize> {
        let mut best: Option<(usize, u32)> = None;

        for entry in &self.field_offsets {
            if let ABIFieldOffsetInfo::Normal {
                offset: field_offset,
                index,
                ..
            } = entry
            {
                if offset >= *field_offset && offset < (*field_offset + self.size) {
                    match best {
                        Some((_, best_size)) if best_size >= self.size => {}
                        _ => best = Some((*index as usize, self.size)),
                    }
                }
            }
        }

        best.map(|(idx, _)| idx)
    }

    pub fn lookup_field_offset(&self, field_original_index: usize) -> u32 {
        for entry in &self.field_offsets {
            if let ABIFieldOffsetInfo::Normal {
                offset, original_index, ..
            } = entry
            {
                if *original_index == field_original_index {
                    return *offset;
                }
            }
        }

        panic!("field offset not found for index {}", field_original_index);
    }
}

impl ABIFieldOffsetInfo {
    pub fn normal(index: u32, offset: u32, original_index: usize) -> Self {
        ABIFieldOffsetInfo::Normal {
            index,
            offset,
            original_index,
        }
    }

    pub fn padding(index: u32, offset: u32, size: u32) -> Self {
        ABIFieldOffsetInfo::Padding { index, offset, size }
    }

    pub fn offset(&self) -> u32 {
        match self {
            ABIFieldOffsetInfo::Normal { offset, .. } => *offset,
            ABIFieldOffsetInfo::Padding { offset, .. } => *offset,
        }
    }

    pub fn index(&self) -> u32 {
        match self {
            ABIFieldOffsetInfo::Normal { index, .. } => *index,
            ABIFieldOffsetInfo::Padding { index, .. } => *index,
        }
    }

    pub fn is_padding(&self) -> bool {
        matches!(self, ABIFieldOffsetInfo::Padding { .. })
    }

    pub fn size(&self) -> Option<u32> {
        match self {
            ABIFieldOffsetInfo::Normal { .. } => None,
            ABIFieldOffsetInfo::Padding { size, .. } => Some(*size),
        }
    }

    pub fn original_index(&self) -> Option<usize> {
        match self {
            ABIFieldOffsetInfo::Normal { original_index, .. } => Some(*original_index),
            ABIFieldOffsetInfo::Padding { .. } => None,
        }
    }
}
