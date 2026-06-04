// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language
use crate::Lexer;
use core::fmt;

impl<'diag, 'source_file> fmt::Display for Lexer<'diag, 'source_file> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        write!(f, "pos: {}, next_pos: {}, char: {}", self.pos, self.next_pos, self.ch)
    }
}
