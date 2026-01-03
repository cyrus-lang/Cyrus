// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
use crate::Lexer;
use core::fmt;

impl fmt::Display for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        write!(f, "pos: {}, next_pos: {}, char: {}", self.pos, self.next_pos, self.ch)
    }
}
