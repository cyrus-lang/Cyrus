// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use std::fmt;

use crate::token::Location;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SourceLoc {
    pub line: usize,
    pub column: usize,
    pub file_path: String,
}

impl SourceLoc {
    pub fn from_loc(loc: Location, file_path: String) -> Self {
        SourceLoc {
            line: loc.line,
            column: loc.column,
            file_path,
        }
    }
}

impl Default for SourceLoc {
    fn default() -> Self {
        SourceLoc {
            line: 0,
            column: 0,
            file_path: "<unknown>".to_string(),
        }
    }
}

impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file_path, self.line, self.column)
    }
}
