/* 
 * Copyright (c) 2026 The Cyrus Team
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
 */use std::fmt;

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
