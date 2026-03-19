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

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileID(pub u32);

#[derive(Debug, Clone)]
pub struct SourceMap {
    files: HashMap<FileID, SourceFile>,
    next_id: u32,
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub id: FileID,      // The unique identifier for this file
    pub name: String,    // The original filename (e.g., "main.cyrus")
    pub content: String, // The full source code of the file
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub id: FileID,
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl SourceMap {
    /// Creates a new, empty SourceMap.
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            next_id: 0,
        }
    }

    /// Adds a new source file to the map and returns its assigned FileID.
    pub fn add_file(&mut self, name: String, content: String) -> FileID {
        let id = FileID(self.next_id);
        let source_file = SourceFile::new(id, name, content);
        self.files.insert(id, source_file);
        self.next_id += 1;
        id
    }

    /// Retrieves a SourceFile by its FileID.
    pub fn get_file(&self, id: FileID) -> Option<&SourceFile> {
        self.files.get(&id)
    }
}

impl SourceFile {
    pub fn new(id: FileID, name: String, content: String) -> Self {
        Self { id, name, content }
    }
}

impl Loc {
    pub fn new(file_id: FileID, line: usize, start: usize, end: usize) -> Self {
        Self {
            id: file_id,
            line,
            start,
            end,
        }
    }
}
