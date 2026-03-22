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

use std::{
    cell::{Cell, Ref, RefCell},
    collections::HashMap,
    path::Path,
};

use cyrusc_fs_utils::read_file;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileID(pub u32);

#[derive(Debug, Clone)]
pub struct SourceMap {
    files: RefCell<HashMap<FileID, SourceFile>>,
    next_id: Cell<u32>,
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
            files: RefCell::new(HashMap::new()),
            next_id: Cell::new(0),
        }
    }

    /// Adds a new source file to the map and returns its assigned FileID.
    pub fn add_file(&self, name: String, content: String) -> FileID {
        let id_val = self.next_id.get();
        self.next_id.set(id_val + 1);

        let id = FileID(id_val);
        let source_file = SourceFile::new(id, name, content);

        self.files.borrow_mut().insert(id, source_file);
        id
    }

    // Loads a file from disk and registers it in the SourceMap.
    pub fn add_file_by_loading<P: AsRef<Path>>(&self, path: P) -> FileID {
        let (content, name) = read_file(&path);
        self.add_file(name, content)
    }

    /// Retrieves a SourceFile by its FileID.
    pub fn get_file(&self, id: FileID) -> Option<Ref<'_, SourceFile>> {
        if !self.files.borrow().contains_key(&id) {
            return None;
        }

        Some(std::cell::Ref::map(self.files.borrow(), |m| m.get(&id).unwrap()))
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

    pub fn default(id: FileID) -> Self {
        Self {
            id,
            line: 0,
            start: 0,
            end: 0,
        }
    }
}
