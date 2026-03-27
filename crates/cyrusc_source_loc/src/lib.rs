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
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{
        Arc, RwLock,
        atomic::{AtomicU32, Ordering},
    },
};

use cyrusc_fs_utils::read_file;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileID(pub u32);

#[derive(Debug)]
pub struct SourceMap {
    files: RwLock<HashMap<FileID, Arc<SourceFile>>>,
    next_id: AtomicU32,
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub file_id: FileID,    // The unique identifier for this file
    pub file_path: PathBuf, // The original filename (e.g., "main.cyrus")
    pub content: String,    // The full source code of the file
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub file_id: FileID,
    pub line: usize,
    pub column: usize,
    pub start: usize,
    pub end: usize,
}

impl SourceMap {
    /// Creates a new, empty SourceMap.
    pub fn new() -> Self {
        Self {
            files: RwLock::new(HashMap::new()),
            next_id: AtomicU32::new(0),
        }
    }

    /// Adds a new source file to the map and returns its assigned FileID.
    pub fn add_file<P: AsRef<Path>>(&self, file_path: P, content: String) -> FileID {
        let id_val = self.next_id.fetch_add(1, Ordering::Relaxed);
        let file_id = FileID(id_val);

        let source_file = Arc::new(SourceFile::new(file_id, file_path.as_ref().to_path_buf(), content));

        self.files.write().unwrap().insert(file_id, source_file);
        file_id
    }

    /// Loads a file from disk and registers it in the SourceMap.
    pub fn add_file_by_loading<P: AsRef<Path>>(&self, file_path: P) -> FileID {
        let (content, _) = read_file(&file_path);
        self.add_file(file_path, content)
    }

    /// Retrieves a SourceFile by its FileID.
    pub fn get_file(&self, file_id: FileID) -> Option<Arc<SourceFile>> {
        self.files.read().unwrap().get(&file_id).cloned()
    }
}

impl SourceFile {
    pub fn new(file_id: FileID, file_path: PathBuf, content: String) -> Self {
        Self {
            file_id,
            file_path,
            content,
        }
    }
}

impl Loc {
    pub fn new(file_id: FileID, line: usize, column: usize, start: usize, end: usize) -> Self {
        Self {
            file_id,
            line,
            column,
            start,
            end,
        }
    }

    pub fn default(file_id: FileID) -> Self {
        Self {
            file_id: file_id,
            line: 0,
            column: 0,
            start: 0,
            end: 0,
        }
    }
}
