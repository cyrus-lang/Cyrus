use std::path::PathBuf;

// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#[derive(Debug, Clone)]
pub struct ObjectFileInfo {
    pub path: PathBuf,
    pub size: usize,
}

impl ObjectFileInfo {
    pub fn new(path: PathBuf, size: usize) -> Self {
        Self { path, size }
    }
}

#[inline]
pub fn collect_objects_file_names(objs: Vec<ObjectFileInfo>) -> Vec<String> {
    objs.iter().map(|obj| obj.path.to_string_lossy().to_string()).collect()
}
