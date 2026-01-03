// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#[derive(Debug, Clone)]
pub struct ObjectFileInfo {
    pub path: String,
    pub size: usize,
}

impl ObjectFileInfo {
    pub fn new(path: String, size: usize) -> Self {
        Self { path, size }
    }
}

pub fn get_objects_file_names(objs: Vec<ObjectFileInfo>) -> Vec<String> {
    objs.iter().map(|obj| obj.path.clone()).collect()
}
