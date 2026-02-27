use std::path::PathBuf;

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
