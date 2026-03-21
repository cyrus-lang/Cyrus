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

use cyrusc_ast::{Import, ModulePath, ModuleSegmentSingle, ProgramTree};
use std::{
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct LoadedModule {
    pub alias: ModuleAlias,
    pub path: ModulePath,
    pub file_path: PathBuf,
    pub program: Rc<ProgramTree>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleAlias {
    Group(String),
    Single(Vec<ModuleSegmentSingle>),
}

pub trait ModuleLoader {
    fn load_module(&mut self, import: &Import) -> Vec<Result<LoadedModule, ()>>;

    /// Forms a stable module name from a filesystem path.
    /// Strips extensions, normalizes separators, and prefixes stdlib modules.
    fn module_name_from_file_path(&mut self, path: &Path) -> String;
}

impl Hash for ModuleAlias {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ModuleAlias::Group(name) => {
                0u8.hash(state);
                name.hash(state);
            }
            ModuleAlias::Single(singles) => {
                1u8.hash(state);
                singles.hash(state);
            }
        }
    }
}
