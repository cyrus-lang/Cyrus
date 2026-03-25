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

use cyrusc_source_loc::{Loc, SourceMap};

pub(crate) fn format_missing_fields(list: &Vec<String>) -> String {
    list.iter()
        .map(|str| format!("'{str}'"))
        .collect::<Vec<String>>()
        .join(", ")
}

pub(crate) fn format_loc(source_map: &SourceMap, loc: Loc) -> String {
    let source_file = { source_map.get_file(loc.file_id).unwrap().clone() };
    format!("{}:{}:{}", source_file.name, loc.line, loc.column)
}
