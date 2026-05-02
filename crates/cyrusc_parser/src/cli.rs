use cyrusc_diagcentral::reporter::DiagReporter;
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

use cyrusc_fs_utils::read_file;
use cyrusc_parser::SourceParser;
use cyrusc_source_loc::SourceMap;
use std::{env, process::exit, sync::Arc};

// FIXME: Move to cyrusc_compiler/driver.rs
pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let (file_content, file_name) = read_file(file_path.clone());

    let source_map = Arc::new(SourceMap::new());
    let file_id = source_map.add_file(file_name, file_content);
    let source_file = {source_map.get_file(file_id).unwrap().clone()};

    let reporter = Arc::new(DiagReporter::new(source_map.clone()));

    let source_parser = SourceParser::new(reporter);

    match source_parser.parse_program(&source_file) {
        Ok(program_tree) => {
            dbg!(program_tree);
        }
        Err(()) => exit(1),
    }
}
