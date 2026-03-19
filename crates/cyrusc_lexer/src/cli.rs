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

use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::read_file;
use cyrusc_lexer::Lexer;
use cyrusc_source_loc::SourceMap;
use std::env;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let (file_content, file_name) = read_file(file_path.clone());

    let mut source_map = SourceMap::new();
    let file_id = source_map.add_file(file_name, file_content);
    let source_file = source_map.get_file(file_id).unwrap();
    let mut reporter = DiagReporter::new(&source_map);

    let mut lexer = Lexer::new(&mut reporter, &source_file);
    let tokens = lexer.tokenize();

    dbg!(tokens.clone());
}
