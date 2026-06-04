use cyrusc_diagcentral::reporter::DiagReporter;
// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

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
