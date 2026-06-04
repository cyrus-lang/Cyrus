// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::read_file;
use cyrusc_lexer::Lexer;
use cyrusc_source_loc::SourceMap;
use std::{env, sync::Arc};

// FIXME: Move to cyrusc_compiler/driver.rs
pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let (file_content, file_name) = read_file(file_path.clone());

    let source_map = Arc::new(SourceMap::new());
    let file_id = source_map.add_file(file_name, file_content);
    let source_file = {source_map.get_file(file_id).unwrap().clone()};
    let reporter = Arc::new(DiagReporter::new(source_map.clone()));

    let mut lexer = Lexer::new(&reporter, &source_file);
    let tokens = lexer.tokenize();

    dbg!(tokens.clone());
}
