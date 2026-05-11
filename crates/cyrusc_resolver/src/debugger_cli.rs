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
use cyrusc_fs_utils::get_directory_of_file;
use cyrusc_internal::{compiler_options::CompilerOptions, symbols::symbols::{SymbolEntry, SymbolEntryKind}};
use cyrusc_parser::SourceParser;
use cyrusc_resolver::{
    Resolver,
    fs_module_loader::{FsModuleLoader, FsModuleLoaderOptions},
    modules::VisitingModule,
};
use cyrusc_source_loc::SourceMap;
use cyrusc_typed_ast::{SymbolID, decls::table::DeclTablesRegistry};

use std::{
    env,
    fs::File,
    io::{BufWriter, Write},
    path::Path,
    process::exit,
    sync::Arc,
    vec,
};

pub fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        println!("Usage: resolver_debug <input.cyrus> <symbol_dump.txt>");
        exit(1);
    }

    let file_path = args[1].clone();
    let output_file = args[2].clone();

    let input_dir = get_directory_of_file(&file_path).expect("failed to resolve input directory");

    let source_map = Arc::new(SourceMap::new());
    let file_id = source_map.add_file_by_loading(file_path.clone());
    let entry_source_file = source_map.get_file(file_id).unwrap().clone();

    let reporter = Arc::new(DiagReporter::new(source_map.clone()));
    let source_parser = Arc::new(SourceParser::new(reporter.clone()));

    let opts = CompilerOptions::default();

    match source_parser.parse_program(&entry_source_file) {
        Ok(program) => {
            let mut current_dir = env::current_dir().unwrap();
            current_dir.push("./stdlib");

            let stdlib_path = current_dir.canonicalize().unwrap().to_str().unwrap().to_string();

            let module_loader_opts = FsModuleLoaderOptions {
                base_path: current_dir.to_str().unwrap().to_string(),
                stdlib_path: Some(stdlib_path.clone()),
                source_dirs: vec![input_dir.clone()],
            };

            let fs_module_loader = FsModuleLoader::new(source_map.clone(), source_parser, module_loader_opts);

            let decl_tables = Arc::new(DeclTablesRegistry::new());

            let mut resolver = Resolver::new(
                &opts,
                Box::new(fs_module_loader),
                source_map.clone(),
                reporter.clone(),
                decl_tables,
            );

            let module_symbol_id = resolver.create_entry_module_symbol_id(Path::new(&file_path), file_id);

            resolver
                .resolve_module(module_symbol_id, &program, &mut VisitingModule::new(), file_id, true)
                .unwrap();

            let file = File::create(output_file).expect("failed to create output file");
            let mut writer = BufWriter::new(file);

            dump_global_symbols(&resolver, &mut writer);
            dump_source_map(&source_map, &mut writer);

            if resolver.reporter.has_errors() {
                resolver.reporter.display();
                eprintln!("exit with error.");
                return;
            }
        }

        Err(()) => {
            eprintln!("exit with error.")
        }
    }
}

fn dump_source_map(source_map: &SourceMap, writer: &mut BufWriter<File>) {
    writeln!(writer, "\n=========== SOURCE MAP ===========\n").unwrap();

    for (file_id, source_file) in source_map.files() {
        writeln!(
            writer,
            "file_id={}  ->  {}",
            file_id.0,
            source_file.file_path.to_str().unwrap()
        )
        .unwrap();
    }
}

// Symbol Graph Debugger
fn dump_global_symbols(resolver: &Resolver, writer: &mut BufWriter<File>) {
    let inner = resolver.global_symbols.inner.read().unwrap();

    writeln!(writer, "=========== GLOBAL SYMBOL TABLE ===========\n").unwrap();

    dump_flat_table(&inner.entries, writer);

    writeln!(writer, "\n=========== SCOPE GRAPH ===========\n").unwrap();

    dump_scope_tree(resolver, &inner.entries, writer);
}

fn dump_flat_table(entries: &Vec<SymbolEntry>, writer: &mut BufWriter<File>) {
    for (id, symbol_entry) in entries.iter().enumerate() {
        let parent = match symbol_entry.parent_scope_id {
            Some(p) => p.to_string(),
            None => "ROOT".into(),
        };

        let kind = simplify_kind(&symbol_entry);

        writeln!(
            writer,
            "[{}] parent={} kind={} used={}",
            id, parent, kind, symbol_entry.used
        )
        .unwrap();

        if let Some(proxy) = proxy_target(&symbol_entry.kind) {
            writeln!(writer, "     -> proxy_target={}", proxy).unwrap();
        }
    }
}

fn dump_scope_tree(resolver: &Resolver, entries: &Vec<SymbolEntry>, writer: &mut BufWriter<File>) {
    let mut children: Vec<Vec<usize>> = vec![Vec::new(); entries.len()];

    for (i, symbol_entry) in entries.iter().enumerate() {
        if let Some(parent) = symbol_entry.parent_scope_id {
            if parent.0 < children.len().try_into().unwrap() {
                children[parent.0 as usize].push(i);
            }
        }
    }

    for (id, symbol_entry) in entries.iter().enumerate() {
        if symbol_entry.parent_scope_id.is_none() {
            print_node(resolver, id, entries, &children, writer, 0);
        }
    }
}

fn print_node(
    resolver: &Resolver,
    id: usize,
    entries: &Vec<SymbolEntry>,
    children: &Vec<Vec<usize>>,
    writer: &mut BufWriter<File>,
    depth: usize,
) {
    let symbol_entry = &entries[id];

    let indent = "  ".repeat(depth);

    let kind = simplify_kind(&symbol_entry);

    writeln!(writer, "{}[{}] {}", indent, id, kind).unwrap();

    if let Some(proxy) = proxy_target(&symbol_entry.kind) {
        writeln!(writer, "{}   -> {}", indent, proxy).unwrap();
    }

    for child in &children[id] {
        print_node(resolver, *child, entries, children, writer, depth + 1);
    }
}

fn simplify_kind(symbol_entry: &SymbolEntry) -> String {
    let kind_str = match &symbol_entry.kind {
        SymbolEntryKind::Unresolved => "Unresolved",

        SymbolEntryKind::Module(_) => "Module",
        SymbolEntryKind::Namespace(_) => "Namespace",

        SymbolEntryKind::Method(_) => "Method",
        SymbolEntryKind::Func(_) => "Func",

        SymbolEntryKind::Typedef(_) => "Typedef",

        SymbolEntryKind::Var(_) => "Var",
        SymbolEntryKind::GlobalVar(_) => "GlobalVar",

        SymbolEntryKind::Struct(_) => "Struct",
        SymbolEntryKind::Enum(_) => "Enum",
        SymbolEntryKind::Union(_) => "Union",
        SymbolEntryKind::Interface(_) => "Interface",

        SymbolEntryKind::ProxiedSymbol { .. } => "ProxiedSymbol",
        SymbolEntryKind::ProxiedModule { .. } => "ProxiedModule",
    };

    kind_str.to_string()
}

fn proxy_target(kind: &SymbolEntryKind) -> Option<SymbolID> {
    match kind {
        SymbolEntryKind::ProxiedModule { symbol_id } => Some(*symbol_id),
        SymbolEntryKind::ProxiedSymbol { symbol_id, .. } => Some(*symbol_id),
        _ => None,
    }
}
