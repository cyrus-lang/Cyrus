use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::get_directory_of_file;
use cyrusc_internal::symbols::{
    symbols::{SymbolEntry, SymbolEntryKind},
    table::Query,
};
use cyrusc_parser::SourceParser;
use cyrusc_resolver::{
    GlobalSymbolRegistry, Resolver,
    fs_module_loader::{FsModuleLoader, FsModuleLoaderOptions},
    modules::VisitingModule,
};
use cyrusc_source_loc::SourceMap;
use cyrusc_typed_ast::{
    SymbolID,
    generics::{mapping_ctx_arena::GenericMappingCtxArenaImpl, monomorph::MonomorphRegistry},
};

use std::{
    env,
    fs::File,
    io::{BufWriter, Write},
    path::Path,
    process::exit,
    sync::{Arc, Mutex},
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

            let mapping_ctx_arena = Arc::new(Mutex::new(GenericMappingCtxArenaImpl::new()));

            let monomorph_registry = Arc::new(Mutex::new(MonomorphRegistry::new()));

            let mut resolver = Resolver::new(
                Box::new(fs_module_loader),
                source_map.clone(),
                reporter.clone(),
                monomorph_registry,
                mapping_ctx_arena,
            );

            let module_symbol_id = resolver.create_entry_module_symbol_id(Path::new(&file_path), file_id);

            let _typed_program_tree = resolver
                .resolve_module(module_symbol_id, &program, &mut VisitingModule::new(), file_id, true)
                .unwrap();

            if resolver.reporter.has_errors() {
                resolver.reporter.display();
                exit(1);
            }

            dump_global_symbols(&resolver, &output_file);
        }

        Err(()) => exit(1),
    }
}

// Symbol Graph Debugger
fn dump_global_symbols(resolver: &Resolver, output_path: &str) {
    let file = File::create(output_path).expect("failed to create output file");
    let mut writer = BufWriter::new(file);

    let inner = resolver.global_symbols.inner.read().unwrap();

    writeln!(writer, "=========== GLOBAL SYMBOL TABLE ===========\n").unwrap();

    dump_flat_table(resolver, &inner.entries, &mut writer);

    writeln!(writer, "\n=========== SCOPE GRAPH ===========\n").unwrap();

    dump_scope_tree(resolver, &inner.entries, &mut writer);
}

fn dump_flat_table(resolver: &Resolver, entries: &Vec<SymbolEntry>, writer: &mut BufWriter<File>) {
    for (id, entry) in entries.iter().enumerate() {
        let parent = match entry.parent_scope_id {
            Some(p) => p.to_string(),
            None => "ROOT".into(),
        };

        let kind = simplify_kind(resolver, &entry);

        writeln!(writer, "[{}] parent={} kind={} used={}", id, parent, kind, entry.used).unwrap();

        if let Some(proxy) = proxy_target(&entry.kind) {
            writeln!(writer, "     -> proxy_target={}", proxy).unwrap();
        }
    }
}

fn dump_scope_tree(resolver: &Resolver, entries: &Vec<SymbolEntry>, writer: &mut BufWriter<File>) {
    let mut children: Vec<Vec<usize>> = vec![Vec::new(); entries.len()];

    for (i, entry) in entries.iter().enumerate() {
        if let Some(parent) = entry.parent_scope_id {
            if parent.0 < children.len().try_into().unwrap() {
                children[parent.0 as usize].push(i);
            }
        }
    }

    for (id, entry) in entries.iter().enumerate() {
        if entry.parent_scope_id.is_none() {
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
    let entry = &entries[id];

    let indent = "  ".repeat(depth);

    let kind = simplify_kind(resolver, &entry);

    writeln!(writer, "{}[{}] {}", indent, id, kind).unwrap();

    if let Some(proxy) = proxy_target(&entry.kind) {
        writeln!(writer, "{}   -> {}", indent, proxy).unwrap();
    }

    for child in &children[id] {
        print_node(resolver, *child, entries, children, writer, depth + 1);
    }
}

fn simplify_kind(resolver: &Resolver, entry: &SymbolEntry) -> String {
    let kind_str = match &entry.kind {
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

    let name = entry.decl_name();

    format!("{}({})", kind_str, name)
}

fn proxy_target(kind: &SymbolEntryKind) -> Option<SymbolID> {
    match kind {
        SymbolEntryKind::ProxiedModule { symbol_id } => Some(*symbol_id),
        SymbolEntryKind::ProxiedSymbol { symbol_id, .. } => Some(*symbol_id),
        _ => None,
    }
}
