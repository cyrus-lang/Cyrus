use crate::diagnostics::ResolverDiagKind;
use ast::{
    Identifier, Import, ModulePath, ModuleSegment, ProgramTree, format::module_segments_as_string, token::Location,
};
use diagcentral::{Diag, DiagLevel, display_single_diag};
use lexer::Lexer;
use parser::Parser;
use std::{
    env,
    path::{Path, PathBuf},
    rc::Rc,
};
use utils::fs::find_file_from_sources;

pub type ModuleAlias = String;
pub type ModuleFilePath = String;

#[derive(Debug)]
pub struct ModuleLoaderOptions {
    pub stdlib_path: Option<String>,
    pub source_dirs: Vec<String>,
}

#[derive(Debug)]
pub struct ModuleLoader {
    pub opts: ModuleLoaderOptions,
}

impl ModuleLoader {
    pub fn new(opts: ModuleLoaderOptions) -> Self {
        ModuleLoader { opts }
    }

    pub fn load_module(
        &self,
        import: Import,
        current_module_file_path: String,
    ) -> Vec<Result<(ModuleAlias, ModuleFilePath, Rc<ProgramTree>), ResolverDiagKind>> {
        let mut loaded_modules_list: Vec<Result<(ModuleAlias, ModuleFilePath, Rc<ProgramTree>), ResolverDiagKind>> =
            Vec::new();

        for sub_import in &import.paths {
            let module_file_path = match self.get_imported_module_path(
                sub_import.clone(),
                sub_import.segments.clone(),
                current_module_file_path.clone(),
                import.loc.clone(),
                import.span.end,
            ) {
                Ok(module_file_path) => module_file_path,
                Err(diag_kind) => {
                    loaded_modules_list.push(Err(diag_kind));
                    continue;
                }
            };

            let alias = sub_import
                .alias
                .clone()
                .unwrap_or({
                    // last segment of the import_path
                    sub_import.segments.last().unwrap().as_identifier().name
                });

            let file_content = std::fs::read_to_string(module_file_path.clone()).unwrap();
            let mut lexer = Lexer::new(file_content, module_file_path.clone());
            let mut parser = Parser::new(lexer.tokenize(), module_file_path.clone());
            match parser.parse() {
                Ok(node) => {
                    let program_tree = node.as_program();
                    let program_tree_rc = Rc::new(ProgramTree {
                        body: Rc::clone(&program_tree.body),
                    });

                    loaded_modules_list.push(Ok((alias, module_file_path.clone(), program_tree_rc)));
                }
                Err(errors) => {
                    parser.display_parser_errors(errors.clone());
                }
            }
        }

        loaded_modules_list
    }

    fn get_imported_module_path(
        &self,
        module_path: ModulePath,
        segments: Vec<ModuleSegment>,
        current_module_file_path: String,
        loc: Location,
        span_end: usize,
    ) -> Result<ModuleFilePath, ResolverDiagKind> {
        let mut sources = self.opts.source_dirs.clone();

        let mut segments = segments;
        if matches!(segments.first(), Some(ModuleSegment::SubModule(id)) if id.name == "std") {
            segments.remove(0);
            sources = vec![self.get_stdlib_modules_path()];
        }

        let get_module_alias = |mut module_path: ModulePath| match module_path.alias.clone() {
            Some(alias) => {
                module_path.segments = vec![ModuleSegment::SubModule(Identifier {
                    name: alias,
                    span: module_path.span.clone(),
                    loc: module_path.loc.clone(),
                })];
                module_path
            }
            None => {
                let module_path_last_segment = module_path.segments.last().unwrap().clone();
                module_path.segments = vec![module_path_last_segment];
                module_path
            }
        };

        // starting point
        let module_file_path = String::new();

        self.resolve_segments(
            module_path,
            &segments,
            sources,
            module_file_path,
            &get_module_alias,
            current_module_file_path,
            loc,
            span_end,
        )
    }

    fn resolve_segments(
        &self,
        module_path: ModulePath,
        segments: &[ModuleSegment],
        sources: Vec<String>,
        mut module_file_path: String,
        get_module_alias: &dyn Fn(ModulePath) -> ModulePath,
        current_module_file_path: String,
        loc: Location,
        span_end: usize,
    ) -> Result<ModuleFilePath, ResolverDiagKind> {
        let module_name = module_segments_as_string(segments.to_vec());

        for (idx, segment) in segments.iter().enumerate() {
            if let ModuleSegment::SubModule(identifier) = segment {
                let file_path = format!("{}{}.cyr", module_file_path, identifier.name);
                let dir_path = format!("{}{}/", module_file_path, identifier.name);

                // when inside a known directory, check directly; otherwise search in sources
                let file_exists = if Path::new(&file_path).exists() {
                    Some(PathBuf::from(&file_path))
                } else {
                    find_file_from_sources(file_path.clone(), sources.clone())
                };

                let dir_exists = if Path::new(&dir_path).exists() {
                    Some(PathBuf::from(&dir_path))
                } else {
                    find_file_from_sources(dir_path.clone(), sources.clone())
                };

                match (file_exists, dir_exists) {
                    (Some(..), Some(..)) => {
                        return Err(ResolverDiagKind::DuplicateModule {
                            module_name: identifier.name.clone(),
                        });
                    }
                    (Some(file_buf), None) => {
                        module_file_path = file_buf.to_str().unwrap().to_string();
                        if idx == segments.len() - 1 {
                            return Ok(module_file_path);
                        }
                    }
                    (None, Some(dir_buf)) => {
                        if idx == segments.len() - 1 {
                            // directory is the final segment → require index.cyr
                            let index_path = dir_buf.join("index.cyr");
                            if !index_path.exists() {
                                return Err(ResolverDiagKind::ModuleIndexNotFound {
                                    module_name: identifier.name.clone(),
                                });
                            }
                            module_file_path = index_path.to_str().unwrap().to_string();
                            return Ok(module_file_path);
                        } else {
                            // not the last segment → descend into directory
                            module_file_path = dir_buf.to_str().unwrap().to_string();
                            if !module_file_path.ends_with('/') {
                                module_file_path.push('/');
                            }
                        }
                    }
                    (None, None) => {
                        return Err(ResolverDiagKind::ModuleNotFound { module_name });
                    }
                }
            } else if let ModuleSegment::Single(_) = segment {
                // recurse for single segments
                return self.resolve_segments(
                    module_path.clone(),
                    &segments[..=idx],
                    sources.clone(),
                    module_file_path.clone(),
                    get_module_alias,
                    current_module_file_path.clone(),
                    loc.clone(),
                    span_end,
                );
            }
        }

        Ok(module_file_path)
    }
    fn get_stdlib_modules_path(&self) -> String {
        match self.opts.stdlib_path.clone() {
            Some(stdlib_path) => stdlib_path,
            None => match env::var("CYRUS_STDLIB_PATH") {
                Ok(stdlib_path) => stdlib_path,
                Err(_) => {
                    display_single_diag!(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::StdlibNotFound,
                        location: None,
                        hint: None
                    });
                }
            },
        }
    }
}
