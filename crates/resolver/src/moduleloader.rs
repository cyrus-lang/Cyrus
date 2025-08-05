use crate::diagnostics::ResolverDiagKind;
use ast::{
    Identifier, Import, ModulePath, ModuleSegment, ProgramTree, format::module_segments_as_string, token::Location,
};
use diagcentral::{Diag, DiagLevel, DiagLoc, display_single_diag};
use lexer::Lexer;
use parser::Parser;
use std::{env, path::Path, rc::Rc};
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
                .alias.clone()
                .unwrap_or(module_segments_as_string(sub_import.segments.clone()));

            let file_content = std::fs::read_to_string(module_file_path.clone()).unwrap();
            let mut lexer = Lexer::new(file_content, module_file_path.clone());
            let mut parser = Parser::new(&mut lexer);
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

        let module_file_path = String::new();

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
        module_file_path: String,
        get_module_alias: &dyn Fn(ModulePath) -> ModulePath,
        current_module_file_path: String,
        loc: Location,
        span_end: usize,
    ) -> Result<ModuleFilePath, ResolverDiagKind> {
        let module_name = module_segments_as_string(segments.to_vec());
        let mut module_file_path = module_file_path;

        for idx in 0..segments.len() {
            match &segments[idx] {
                ModuleSegment::SubModule(identifier) => {
                    let dir_path = format!("{}{}", module_file_path, identifier.name);
                    let file_path = format!("{}{}.cyr", module_file_path, identifier.name);

                    if let Some(file_path_buf) = find_file_from_sources(file_path.clone(), sources.clone()) {
                        if idx == segments.len() - 1 {
                            return Ok(file_path_buf.to_str().unwrap().to_string());
                        } else if (segments.len() - 1) - idx == 1 {
                            match &segments[idx + 1] {
                                ModuleSegment::Single(..) => {
                                    let base_path = self.resolve_segments(
                                        module_path.clone(),
                                        &segments[..idx + 1],
                                        sources.clone(),
                                        module_file_path.clone(),
                                        get_module_alias,
                                        current_module_file_path.clone(),
                                        loc.clone(),
                                        span_end,
                                    )?;

                                    if !Path::new(&base_path).is_file() {
                                        display_single_diag!(Diag {
                                            level: DiagLevel::Error,
                                            kind: ResolverDiagKind::CannotImportDirectoryAsModule,
                                            location: Some(DiagLoc {
                                                file: current_module_file_path,
                                                line: loc.line,
                                                column: loc.column,
                                                length: span_end,
                                            }),
                                            hint: None
                                        });
                                    }

                                    return Ok(base_path);
                                }
                                _ => {}
                            }
                        }
                    } else {
                        if let Some(found_path) = find_file_from_sources(dir_path, sources.clone()) {
                            module_file_path.push_str(found_path.to_str().unwrap());
                            module_file_path.push('/');
                        } else {
                            display_single_diag!(Diag {
                                level: DiagLevel::Error,
                                kind: ResolverDiagKind::ModuleNotFound { module_name },
                                location: Some(DiagLoc {
                                    file: current_module_file_path,
                                    line: loc.line,
                                    column: loc.column,
                                    length: span_end,
                                }),
                                hint: None
                            });
                        }
                    }
                }
                ModuleSegment::Single(_) => {
                    return self.resolve_segments(
                        module_path.clone(),
                        &segments[..idx + 1],
                        sources.clone(),
                        module_file_path.clone(),
                        get_module_alias,
                        current_module_file_path,
                        loc.clone(),
                        span_end,
                    );
                }
            }
        }

        let path = Path::new(&module_file_path);
        if !path.exists() {
            display_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: ResolverDiagKind::ModuleImportNotFound { module_name },
                location: Some(DiagLoc {
                    file: current_module_file_path,
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
                hint: None
            });
        }

        if path.is_dir() {
            display_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: ResolverDiagKind::CannotImportDirectoryAsModule,
                location: Some(DiagLoc {
                    file: current_module_file_path,
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
                hint: None
            });
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

    // Local means it only make sense in this module.
    // It may be aliased or not.
    pub(crate) fn get_local_module_name(&self, module_path: ModulePath, current_module_file_path: String) -> String {
        let last_segment = module_path.segments.last().unwrap();

        match last_segment {
            ModuleSegment::SubModule(identifier) => module_path.alias.unwrap_or(identifier.name.clone()),
            ModuleSegment::Single(_) => {
                if module_path.alias.is_some() {
                    display_single_diag!(Diag {
                        level: DiagLevel::Error,
                        kind: ResolverDiagKind::InvalidRenameWhenImportingModule,
                        location: Some(DiagLoc {
                            file: current_module_file_path,
                            line: module_path.loc.line,
                            column: module_path.loc.column,
                            length: module_path.span.end,
                        }),
                        hint: None
                    });
                } else {
                    match module_path.segments.iter().nth_back(1).unwrap() {
                        ModuleSegment::SubModule(identifier) => module_path.alias.unwrap_or(identifier.name.clone()),
                        ModuleSegment::Single(_) => unreachable!(),
                    }
                }
            }
        }
    }
}
