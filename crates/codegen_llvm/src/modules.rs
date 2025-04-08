use crate::{
    CodeGenLLVM,
    build::BuildManifest,
    diag::*,
    funcs::{FuncMetadata, FuncTable},
    structs::StructTable,
};
use ast::{
    ast::{Field, FuncDecl, Identifier, Import, ModulePath, VisType},
    token::Location,
};
use inkwell::module::Module;
use std::{cell::RefCell, collections::HashMap, process::exit, rc::Rc};
use utils::fs::{find_file_from_sources, list_files};

#[derive(Debug, Clone)]
pub struct ExportedFuncMetadata {
    pub func_decl: FuncDecl,
}

#[derive(Debug, Clone)]
pub struct ExportedStructMetadata {
    pub name: String,
    pub vis_type: VisType,
    pub inherits: Vec<Identifier>,
    pub fields: Vec<Field>,
    pub methods: Vec<FuncDecl>,
}

#[derive(Debug, Clone)]
pub struct ModuleMetadata<'ctx> {
    pub import_alias: String,
    pub module: Rc<RefCell<Module<'ctx>>>,
    pub imported_funcs: HashMap<String, ExportedFuncMetadata>,
    pub imported_structs: HashMap<String, ExportedStructMetadata>,
}

#[derive(Debug, Clone)]
pub enum ImportSingle {
    Object(String),
    Wildcard,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    fn build_import_alias(&self, module_paths: Vec<ModulePath>) -> String {
        module_paths
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(".")
    }

    pub(crate) fn build_import(&mut self, import: Import) {
        let sources = &self.opts.sources_dir;
        let import_alias = self.build_import_alias(import.module_paths.clone());
        let mut module_paths = import.module_paths.clone();
        let mut import_single: Option<ImportSingle> = None;

        let mut begging_path = {
            if let ModulePath::SubModule(identifier) = module_paths.clone().first().unwrap() {
                module_paths.remove(0);
                let file_name = identifier.name.clone();
                match find_file_from_sources(file_name, sources.clone()) {
                    Some(path) => path,
                    None => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::ModuleNotFound(import_alias.clone()),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: import.loc.line,
                                column: import.loc.column,
                                length: import.span.end,
                            }),
                        });
                        exit(1);
                    }
                }
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::InvalidWildcard,
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: import.loc.line,
                        column: import.loc.column,
                        length: import.span.end,
                    }),
                });
                exit(1);
            }
        };

        for (idx, module_path) in module_paths.iter().enumerate() {
            match module_path {
                ModulePath::Wildcard => {
                    if idx == module_paths.len() - 1 {
                        import_single = Some(ImportSingle::Wildcard)
                    } else {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::InvalidWildcard,
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: import.loc.line,
                                column: import.loc.column,
                                length: import.span.end,
                            }),
                        });
                        exit(1);
                    }
                }
                ModulePath::SubModule(identifier) => {
                    // check current identifier that, it's a sub_module or a thing that must be imported from latest module
                    let temp_path = format!("{}/{}", begging_path.to_str().unwrap(), identifier.name.clone());
                    match find_file_from_sources(temp_path.clone(), sources.clone()) {
                        Some(new_begging_path) => {
                            begging_path = new_begging_path;
                        }
                        None => {
                            if let Some(new_begging_path) =
                                find_file_from_sources(format!("{}.cyr", temp_path), sources.clone())
                            {
                                begging_path = new_begging_path;
                            } else {
                                if idx != module_paths.len() - 1 {
                                    display_single_diag(Diag {
                                        level: DiagLevel::Error,
                                        kind: DiagKind::ModuleNotFound(import_alias.clone()),
                                        location: Some(DiagLoc {
                                            file: self.file_path.clone(),
                                            line: import.loc.line,
                                            column: import.loc.column,
                                            length: import.span.end,
                                        }),
                                    });
                                    exit(1);
                                } else {
                                    import_single = Some(ImportSingle::Object(identifier.name.clone()));
                                }
                            }
                        }
                    }
                }
            }
        }

        // import directory is not allowed
        // only import module and wildcard is allowed
        if begging_path.is_dir() && import_single.is_none() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Import module '{}' as directory is not allowed.",
                    import_alias.clone()
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: import.loc.line,
                    column: import.loc.column,
                    length: import.span.end,
                }),
            });
            exit(1);
        }

        // here we have two possibilities!
        // if begging_path is a file, we import it.
        // and if it's not, so it's a something that must be imported from latest module
        if let Some(import_single) = import_single {
            match import_single {
                ImportSingle::Object(object_name) => {
                    match find_file_from_sources(
                        format!("{}/{}.cyr", begging_path.to_str().unwrap(), object_name),
                        sources.clone(),
                    ) {
                        Some(file_path) => {
                            self.build_imported_module(file_path.to_str().unwrap().to_string(), import_alias.clone());
                        }
                        None => {
                            if begging_path.is_file() {
                                self.build_import_single(
                                    begging_path.to_str().unwrap().to_string(),
                                    import_alias.clone(),
                                    object_name,
                                    import.loc.clone(),
                                    import.span.end,
                                );
                            } else {
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::ModuleNotFound(import_alias.clone()),
                                    location: Some(DiagLoc {
                                        file: self.file_path.clone(),
                                        line: import.loc.line,
                                        column: import.loc.column,
                                        length: import.span.end,
                                    }),
                                });
                                exit(1);
                            }
                        }
                    }
                }
                ImportSingle::Wildcard => {
                    if begging_path.is_dir() {
                        // wildcard for files
                        let directory_path = begging_path.to_str().unwrap();
                        let file_paths = list_files(directory_path, "cyr");
                        for file_path in file_paths {
                            self.build_imported_module(
                                format!("{}/{}", directory_path, file_path),
                                import_alias.clone(),
                            );
                        }
                    } else {
                        self.build_wildcard_imported_module(
                            begging_path.to_str().unwrap().to_string(),
                            import_alias.clone(),
                        );
                    }
                }
            }
        } else {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::ModuleNotFound(import_alias.clone()),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: import.loc.line,
                    column: import.loc.column,
                    length: import.span.end,
                }),
            });
            exit(1);
        }
    }

    fn build_import_module_single(
        &mut self,
        module_metadata: ModuleMetadata<'ctx>,
        object_name: String,
        import_alias: String,
        loc: Location,
        span_end: usize,
    ) {
        match module_metadata.imported_funcs.get(&object_name.clone()) {
            Some(v) => {
                let func_ptr = self.build_func_decl(v.func_decl.clone());
                self.func_table.insert(
                    v.func_decl.name.clone(),
                    FuncMetadata {
                        ptr: func_ptr,
                        func_decl: v.func_decl.clone(),
                    },
                );
            }
            None => match module_metadata.imported_structs.get(&object_name.clone()) {
                Some(_) => {
                    todo!("Import struct is not implemented yet.");
                }
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::ModuleNotFound(import_alias.clone()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            },
        }
    }
    fn build_import_single(
        &mut self,
        file_path: String,
        import_alias: String,
        object_name: String,
        loc: Location,
        span_end: usize,
    ) {
        // Import with optional alias
        let module_metadata = self.build_imported_module(file_path, import_alias.clone());
        self.build_import_module_single(module_metadata, object_name, import_alias, loc, span_end);
    }

    fn build_wildcard_imported_module(&mut self, file_path: String, import_alias: String) {
        // Import with optional alias
        todo!();
    }

    fn build_imported_module(&mut self, file_path: String, import_alias: String) -> ModuleMetadata<'ctx> {
        // Import things with alias

        // TODO Assure module names to be unique by their absolute path
        let module_name = file_path.clone();
        let sub_module = Rc::new(RefCell::new(self.context.create_module(&module_name)));
        let sub_builder = self.context.create_builder();
        let target_machine = CodeGenLLVM::target_machine(Rc::clone(&sub_module));
        let program = parser::parse_program(file_path.clone()).0;

        let mut sub_codegen = CodeGenLLVM {
            program,
            opts: self.opts.clone(),
            context: self.context,
            module: Rc::clone(&sub_module),
            builder: sub_builder,
            target_machine,
            build_manifest: BuildManifest::default(),
            file_path: file_path.clone(),
            reporter: self.reporter.clone(),
            entry_point: None,
            is_entry_point: false,
            func_table: HashMap::new(),
            struct_table: HashMap::new(),
            internal_funcs_table: self.internal_funcs_table.clone(),
            compiler_invoked_single: self.compiler_invoked_single,
            current_func_ref: None,
            current_block_ref: None,
            terminated_blocks: Vec::new(),
            string_type: self.string_type.clone(),
            loaded_modules: HashMap::new(),
        };

        sub_codegen.compile();
        self.build_manifest = sub_codegen.build_manifest.clone();

        let module_metadata = ModuleMetadata {
            module: Rc::clone(&sub_module),
            imported_funcs: self.collect_imported_funcs(sub_codegen.func_table),
            imported_structs: self.collect_imported_structs(sub_codegen.struct_table),
            import_alias,
        };

        self.loaded_modules.insert(module_name, module_metadata.clone());

        module_metadata
    }

    fn collect_imported_funcs(&self, func_table: FuncTable) -> HashMap<String, ExportedFuncMetadata> {
        let mut imported_funcs: HashMap<String, ExportedFuncMetadata> = HashMap::new();

        for (_, (func_name, metadata)) in func_table.iter().enumerate() {
            // only pub funcs are exported imported from sub_module
            if metadata.func_decl.vis_type == VisType::Pub {
                imported_funcs.insert(
                    func_name.to_string(),
                    ExportedFuncMetadata {
                        func_decl: metadata.func_decl.clone(),
                    },
                );

                // ANCHOR
                // self.build_func_decl(metadata.)
            }
        }

        imported_funcs
    }

    fn collect_imported_structs(&self, struct_table: StructTable) -> HashMap<String, ExportedStructMetadata> {
        let mut imported_structs: HashMap<String, ExportedStructMetadata> = HashMap::new();

        for (_, (func_name, metadata)) in struct_table.iter().enumerate() {
            // only pub structs are exported imported from sub_module
            todo!();
        }

        imported_structs
    }
}
