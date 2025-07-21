use crate::{
    BlockRegistry, CodeGenLLVM,
    build::BuildManifest,
    diag::*,
    funcs::{FuncMetadata, FuncTable},
    structs::{StructMetadata, StructTable},
    types::{TypedefMetadata, TypedefTable},
    variables::{GlobalVariableMetadata, GlobalVariablesTable},
};
use ast::{
    ast::{Import, ModulePath, ModuleSegment, ModuleSegmentSingle},
    format::module_segments_as_string,
    token::Location,
};
use inkwell::values::{FunctionValue, StructValue};
use rand::Rng;
use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    env,
    path::Path,
    process::exit,
    rc::Rc,
};
use utils::fs::{find_file_from_sources, relative_to_absolute};

pub type ModuleID = u64;
pub type LocalIRValueID = u64;
pub type LocalIRValueRegistryRef<'a> = Rc<RefCell<LocalIRValueRegistry<'a>>>;
pub type LocalIRValueRegistry<'a> = HashMap<LocalIRValueID, LocalIRValue<'a>>;
pub type ModuleMetadataRegistryRef<'a> = Rc<RefCell<Vec<ModuleMetadata<'a>>>>;

#[derive(Debug, Clone)]
pub enum LocalIRValue<'a> {
    Func(FunctionValue<'a>),
    Struct(StructValue<'a>),
}

#[derive(Debug, Clone)]
pub struct ModuleMetadata<'a> {
    pub module_id: ModuleID,
    pub module_file_path: String,
    pub func_table: FuncTable<'a>,
    pub struct_table: StructTable<'a>,
    pub global_variables_table: GlobalVariablesTable<'a>,
    pub typedef_table: TypedefTable<'a>,
}

#[derive(Clone)]
pub(crate) struct ImportedModules {
    // reference to module metadata registry
    pub module_id: u64,
    pub module_path: ModulePath,
}

#[derive(Clone)]
pub(crate) struct ImportedModulePath {
    file_path: String,
    module_path: ModulePath,
    singles: Option<Vec<ModuleSegmentSingle>>,
    loc: Location,
    span_end: usize,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    fn error_if_module_already_loaded(&self, module_path: ModulePath) {
        if self
            .imported_modules
            .iter()
            .find(|i| i.module_path == module_path)
            .is_some()
        {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Cannot import module '{}' twice.",
                    module_segments_as_string(module_path.segments)
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: module_path.loc.line,
                    column: module_path.loc.column,
                    length: module_path.span.end,
                }),
            });
            exit(1);
        }
    }

    pub(crate) fn find_imported_module(&self, module_id: u64) -> Option<ImportedModules> {
        self.imported_modules.iter().find(|m| m.module_id == module_id).cloned()
    }

    fn build_stdlib_modules_path(&self) -> String {
        match self.opts.stdlib_path.clone() {
            Some(stdlib_path) => stdlib_path,
            None => match env::var("CYRUS_STDLIB_PATH") {
                Ok(stdlib_path) => stdlib_path,
                Err(_) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Standard library path not defined anywhere. You can set it with 'CYRUS_STDLIB_PATH' environment variable or '--stdlib' command line argument.".to_string()),
                        location: None,
                    });
                    exit(1);
                }
            },
        }
    }

    fn build_imported_module_path(
        &self,
        module_path: ModulePath,
        mut segments: Vec<ModuleSegment>,
        loc: Location,
        span_end: usize,
    ) -> ImportedModulePath {
        let module_name = module_segments_as_string(segments.clone());

        // Remove "std" segment if present and switch source path accordingly
        let mut sources = self.opts.sources_dir.clone();
        if matches!(segments.first(), Some(ModuleSegment::SubModule(id)) if id.name == "std") {
            segments.remove(0);
            sources.insert(0, self.build_stdlib_modules_path());
        }

        let mut module_file_path = String::new();

        let lookup_dir_and_develop_module_file_path =
            |mut module_file_path: String, dir_path: String, codegen_file_path: String| -> String {
                if let Some(found_path) = find_file_from_sources(dir_path, sources.clone()) {
                    module_file_path.push_str(found_path.to_str().unwrap());
                    module_file_path.push('/');
                    module_file_path
                } else {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::ModuleNotFound(module_name.clone()),
                        location: Some(DiagLoc {
                            file: codegen_file_path,
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            };

        let handle_import_single = |idx: usize,
                                    module_segment_singles: Vec<ModuleSegmentSingle>|
         -> ImportedModulePath {
            if idx != segments.len() - 1 {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Single segment must be the last in import path.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }

            // Build path from all previous segments
            let base_path =
                self.build_imported_module_path(module_path.clone(), segments[0..idx].to_vec(), loc.clone(), span_end);

            if !Path::new(&base_path.file_path).is_file() {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(
                        "Cannot use `::` to access from a directory. Expected a module file.".to_string(),
                    ),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }

            ImportedModulePath {
                file_path: base_path.file_path,
                module_path: module_path.clone(),
                singles: Some(module_segment_singles.clone()),
                loc: loc.clone(),
                span_end,
            }
        };

        let lookup_file = |idx: usize, file_path: String| -> Option<ImportedModulePath> {
            if let Some(found_path) = find_file_from_sources(file_path, sources.clone()) {
                if idx == segments.len() - 1 {
                    // last segment is a file
                    Some(ImportedModulePath {
                        file_path: found_path.to_str().unwrap().to_string(),
                        module_path: module_path.clone(),
                        singles: None,
                        loc: loc.clone(),
                        span_end,
                    })
                } else {
                    if (segments.len() - 1) - idx == 1 {
                        // last segment is import single
                        let next_segment = segments[idx + 1].clone();
                        match next_segment {
                            ModuleSegment::SubModule(_) => None,
                            ModuleSegment::Single(module_segment_singles) => {
                                Some(handle_import_single(idx + 1, module_segment_singles))
                            }
                        }
                    } else {
                        None
                    }
                }
            } else {
                None
            }
        };

        // Traverse all segments
        for idx in 0..segments.len() {
            let segment = &segments[idx];
            match segment {
                ModuleSegment::SubModule(identifier) => {
                    let dir_path = format!("{}{}", module_file_path, identifier.name);
                    let file_path = format!("{}{}.cyr", module_file_path, identifier.name);

                    match lookup_file(idx, file_path) {
                        Some(generated_module_import_path) => {
                            return generated_module_import_path;
                        }
                        None => {
                            module_file_path = lookup_dir_and_develop_module_file_path(
                                module_file_path.clone(),
                                dir_path,
                                self.file_path.clone(),
                            );
                        }
                    }
                }

                ModuleSegment::Single(singles) => {
                    return handle_import_single(idx, singles.clone());
                }
            }
        }

        // Reached end, check if final path is a valid directory (not allowed)
        if Path::new(&module_file_path).is_dir() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Importing module `{}` as a directory is not allowed.",
                    module_name
                )),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
            });
            exit(1);
        }

        ImportedModulePath {
            file_path: module_file_path,
            module_path,
            singles: None,
            loc,
            span_end,
        }
    }

    pub(crate) fn get_module_metadata_by_file_path(&self, module_file_path: String) -> Option<ModuleMetadata<'ctx>> {
        let module_metadata_registry = self.module_metadata_registry.borrow_mut();
        module_metadata_registry
            .iter()
            .find(|r| r.module_file_path == module_file_path)
            .cloned()
    }

    pub(crate) fn get_module_metadata_by_module_id(&self, module_id: u64) -> Option<RefMut<ModuleMetadata<'ctx>>> {
        let registry = self.module_metadata_registry.borrow_mut();
        let index = registry.iter().position(|r| r.module_id == module_id)?;
        Some(RefMut::map(registry, move |v| &mut v[index]))
    }

    pub(crate) fn build_import(&mut self, import: Import) {
        for module_path in import.paths.clone() {
            let import_path = self.build_imported_module_path(
                module_path.clone(),
                module_path.segments.clone(),
                import.loc.clone(),
                import.span.end,
            );

            self.error_if_module_already_loaded(import_path.module_path.clone());
            self.build_imported_module(import_path);
        }
    }

    // Local means it only make sense in this module.
    // It may be aliased or not.
    pub(crate) fn build_local_module_name(&self, module_path: ModulePath) -> String {
        let last_segment = module_path.segments.last().unwrap();

        match last_segment {
            ModuleSegment::SubModule(identifier) => module_path.alias.unwrap_or(identifier.name.clone()),
            ModuleSegment::Single(_) => {
                if module_path.alias.is_some() {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(
                            "Cannot rename imported module when you considered to import singles.".to_string(),
                        ),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: module_path.loc.line,
                            column: module_path.loc.column,
                            length: module_path.span.end,
                        }),
                    });
                    exit(1);
                } else {
                    match module_path.segments.iter().nth_back(1).unwrap() {
                        ModuleSegment::SubModule(identifier) => module_path.alias.unwrap_or(identifier.name.clone()),
                        ModuleSegment::Single(_) => unreachable!(),
                    }
                }
            }
        }
    }

    fn build_module_with_sub_codegen(&self, import_path: ImportedModulePath) -> (BuildManifest, ModuleID) {
        let module_name = self.build_local_module_name(import_path.module_path.clone());

        let sub_module = Rc::new(RefCell::new(self.context.create_module(&module_name)));
        let sub_builder = self.context.create_builder();
        let target_machine = CodeGenLLVM::setup_target_machine(
            Rc::clone(&sub_module),
            self.opts.reloc_mode.to_llvm_reloc_mode(),
            self.opts.code_model.to_llvm_code_model(),
            self.opts.cpu.clone(),
            self.opts.target_triple.clone(),
        );
        let base_dir = env::current_dir().unwrap().to_str().unwrap().to_string();
        let file_path = relative_to_absolute(import_path.file_path.clone(), base_dir).unwrap();
        let program = parser::parse_program(file_path.clone()).0;

        let module_id = generate_module_id();

        let sub_codegen = Rc::new(RefCell::new(CodeGenLLVM {
            program,
            opts: self.opts.clone(),
            context: &self.context,
            module_id,
            module: Rc::clone(&sub_module),
            module_name: module_name.clone(),
            builder: sub_builder,
            target_machine,
            build_manifest: BuildManifest::default(),
            file_path: file_path,
            reporter: self.reporter.clone(),
            entry_point: None,
            entry_point_path: self.entry_point_path.clone(),
            compiler_invoked_single: self.compiler_invoked_single,
            block_registry: BlockRegistry::new(),
            imported_modules: Vec::new(),
            output_kind: self.output_kind.clone(),
            final_build_dir: self.final_build_dir.clone(),
            module_metadata_registry: Rc::clone(&self.module_metadata_registry),
            local_ir_value_registry: Rc::new(RefCell::new(HashMap::new())),
        }));
        
        self.add_module_to_metadata_registry(ModuleMetadata {
            module_id: module_id,
            module_file_path: import_path.file_path.clone(),
            func_table: FuncTable::new(),
            struct_table: StructTable::new(),
            global_variables_table: GlobalVariablesTable::new(),
            typedef_table: TypedefTable::new(),
        });

        let mut sub_codegen_ref = sub_codegen.borrow_mut();

        sub_codegen_ref.compile();
        (sub_codegen_ref.build_manifest.clone(), sub_codegen_ref.module_id)
    }

    pub(crate) fn add_module_to_metadata_registry(&self, module_metadata: ModuleMetadata<'ctx>) {
        self.module_metadata_registry.borrow_mut().push(module_metadata);
    }

    fn build_imported_module(&mut self, import_path: ImportedModulePath) {
        let module_metadata = match self.get_module_metadata_by_file_path(import_path.file_path.clone()) {
            Some(module_metadata) => {
                // This module compiled before, let's use the same module_metadata.
                module_metadata
            }
            None => {
                let (build_manifest, module_id) = self.build_module_with_sub_codegen(import_path.clone());

                self.build_manifest = build_manifest;
                self.get_module_metadata_by_module_id(module_id)
                    .expect("Couldn't get module metadata by module id.")
                    .clone()
            }
        };

        if let Some(module_segment_singles) = import_path.singles.clone() {
            // Build and load import singles into local_defs.
            self.build_module_import_singles(
                module_metadata.module_id.clone(),
                module_segment_singles,
                import_path.module_path.clone(),
                import_path.loc.clone(),
                import_path.span_end,
            );
        } else {
            self.imported_modules.push(ImportedModules {
                module_id: module_metadata.module_id,
                module_path: import_path.module_path.clone(),
            });
        }
    }

    // FIXME Depends on resolver.
    pub(crate) fn build_module_import_singles(
        &mut self,
        module_id: u64,
        module_segment_singles: Vec<ModuleSegmentSingle>,
        module_path: ModulePath,
        loc: Location,
        span_end: usize,
    ) {
        // FIXME
        todo!();

        // let module_metadata = match self.get_module_metadata_by_module_id(module_id) {
        //     Some(module_metadata) => module_metadata,
        //     None => panic!("Couldn't lookup module in the module metadata registry."),
        // };

        // module_segment_singles.iter().for_each(|single| {
        //     let lookup_result = self.lookup_from_module_metadata(
        //         single.identifier.name.clone(),
        //         module_metadata.clone(),
        //         loc.clone(),
        //         span_end,
        //     );

        //     match lookup_result {
        //         DefinitionLookupResult::Func(mut func_metadata) => {
        //             func_metadata.imported_from = Some(module_path.clone());
        //             let (func_name, func_metadata) = self.build_decl_imported_func(func_metadata);
        //             self.local_defs.func_table.insert(func_name, func_metadata);
        //         }
        //         DefinitionLookupResult::Struct(struct_metadata) => {
        //             // FIXME
        //             todo!();

        //             // let struct_name = {
        //             //     match struct_metadata.struct_name.segments.last().unwrap() {
        //             //         ModuleSegment::SubModule(identifier) => identifier.name.clone(),
        //             //         ModuleSegment::Single(_) => unreachable!(),
        //             //     }
        //             // };

        //             // let new_struct_metadata = self.build_decl_imported_struct_methods(
        //             //     module_path.clone(),
        //             //     import.clone(),
        //             //     struct_metadata.clone(),
        //             // );

        //             // self.local_defs
        //             //     .struct_table
        //             //     .insert(struct_name, new_struct_metadata.clone());
        //         }
        //         DefinitionLookupResult::Typedef(typedef_metadata) => {
        //             self.local_defs
        //                 .typedef_table
        //                 .insert(single.identifier.name, typedef_metadata.clone());
        //         }
        //         DefinitionLookupResult::GlobalVariable(mut global_variable_metadata) => {
        //             let global_value = self.build_decl_imported_global_variable(
        //                 global_variable_metadata.clone(),
        //                 loc.clone(),
        //                 span_end,
        //             );

        //             global_variable_metadata.global_value = global_value;
        //             self.local_defs
        //                 .global_variables_table
        //                 .insert(single.identifier.name, global_variable_metadata.clone());
        //         }
        //     }
        // });
    }

    pub(crate) fn generate_abi_name(&self, module_id: String, name: String) -> String {
        format!("{}_{}", module_id, name)
    }

    pub(crate) fn generate_method_abi_name(
        &self,
        module_id: String,
        struct_name: String,
        method_name: String,
    ) -> String {
        format!("{}_{}_{}", module_id, struct_name, method_name)
    }

    pub(crate) fn insert_local_ir_value(&self, id: LocalIRValueID, value: LocalIRValue<'ctx>) {
        let mut local_ir_value_registry = self.local_ir_value_registry.borrow_mut();
        local_ir_value_registry.insert(id, value);
    }

    pub(crate) fn get_local_func_ir_value(&self, id: LocalIRValueID) -> FunctionValue<'ctx> {
        let local_ir_value_registry = self.local_ir_value_registry.borrow();
        let local_ir_value = match local_ir_value_registry.get(&id) {
            Some(local_ir_value) => local_ir_value,
            None => {
                panic!("Could not get func ir value from the registry.");
            }
        };

        if let LocalIRValue::Func(func_value) = local_ir_value {
            *func_value
        } else {
            panic!("Local IR Value is not a function.")
        }
    }
}

pub fn generate_module_id() -> ModuleID {
    let mut rng = rand::rng();
    rng.random::<u64>()
}

pub fn generate_local_ir_value_id() -> LocalIRValueID {
    let mut rng = rand::rng();
    rng.random::<u64>()
}

impl<'a> ModuleMetadata<'a> {
    pub fn insert_func(&mut self, name: String, metadata: FuncMetadata<'a>) {
        self.func_table.insert(name, metadata);
    }

    pub fn insert_global_variable(&mut self, name: String, metadata: GlobalVariableMetadata<'a>) {
        self.global_variables_table.insert(name, metadata);
    }

    pub fn insert_struct(&mut self, name: String, metadata: StructMetadata<'a>) {
        self.struct_table.insert(name, metadata);
    }

    pub fn insert_typedef(&mut self, name: String, metadata: TypedefMetadata<'a>) {
        self.typedef_table.insert(name, metadata);
    }
}
