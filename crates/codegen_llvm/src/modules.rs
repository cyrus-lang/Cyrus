use crate::context::{BlockRegistry, CodeGenLLVM};
use crate::structs::{StructID, StructMethodMetadata};
use crate::{
    build::BuildManifest,
    diag::*,
    funcs::{FuncMetadata, FuncTable},
    resolver::MetadataResolverResult,
    structs::{StructMetadata, StructTable},
    types::{TypedefMetadata, TypedefTable},
    variables::{GlobalVariableMetadata, GlobalVariablesTable},
};
use ast::{
    ast::{AccessSpecifier, Import, ModulePath, ModuleSegment, ModuleSegmentSingle},
    format::module_segments_as_string,
    token::Location,
};
use inkwell::types::StructType;
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

    pub(crate) fn get_imported_module(&self, module_id: u64) -> Option<ImportedModules> {
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

        // Remove "std" segment if present and switch source path accordingly.
        let mut sources = self.opts.source_dirs.clone();

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
        let module_metadata = module_metadata_registry
            .iter()
            .find(|r| r.module_file_path == module_file_path)
            .cloned();
        drop(module_metadata_registry);
        module_metadata
    }

    pub(crate) fn get_module_metadata_by_module_id(&self, module_id: u64) -> Option<RefMut<ModuleMetadata<'ctx>>> {
        let index = {
            let registry = self.module_metadata_registry.borrow();
            let index = registry.iter().position(|r| r.module_id == module_id)?;
            drop(registry);
            index
        };

        let registry = self.module_metadata_registry.borrow_mut();
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
    pub(crate) fn get_local_module_name(&self, module_path: ModulePath) -> String {
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
        let module_name = self.get_local_module_name(import_path.module_path.clone());

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
        let mut module_metadata_registry = self.module_metadata_registry.borrow_mut();
        module_metadata_registry.push(module_metadata);
        drop(module_metadata_registry);
    }

    fn build_imported_module(&mut self, mut import_path: ImportedModulePath) {
        let module_metadata = match self.get_module_metadata_by_file_path(import_path.file_path.clone()) {
            Some(module_metadata) => {
                // This module compiled before, let's use the same module_metadata.
                module_metadata
            }
            None => {
                let (build_manifest, module_id) = self.build_module_with_sub_codegen(import_path.clone());
                self.build_manifest = build_manifest;

                let module_metadata = self
                    .get_module_metadata_by_module_id(module_id)
                    .expect("Couldn't get module metadata by module id.");
                let module_metadata_clone = module_metadata.clone();
                drop(module_metadata);
                module_metadata_clone
            }
        };

        if let Some(module_segment_singles) = import_path.singles.clone() {
            // Build and load import singles into local_defs.

            import_path.module_path.segments.pop();

            self.build_module_import_singles(
                module_metadata.module_id.clone(),
                module_segment_singles,
                import_path.module_path,
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

    pub(crate) fn error_if_func_is_private(
        &self,
        func_name: String,
        access_specifier: AccessSpecifier,
        loc: Location,
        span_end: usize,
    ) {
        if access_specifier.is_private() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::ImportingPrivateFunc(func_name),
                location: Some(DiagLoc {
                    file: self.file_path.clone(),
                    line: loc.line,
                    column: loc.column,
                    length: span_end,
                }),
            });
            exit(1);
        }
    }

    pub(crate) fn build_module_import_singles(
        &mut self,
        module_id: u64,
        module_segment_singles: Vec<ModuleSegmentSingle>,
        module_path: ModulePath,
        loc: Location,
        span_end: usize,
    ) {
        let import_func = |mut func_metadata: FuncMetadata<'ctx>| {
            self.error_if_func_is_private(
                func_metadata.func_decl.get_usable_name(),
                func_metadata.func_decl.access_specifier.clone(),
                loc.clone(),
                span_end,
            );

            let func_name = func_metadata.func_decl.get_usable_name();
            func_metadata.imported_from = Some(module_id);
            let func_value = self.build_func_decl(
                func_metadata.func_decl.clone(),
                func_metadata.params_metadata.clone(),
                false,
                false,
            );
            let local_ir_value_id = generate_local_ir_value_id();
            self.insert_local_ir_value(local_ir_value_id, LocalIRValue::Func(func_value));
            let mut module_metadata = match self.get_module_metadata_by_module_id(self.module_id) {
                Some(module_metadata) => module_metadata,
                None => panic!("Couldn't lookup module in the module metadata registry."),
            };
            func_metadata.local_ir_value_id = local_ir_value_id;
            module_metadata.func_table.insert(func_name, func_metadata);
            drop(module_metadata);
        };

        // let import_struct = |mut func_metadata: FuncMetadata<'ctx>| {
        //     self.error_if_func_is_private(
        //         func_metadata.func_decl.get_usable_name(),
        //         func_metadata.func_decl.access_specifier.clone(),
        //         loc.clone(),
        //         span_end,
        //     );

        //     let func_name = func_metadata.func_decl.get_usable_name();
        //     func_metadata.imported_from = Some(module_id);
        //     let func_value = self.build_func_decl(
        //         func_metadata.func_decl.clone(),
        //         func_metadata.params_metadata.clone(),
        //         false,
        //         false,
        //     );
        //     let local_ir_value_id = generate_local_ir_value_id();
        //     self.insert_local_ir_value(local_ir_value_id, LocalIRValue::Func(func_value));
        //     let mut module_metadata = match self.get_module_metadata_by_module_id(self.module_id) {
        //         Some(module_metadata) => module_metadata,
        //         None => panic!("Couldn't lookup module in the module metadata registry."),
        //     };
        //     func_metadata.local_ir_value_id = local_ir_value_id;
        //     module_metadata.func_table.insert(func_name, func_metadata);
        //     drop(module_metadata);
        // };

        module_segment_singles.iter().for_each(|single| {
            let metadata_resolver_result = match self.resolve_metadata(module_id, single.identifier.name.clone()) {
                Some(metadata_resolver_result) => metadata_resolver_result,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::SymbolNotFoundInModule(
                            single.identifier.name.clone(),
                            module_segments_as_string(module_path.segments.clone()),
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
            };

            match metadata_resolver_result {
                MetadataResolverResult::Func(func_metadata) => import_func(func_metadata),
                MetadataResolverResult::Struct(struct_metadata) => todo!(),
                MetadataResolverResult::Typedef(typedef_metadata) => todo!(),
                MetadataResolverResult::GlobalVariable(global_variable_metadata) => todo!(),
            }
        });
    }

    pub(crate) fn insert_local_ir_value(&self, id: LocalIRValueID, value: LocalIRValue<'ctx>) {
        let mut local_ir_value_registry = self.local_ir_value_registry.borrow_mut();
        local_ir_value_registry.insert(id, value);
        drop(local_ir_value_registry);
    }

    pub(crate) fn get_local_func_ir_value(&self, id: LocalIRValueID) -> FunctionValue<'ctx> {
        let local_ir_value_registry = self.local_ir_value_registry.borrow();
        let local_ir_value = match local_ir_value_registry.get(&id).cloned() {
            Some(local_ir_value) => local_ir_value,
            None => {
                panic!("Could not get func ir value from the registry.");
            }
        };

        drop(local_ir_value_registry);
        if let LocalIRValue::Func(func_value) = local_ir_value {
            func_value
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

    pub fn get_struct_metadata_by_id(&mut self, struct_id: StructID) -> &mut StructMetadata<'a> {
        self.struct_table
            .iter_mut()
            .find(|r| r.1.struct_id == struct_id)
            .expect("Couldn't find struct from module metadata registry.")
            .1
    }

    pub fn get_struct_metadata_by_name(&self, struct_name: String) -> Option<StructMetadata<'a>> {
        self.struct_table.get(&struct_name).cloned()
    }

    pub fn get_and_update_struct_type(&mut self, struct_id: StructID, struct_type: StructType<'a>) {
        let struct_metadata = self.get_struct_metadata_by_id(struct_id);
        struct_metadata.struct_type = struct_type;
    }

    pub fn get_and_update_struct_methods(
        &mut self,
        struct_id: StructID,
        struct_methods: Vec<StructMethodMetadata<'a>>,
    ) {
        let struct_metadata = self.get_struct_metadata_by_id(struct_id);
        struct_metadata.methods = struct_methods;
    }
}
