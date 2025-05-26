use crate::{CodeGenLLVM, build::BuildManifest, diag::*, funcs::FuncTable, structs::StructTable};
use ast::{
    ast::{Field, FuncDecl, Identifier, Import, ModulePath, ModuleSegment, StorageClass, Struct},
    format::module_segments_as_string,
    token::{Location, Span},
};
use inkwell::{module::Module, types::StructType};
use std::{cell::RefCell, collections::HashMap, process::exit, rc::Rc};
use utils::fs::find_file_from_sources;

#[derive(Debug, Clone)]
pub struct ExportedFuncMetadata {
    pub func_decl: FuncDecl,
}

#[derive(Debug, Clone)]
pub struct ExportedStructMetadata<'ctx> {
    pub name: String,
    pub storage_class: StorageClass,
    pub inherits: Vec<Identifier>,
    pub fields: Vec<Field>,
    pub methods: Vec<FuncDecl>,
    pub struct_type: StructType<'ctx>,
}

#[derive(Debug, Clone)]
pub struct ModuleMetadata<'ctx> {
    pub identifier: String,
    pub file_path: String,
    pub module: Rc<RefCell<Module<'ctx>>>,
    pub imported_funcs: HashMap<String, ExportedFuncMetadata>,
    pub imported_structs: HashMap<String, ExportedStructMetadata<'ctx>>,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn rebuild_dependent_modules(&mut self) {
        if let Some(module_deps) = self.dependent_modules.get(&self.file_path) {
            for file_path in module_deps {
                // skip for rebuilding entry_point module
                let module_metadata = self
                    .loaded_modules
                    .iter()
                    .find(|m| *m.file_path == *file_path)
                    .cloned()
                    .expect("Failed to get a loaded module by it's file path.");

                dbg!(module_metadata.identifier.clone());
            }
        }
    }

    pub(crate) fn find_loaded_module(&self, module_identifier: String) -> Option<ModuleMetadata<'ctx>> {
        self.loaded_modules
            .iter()
            .find(|m| m.identifier == module_identifier)
            .cloned()
    }

    fn build_import_module_path(&mut self, mut segments: Vec<ModuleSegment>, loc: Location, span_end: usize) -> String {
        let sources = &self.opts.sources_dir;
        let segments_str = module_segments_as_string(segments.clone());

        let mut begging_path = {
            match segments.clone().first().unwrap() {
                ModuleSegment::SubModule(identifier) => {
                    segments.remove(0);
                    let file_name = identifier.name.clone();
                    match find_file_from_sources(file_name, sources.clone()) {
                        Some(path) => path,
                        None => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::ModuleNotFound(segments_str.clone()),
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
                }
            }
        };

        for module_path in segments {
            match module_path {
                ModuleSegment::SubModule(identifier) => {
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
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::ModuleNotFound(segments_str.clone()),
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
                    }
                }
            }
        }

        if begging_path.is_dir() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Import module '{}' as directory is not allowed.",
                    segments_str.clone()
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

        begging_path.to_str().unwrap().to_string()
    }

    pub(crate) fn build_module_identifier(&self, module_path: ModulePath) -> String {
        module_path.alias.unwrap_or({
            match module_path.segments.last().unwrap() {
                ModuleSegment::SubModule(identifier) => identifier.name.clone(),
            }
        })
    }

    fn check_import_twice(&self, module_identifier: String, module_path: ModulePath) {
        if let Some(_) = self.find_loaded_module(module_identifier.clone()) {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::Custom(format!(
                    "Cannot import module '{}' twice.",
                    module_segments_as_string(module_path.segments)
                )),
                location: None,
            });
            exit(1);
        }
    }

    pub(crate) fn build_import(&mut self, import: Import) {
        for module_path in import.paths.clone() {
            let file_path =
                self.build_import_module_path(module_path.segments.clone(), import.loc.clone(), import.span.end);
            let module_identifier = self.build_module_identifier(module_path.clone());
            self.check_import_twice(module_identifier.clone(), module_path);
            self.build_imported_module(file_path.clone(), module_identifier);
        }
    }

    fn build_imported_module(&mut self, file_path: String, module_identifier: String) -> ModuleMetadata<'ctx> {
        let sub_module = Rc::new(RefCell::new(self.context.create_module(&module_identifier)));
        let sub_builder = self.context.create_builder();
        let target_machine = CodeGenLLVM::target_machine(Rc::clone(&sub_module));
        let program = parser::parse_program(file_path.clone()).0;

        let mut sub_codegen = CodeGenLLVM {
            program,
            opts: self.opts.clone(),
            context: self.context,
            module: Rc::clone(&sub_module),
            module_name: module_identifier.clone(),
            builder: sub_builder,
            target_machine,
            build_manifest: BuildManifest::default(),
            file_path: file_path.clone(),
            reporter: self.reporter.clone(),
            entry_point: None,
            is_entry_point: false,
            entry_point_path: self.entry_point_path.clone(),
            func_table: HashMap::new(),
            struct_table: HashMap::new(),
            compiler_invoked_single: self.compiler_invoked_single,
            current_func_ref: None,
            current_block_ref: None,
            terminated_blocks: Vec::new(),
            string_type: self.string_type.clone(),
            loaded_modules: Vec::new(),
            dependent_modules: HashMap::new(),
            output_kind: self.output_kind.clone(),
            internal_object_modules: self.internal_object_modules.clone(),
        };

        // preventing entry_point of being in dependent_modules
        if self.file_path != self.entry_point_path {
            sub_codegen
                .dependent_modules
                .insert(file_path.clone(), vec![self.file_path.clone()]);
        }

        sub_codegen.compile();
        self.build_manifest = sub_codegen.build_manifest.clone();

        let module_metadata = ModuleMetadata {
            module: Rc::clone(&sub_module),
            imported_funcs: self.build_imported_funcs(sub_codegen.func_table),
            imported_structs: self.build_imported_structs(sub_codegen.struct_table),
            identifier: module_identifier,
            file_path,
        };

        self.loaded_modules.push(module_metadata.clone());
        module_metadata
    }

    fn build_imported_funcs(&mut self, func_table: FuncTable) -> HashMap<String, ExportedFuncMetadata> {
        let mut imported_funcs: HashMap<String, ExportedFuncMetadata> = HashMap::new();

        for (_, (_, metadata)) in func_table.iter().enumerate() {
            // only pub funcs are exported imported from sub_module
            if metadata.func_decl.storage_class == StorageClass::Public {
                imported_funcs.insert(
                    metadata.func_decl.renamed_as.clone().unwrap(),
                    ExportedFuncMetadata {
                        func_decl: metadata.func_decl.clone(),
                    },
                );

                self.build_func_decl(metadata.func_decl.clone(), false);
            }
        }

        imported_funcs
    }

    // TODO
    // Implement import struct methods
    fn build_imported_structs(&self, struct_table: StructTable) -> HashMap<String, ExportedStructMetadata<'ctx>> {
        let mut imported_structs: HashMap<String, ExportedStructMetadata> = HashMap::new();

        for (_, (struct_name, metadata)) in struct_table.iter().enumerate() {
            // only pub structs are exported imported from sub_module
            let struct_statement = Struct {
                name: struct_name.clone(),
                storage_class: metadata.storage_class.clone(),
                inherits: metadata.inherits.clone(),
                fields: metadata.fields.clone(),
                methods: Vec::new(), // FIXME
                loc: Location::default(),
                span: Span::default(),
            };
            let struct_type = self.build_struct(struct_statement);

            imported_structs.insert(
                struct_name.clone(),
                ExportedStructMetadata {
                    name: struct_name.clone(),
                    storage_class: metadata.storage_class.clone(),
                    inherits: metadata.inherits.clone(),
                    fields: metadata.fields.clone(),
                    methods: Vec::new(), // FIXME
                    struct_type,
                },
            );
        }

        imported_structs
    }
}
