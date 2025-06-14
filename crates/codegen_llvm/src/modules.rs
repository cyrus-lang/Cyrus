use crate::{
    CodeGenLLVM,
    build::BuildManifest,
    diag::*,
    funcs::{FuncMetadata, FuncTable},
    structs::{StructMetadata, StructTable},
};
use ast::{
    ast::{Import, ModulePath, ModuleSegment, StorageClass},
    format::module_segments_as_string,
    token::Location,
};
use inkwell::module::Module;
use std::{cell::RefCell, collections::HashMap, process::exit, rc::Rc};
use utils::fs::find_file_from_sources;

#[derive(Debug, Clone)]
pub struct ModuleMetadata<'ctx> {
    pub identifier: String,
    pub file_path: String,
    pub module: Rc<RefCell<Module<'ctx>>>,
    pub func_table: HashMap<String, FuncMetadata<'ctx>>,
    pub struct_table: HashMap<String, StructMetadata<'ctx>>,
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
            final_build_dir: self.final_build_dir.clone(),
            current_loop_ref: None,
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
            func_table: self.build_imported_funcs(sub_codegen.func_table),
            struct_table: self.build_imported_structs(sub_codegen.struct_table),
            identifier: module_identifier,
            file_path,
        };

        self.loaded_modules.push(module_metadata.clone());

        module_metadata
    }

    fn build_imported_funcs(&mut self, func_table: FuncTable<'ctx>) -> HashMap<String, FuncMetadata<'ctx>> {
        let mut imported_funcs: HashMap<String, FuncMetadata> = HashMap::new();

        for (_, (_, metadata)) in func_table.iter().enumerate() {
            if metadata.func_decl.storage_class == StorageClass::Public
                || metadata.func_decl.storage_class == StorageClass::PublicExtern
                || metadata.func_decl.storage_class == StorageClass::PublicInline
            {
                let mut new_metadata = metadata.clone();
                let func_value = self.build_func_decl(new_metadata.func_decl.clone());
                new_metadata.ptr = func_value;

                imported_funcs.insert(metadata.func_decl.renamed_as.clone().unwrap(), new_metadata);
            } else {
                imported_funcs.insert(metadata.func_decl.renamed_as.clone().unwrap(), metadata.clone());
            }
        }

        imported_funcs
    }

    // TODO Implement import struct methods
    fn build_imported_structs(&self, struct_table: StructTable<'ctx>) -> HashMap<String, StructMetadata<'ctx>> {
        let mut imported_structs: HashMap<String, StructMetadata> = HashMap::new();

        for (_, (struct_name, metadata)) in struct_table.iter().enumerate() {
            imported_structs.insert(struct_name.clone(), metadata.clone());
        }

        imported_structs
    }
}
