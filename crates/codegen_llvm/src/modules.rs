use crate::{
    CodeGenLLVM,
    build::BuildManifest,
    diag::*,
    funcs::{FuncMetadata, FuncTable},
    structs::StructTable,
    types::InternalStructType,
};
use ast::{
    ast::{Import, ModulePath, ModuleSegment, ModuleSegmentSingle, StorageClass},
    format::module_segments_as_string,
    token::Location,
};
use inkwell::module::Module;
use std::{cell::RefCell, collections::HashMap, path::Path, process::exit, rc::Rc};
use utils::fs::find_file_from_sources;

#[derive(Debug, Clone)]
pub struct ModuleMetadata<'ctx> {
    pub identifier: String,
    pub file_path: String,
    pub module: Rc<RefCell<Module<'ctx>>>,
    pub func_table: HashMap<String, FuncMetadata<'ctx>>,
    pub struct_table: HashMap<String, InternalStructType<'ctx>>,
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

    pub(crate) fn find_loaded_module(&self, module_id: String) -> Option<ModuleMetadata<'ctx>> {
        self.loaded_modules.iter().find(|m| m.identifier == module_id).cloned()
    }

    fn build_import_singles(&self, module_segment_singles: Vec<ModuleSegmentSingle>) {
        println!("implement module_segment_singles\n");
        // for single in  module_segment_singles {}
    }

    fn build_import_module_path(&mut self, segments: Vec<ModuleSegment>, loc: Location, span_end: usize) -> String {
        let sources = &self.opts.sources_dir;
        let segments_str = module_segments_as_string(segments.clone());

        let mut module_file_path = String::new();

        for (idx, module_segment) in segments.iter().enumerate() {
            match module_segment {
                ModuleSegment::SubModule(identifier) => {
                    // once consider identifier as sub_module (directory) and if it's not found in any of the sources
                    // in the second stage consider identifier as a module (file) and try to determine that path exists.
                    let directory_path = format!("{}{}", module_file_path, identifier.name.clone());
                    let source_file_path = format!("{}{}.cyr", module_file_path, identifier.name.clone());

                    // source file has higher priority to a directory
                    match find_file_from_sources(source_file_path, sources.clone()) {
                        Some(new_module_file_path) => {
                            module_file_path = new_module_file_path.to_str().unwrap().to_string();

                            if segments.len() - 1 > idx {
                                match &segments[idx + 1] {
                                    ModuleSegment::SubModule(next_segment_identifier) => {
                                        display_single_diag(Diag {
                                            level: DiagLevel::Error,
                                            kind: DiagKind::Custom(format!(
                                                "Module '{}' is already found as a module but you trying to import '{}' from that module.",
                                                module_segments_as_string(segments[..(idx + 1)].to_vec()),
                                                next_segment_identifier.name.clone()
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
                                    ModuleSegment::Single(_) => {}
                                }
                            }
                        }
                        None => match find_file_from_sources(directory_path, sources.clone()) {
                            Some(new_module_file_path) => {
                                module_file_path.push_str(new_module_file_path.to_str().unwrap());
                                module_file_path.push_str("/");
                            }
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
                        },
                    }
                }
                ModuleSegment::Single(module_segment_singles) => {
                    self.build_import_singles(module_segment_singles.clone());
                    return self.build_import_module_path(segments[0..(segments.len() - 1)].to_vec(), loc, span_end);
                }
            }
        }

        if Path::new(&module_file_path).is_dir() {
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

        module_file_path
    }

    pub(crate) fn build_module_id(&self, module_path: ModulePath) -> String {
        match module_path.segments.last().unwrap() {
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

    fn error_if_module_already_loaded(&self, module_id: String, module_path: ModulePath) {
        if let Some(_) = self.find_loaded_module(module_id.clone()) {
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
            let module_id = self.build_module_id(module_path.clone());
            let file_path =
                self.build_import_module_path(module_path.segments.clone(), import.loc.clone(), import.span.end);

            self.error_if_module_already_loaded(module_id.clone(), module_path);
            self.build_imported_module(file_path.clone(), module_id);
        }
    }

    fn build_imported_module(&mut self, file_path: String, module_id: String) -> ModuleMetadata<'ctx> {
        let sub_module = Rc::new(RefCell::new(self.context.create_module(&module_id)));
        let sub_builder = self.context.create_builder();
        let target_machine = CodeGenLLVM::target_machine(Rc::clone(&sub_module));
        let program = parser::parse_program(file_path.clone()).0;

        let mut sub_codegen = CodeGenLLVM {
            program,
            opts: self.opts.clone(),
            context: self.context,
            module: Rc::clone(&sub_module),
            module_name: module_id.clone(),
            builder: sub_builder,
            target_machine,
            build_manifest: BuildManifest::default(),
            file_path: file_path.clone(),
            reporter: self.reporter.clone(),
            entry_point: None,
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
            func_table: self.build_imported_funcs(sub_codegen.func_table, module_id.clone()),
            struct_table: self.build_imported_structs(sub_codegen.struct_table),
            identifier: module_id,
            file_path,
        };

        self.loaded_modules.push(module_metadata.clone());

        module_metadata
    }

    fn build_imported_funcs(
        &mut self,
        func_table: FuncTable<'ctx>,
        module_id: String,
    ) -> HashMap<String, FuncMetadata<'ctx>> {
        let mut imported_funcs: HashMap<String, FuncMetadata> = HashMap::new();

        for (_, (_, metadata)) in func_table.iter().enumerate() {
            if metadata.func_decl.storage_class == StorageClass::Public
                || metadata.func_decl.storage_class == StorageClass::PublicExtern
                || metadata.func_decl.storage_class == StorageClass::PublicInline
            {
                let mut new_metadata = metadata.clone();

                // function naming collisions fix happens here
                new_metadata.func_decl.renamed_as = Some(metadata.func_decl.name.clone());
                new_metadata.func_decl.name =
                    self.generate_abi_name(module_id.clone(), new_metadata.func_decl.name.clone());

                let param_types = self.build_func_params(
                    metadata.func_decl.name.clone(),
                    metadata.func_decl.loc.clone(),
                    metadata.func_decl.span.end,
                    metadata.func_decl.params.list.clone(),
                    metadata.func_decl.params.variadic.clone(),
                );

                let func_value = self.build_func_decl(new_metadata.func_decl.clone(), param_types);
                new_metadata.ptr = func_value;

                imported_funcs.insert(new_metadata.func_decl.get_usable_name(), new_metadata);
            } else {
                imported_funcs.insert(metadata.func_decl.get_usable_name(), metadata.clone());
            }
        }

        imported_funcs
    }

    // TODO Implement import struct methods
    fn build_imported_structs(&self, struct_table: StructTable<'ctx>) -> HashMap<String, InternalStructType<'ctx>> {
        let mut imported_structs: HashMap<String, InternalStructType> = HashMap::new();

        for (_, (struct_name, metadata)) in struct_table.iter().enumerate() {
            imported_structs.insert(struct_name.clone(), metadata.clone());
        }

        imported_structs
    }

    pub(crate) fn generate_abi_name(&self, module_id: String, name: String) -> String {
        format!("{}_{}", module_id, name)
    }
}
