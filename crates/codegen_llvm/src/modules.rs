use crate::{
    build::BuildManifest, diag::*, funcs::{FuncMetadata, FuncTable}, structs::StructTable, types::{InternalStructType, InternalType, TypedefMetadata, TypedefTable}, variables::{GlobalVariableMetadata, GlobalVariablesTable}, CodeGenLLVM
};
use ast::{
    ast::{AccessSpecifier, FuncParamKind, Import, ModulePath, ModuleSegment, ModuleSegmentSingle, TypeSpecifier},
    format::module_segments_as_string,
    token::{Location, Span, Token, TokenKind},
};
use inkwell::{
    AddressSpace,
    llvm_sys::{core::LLVMFunctionType, prelude::LLVMTypeRef},
    module::{Linkage, Module},
    types::FunctionType,
    values::{AnyValue, BasicValueEnum, GlobalValue},
};
use std::{cell::RefCell, collections::HashMap, env, ops::DerefMut, path::Path, process::exit, rc::Rc};
use utils::fs::{find_file_from_sources, relative_to_absolute};

#[derive(Debug, Clone)]
pub struct ModuleMetadata<'a> {
    pub identifier: String,
    pub file_path: String,
    pub module: Rc<RefCell<Module<'a>>>,
    pub func_table: HashMap<String, FuncMetadata<'a>>,
    pub struct_table: HashMap<String, InternalStructType<'a>>,
    pub global_variables_table: GlobalVariablesTable<'a>,
    pub typedef_table: TypedefTable<'a>,
}

#[derive(Debug, Clone)]
pub enum DefinitionLookupResult<'a> {
    Func(FuncMetadata<'a>),
    Struct(InternalStructType<'a>),
    Typedef(TypedefMetadata<'a>),
    GlobalVariable(GlobalVariableMetadata<'a>),
}

#[derive(Clone)]
pub(crate) struct ImportedModuleMetadata<'a> {
    pub metadata: ModuleMetadata<'a>,
    #[allow(unused)]
    pub sub_codegen: Rc<RefCell<CodeGenLLVM<'a>>>,
}

#[derive(Clone)]
pub(crate) struct GeneratedModuleImportPath {
    file_path: String,
    module_path: ModulePath,
    singles: Option<Vec<ModuleSegmentSingle>>,
    loc: Location,
    span_end: usize,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_module_id(&self, module_path: ModulePath) -> String {
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

    fn error_if_module_already_loaded(&self, module_id: String, module_path: ModulePath) {
        if let Some(_) = self.find_imported_module(module_id.clone()) {
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

    // FIXME I don't even know this works or not :/
    pub(crate) fn rebuild_dependent_modules(&mut self) {
        // if let Some(module_deps) = self.dependent_modules.get(&self.file_path) {
        //     for file_path in module_deps {
        // skip for rebuilding entry_point module
        // let imported_module_metadata = self
        //     .imported_modules
        //     .iter()
        //     .find(|m| *m.metadata.file_path == *file_path)
        //     .cloned()
        //     .expect("Failed to get a loaded module by it's file path.");
        //     }
        // }
    }

    pub(crate) fn lookup_definition(&self, name: String) -> Option<DefinitionLookupResult<'ctx>> {
        match match self.func_table.get(&name) {
            Some(func_metadata) => Some(DefinitionLookupResult::Func(func_metadata.clone())),
            None => match self.struct_table.get(&name) {
                Some(struct_metadata) => Some(DefinitionLookupResult::Struct(struct_metadata.clone())),
                None => match self.typedef_table.get(&name) {
                    Some(typedef_metadata) => Some(DefinitionLookupResult::Typedef(typedef_metadata.clone())),
                    None => None,
                },
            },
        } {
            Some(lookup_result) => Some(lookup_result),
            None => None,
        }
    }

    pub(crate) fn lookup_from_module_metadata(
        &self,
        name: String,
        module_metadata: ModuleMetadata<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> DefinitionLookupResult<'ctx> {
        match match module_metadata.func_table.get(&name) {
            Some(func_metadata) => Some(DefinitionLookupResult::Func(func_metadata.clone())),
            None => match module_metadata.struct_table.get(&name) {
                Some(struct_metadata) => Some(DefinitionLookupResult::Struct(struct_metadata.clone())),
                None => match module_metadata.typedef_table.get(&name) {
                    Some(typedef_metadata) => Some(DefinitionLookupResult::Typedef(typedef_metadata.clone())),
                    None => match module_metadata.global_variables_table.get(&name) {
                        Some(global_variable_metadata) => {
                            Some(DefinitionLookupResult::GlobalVariable(global_variable_metadata.clone()))
                        }
                        None => None,
                    },
                },
            },
        } {
            Some(lookup_result) => lookup_result,
            None => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(format!(
                        "Couldn't find anything with '{}' identifier in module '{}'.",
                        name, module_metadata.identifier
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
        }
    }

    pub(crate) fn find_imported_module(&self, module_id: String) -> Option<ImportedModuleMetadata<'ctx>> {
        self.imported_modules
            .iter()
            .find(|m| m.metadata.identifier == module_id)
            .cloned()
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

    fn build_import_module_path(
        &mut self,
        module_path: ModulePath,
        mut segments: Vec<ModuleSegment>,
        loc: Location,
        span_end: usize,
    ) -> GeneratedModuleImportPath {
        let sources;

        let segments_str = module_segments_as_string(segments.clone());
        let first_segment = segments.first().unwrap();
        match first_segment {
            ModuleSegment::SubModule(identifier) => {
                if identifier.name == "std" {
                    segments.remove(0);
                    sources = vec![self.build_stdlib_modules_path()];
                    // FIXME Import single from stdlib isn't working.
                    // segments.insert(
                    //     0,
                    //     ModuleSegment::SubModule(Identifier {
                    //         name: "std".to_string(),
                    //         span: Span::default(),
                    //         loc: Location::default(),
                    //     }),
                    // );
                } else {
                    sources = self.opts.sources_dir.clone();
                }
            }
            ModuleSegment::Single(_) => unreachable!(),
        }

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
                ModuleSegment::Single(module_segment_single) => {
                    let file_path = self
                        .build_import_module_path(
                            module_path.clone(),
                            segments[0..(segments.len() - 1)].to_vec(),
                            loc.clone(),
                            span_end,
                        )
                        .file_path;

                    return GeneratedModuleImportPath {
                        file_path,
                        module_path: module_path.clone(),
                        singles: Some(module_segment_single.clone()),
                        loc: loc.clone(),
                        span_end,
                    };
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

        GeneratedModuleImportPath {
            file_path: module_file_path,
            module_path,
            singles: None,
            loc,
            span_end,
        }
    }

    pub(crate) fn build_import(&mut self, import: Import) {
        for module_path in import.paths.clone() {
            let module_id = self.build_module_id(module_path.clone());

            let generated_module_import_path = self.build_import_module_path(
                module_path.clone(),
                module_path.segments.clone(),
                import.loc.clone(),
                import.span.end,
            );

            self.error_if_module_already_loaded(module_id.clone(), module_path);
            self.build_imported_module(module_id, generated_module_import_path);
        }
    }

    fn build_imported_module(&mut self, module_id: String, generated_module_import_path: GeneratedModuleImportPath) {
        let sub_module = Rc::new(RefCell::new(self.context.create_module(&module_id)));
        let sub_builder = self.context.create_builder();
        let target_machine = CodeGenLLVM::target_machine(Rc::clone(&sub_module));
        let base_dir = env::current_dir().unwrap().to_str().unwrap().to_string();
        let file_path = relative_to_absolute(generated_module_import_path.file_path.clone(), base_dir).unwrap();
        let program = parser::parse_program(file_path.clone()).0;

        let sub_codegen = Rc::new(RefCell::new(CodeGenLLVM {
            program,
            opts: self.opts.clone(),
            context: &self.context,
            module: Rc::clone(&sub_module),
            module_id: module_id.clone(),
            builder: sub_builder,
            target_machine,
            build_manifest: BuildManifest::default(),
            file_path: file_path,
            reporter: self.reporter.clone(),
            entry_point: None,
            entry_point_path: self.entry_point_path.clone(),
            func_table: HashMap::new(),
            struct_table: HashMap::new(),
            typedef_table: HashMap::new(),
            global_variables_table: HashMap::new(),
            compiler_invoked_single: self.compiler_invoked_single,
            current_func_ref: None,
            current_block_ref: None,
            terminated_blocks: Vec::new(),
            string_type: self.string_type.clone(),
            imported_modules: Vec::new(),
            dependent_modules: HashMap::new(),
            output_kind: self.output_kind.clone(),
            final_build_dir: self.final_build_dir.clone(),
            current_loop_ref: None,
        }));

        let mut sub_codegen_ref = sub_codegen.borrow_mut();

        // preventing entry_point of being in dependent_modules
        if self.file_path != self.entry_point_path {
            sub_codegen_ref.dependent_modules.insert(
                generated_module_import_path.file_path.clone(),
                vec![self.file_path.clone()],
            );
        }

        sub_codegen_ref.compile();
        self.build_manifest = sub_codegen_ref.build_manifest.clone();

        if let Some(module_segment_singles) = generated_module_import_path.singles.clone() {
            let module_metadata = ModuleMetadata {
                module: Rc::clone(&sub_module),
                func_table: sub_codegen_ref.func_table.clone(),
                struct_table: sub_codegen_ref.struct_table.clone(),
                global_variables_table: sub_codegen_ref.global_variables_table.clone(),
                typedef_table: sub_codegen_ref.typedef_table.clone(),
                identifier: module_id.clone(),
                file_path: generated_module_import_path.file_path,
            };

            self.imported_modules.push(ImportedModuleMetadata {
                metadata: module_metadata.clone(),
                sub_codegen: Rc::clone(&sub_codegen),
            });

            drop(sub_codegen_ref);

            self.build_module_import_singles(
                module_id.clone(),
                module_segment_singles,
                generated_module_import_path.module_path.clone(),
                generated_module_import_path.loc.clone(),
                generated_module_import_path.span_end,
            );
        } else {
            let imported_funcs = self.build_imported_funcs(sub_codegen_ref.func_table.clone());
            let imported_structs = self.build_imported_structs(
                generated_module_import_path.module_path,
                module_id.clone(),
                sub_codegen_ref.struct_table.clone(),
            );
            let imported_global_variables = self.build_imported_global_variables(
                sub_codegen_ref.global_variables_table.clone(),
                generated_module_import_path.loc.clone(),
                generated_module_import_path.span_end,
            );

            let module_metadata = ModuleMetadata {
                module: Rc::clone(&sub_module),
                func_table: imported_funcs,
                struct_table: imported_structs,
                global_variables_table: imported_global_variables,
                typedef_table: sub_codegen_ref.typedef_table.clone(),
                identifier: module_id.clone(),
                file_path: generated_module_import_path.file_path,
            };

            self.imported_modules.push(ImportedModuleMetadata {
                metadata: module_metadata.clone(),
                sub_codegen: Rc::clone(&sub_codegen),
            });
        }
    }

    pub(crate) fn build_module_import_singles(
        &mut self,
        module_id: String,
        module_segment_singles: Vec<ModuleSegmentSingle>,
        module_path: ModulePath,
        loc: Location,
        span_end: usize,
    ) {
        let module_metadata = match self.find_imported_module(module_id.clone()) {
            Some(imported_module_metadata) => imported_module_metadata.metadata.clone(),
            None => panic!("Couldn't lookup imported module in codegen."),
        };

        for single in module_segment_singles {
            let lookup_result = self.lookup_from_module_metadata(
                single.identifier.name.clone(),
                module_metadata.clone(),
                loc.clone(),
                span_end,
            );

            match lookup_result {
                DefinitionLookupResult::Func(mut func_metadata) => {
                    func_metadata.imported_from = Some(module_path.clone());
                    func_metadata.func_decl.renamed_as = Some(func_metadata.func_decl.name.clone());
                    func_metadata.func_decl.name =
                        self.generate_abi_name(module_id.clone(), func_metadata.func_decl.name);
                    let (func_name, func_metadata) = self.build_decl_imported_func(func_metadata);
                    self.func_table.insert(func_name, func_metadata);
                }
                DefinitionLookupResult::Struct(internal_struct_type) => {
                    let struct_name = {
                        match internal_struct_type
                            .struct_metadata
                            .struct_name
                            .segments
                            .last()
                            .unwrap()
                        {
                            ModuleSegment::SubModule(identifier) => identifier.name.clone(),
                            ModuleSegment::Single(_) => unreachable!(),
                        }
                    };

                    let new_internal_struct_type = self.build_decl_imported_struct_methods(
                        module_path.clone(),
                        module_id.clone(),
                        internal_struct_type.clone(),
                    );
                    self.struct_table.insert(struct_name, new_internal_struct_type.clone());
                }
                DefinitionLookupResult::Typedef(typedef_metadata) => {
                    self.typedef_table
                        .insert(single.identifier.name, typedef_metadata.clone());
                }
                DefinitionLookupResult::GlobalVariable(mut global_variable_metadata) => {
                    let global_value = self.build_decl_imported_global_variable(
                        global_variable_metadata.clone(),
                        loc.clone(),
                        span_end,
                    );

                    global_variable_metadata.global_value = global_value;
                    self.global_variables_table
                        .insert(single.identifier.name, global_variable_metadata.clone());
                }
            }
        }
    }

    fn build_imported_global_variables(
        &mut self,
        global_variables_table: GlobalVariablesTable<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> HashMap<String, GlobalVariableMetadata<'ctx>> {
        let mut imported_global_variables: HashMap<String, GlobalVariableMetadata> = HashMap::new();
        for (_, global_variable_metadata) in global_variables_table {
            let global_value =
                self.build_decl_imported_global_variable(global_variable_metadata.clone(), loc.clone(), span_end);

            imported_global_variables.insert(
                global_variable_metadata.name.clone(),
                GlobalVariableMetadata {
                    name: global_variable_metadata.name.clone(),
                    variable_type: global_variable_metadata.variable_type.clone(),
                    access_specifier: global_variable_metadata.access_specifier.clone(),
                    global_value,
                },
            );
        }
        imported_global_variables
    }

    fn build_decl_imported_global_variable(
        &self,
        global_variable_metadata: GlobalVariableMetadata<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> GlobalValue<'ctx> {
        let module = self.module.borrow_mut();
        let linkage = Linkage::Common;

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let variable_basic_type = match global_variable_metadata.variable_type.to_basic_type(ptr_type) {
            Ok(basic_type) => basic_type,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(err.to_string()),
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

        let initialzier_basic_value: BasicValueEnum<'ctx> = self
            .build_zero_initialized_internal_value(global_variable_metadata.variable_type, loc.clone(), span_end)
            .to_basic_metadata()
            .as_any_value_enum()
            .try_into()
            .unwrap();

        let global_value = module.add_global(variable_basic_type, None, &global_variable_metadata.name);
        global_value.set_initializer(&initialzier_basic_value);
        global_value.set_linkage(linkage);
        global_value
    }

    fn build_decl_imported_instance_method(
        &mut self,
        metadata: FuncMetadata<'ctx>,
        self_modifier_type: InternalType<'ctx>,
    ) -> (String, FuncMetadata<'ctx>) {
        let param_types = metadata.params_metadata.param_types.clone();

        if metadata.func_decl.access_specifier == AccessSpecifier::Public
            || metadata.func_decl.access_specifier == AccessSpecifier::PublicExtern
            || metadata.func_decl.access_specifier == AccessSpecifier::PublicInline
        {
            let mut new_metadata = metadata.clone();

            let mut params_metadata = self.build_func_params(
                metadata.func_decl.name.clone(),
                metadata.func_decl.loc.clone(),
                metadata.func_decl.span.end,
                metadata.func_decl.params.list.clone(),
                metadata.func_decl.params.variadic.clone(),
            );

            params_metadata.param_types.insert(0, self_modifier_type);

            let is_var_args = metadata.func_decl.params.variadic.is_some();

            let return_type = self.build_type(
                metadata
                    .func_decl
                    .return_type
                    .clone()
                    .unwrap_or(TypeSpecifier::TypeToken(Token {
                        kind: TokenKind::Void,
                        span: Span::default(),
                        loc: Location::default(),
                    })),
                metadata.func_decl.loc.clone(),
                metadata.func_decl.span.end,
            );

            let fn_type = unsafe {
                FunctionType::new(LLVMFunctionType(
                    return_type.as_type_ref(),
                    param_types
                        .iter()
                        .map(|p| p.as_type_ref())
                        .collect::<Vec<LLVMTypeRef>>()
                        .as_mut_ptr(),
                    param_types.len() as u32,
                    is_var_args as i32,
                ))
            };

            let func_linkage = self.build_func_linkage(metadata.func_decl.access_specifier.clone());
            let func_value = self.module.borrow_mut().deref_mut().add_function(
                &metadata.func_decl.name,
                fn_type,
                Some(func_linkage),
            );

            new_metadata.ptr = func_value;
            (new_metadata.func_decl.get_usable_name(), new_metadata)
        } else {
            (metadata.func_decl.get_usable_name(), metadata.clone())
        }
    }

    fn build_decl_imported_func(&mut self, metadata: FuncMetadata<'ctx>) -> (String, FuncMetadata<'ctx>) {
        let param_types = metadata.params_metadata.param_types.clone();

        if metadata.func_decl.access_specifier == AccessSpecifier::Public
            || metadata.func_decl.access_specifier == AccessSpecifier::PublicExtern
            || metadata.func_decl.access_specifier == AccessSpecifier::PublicInline
        {
            let mut new_metadata = metadata.clone();
            let is_variadic = new_metadata.func_decl.params.variadic.is_some();

            let return_type = self.build_type(
                new_metadata
                    .func_decl
                    .return_type
                    .clone()
                    .unwrap_or(TypeSpecifier::TypeToken(Token {
                        kind: TokenKind::Void,
                        span: Span::default(),
                        loc: Location::default(),
                    })),
                new_metadata.func_decl.loc.clone(),
                new_metadata.func_decl.span.end,
            );

            let func_type = unsafe {
                FunctionType::new(LLVMFunctionType(
                    return_type.as_type_ref(),
                    param_types
                        .iter()
                        .map(|p| p.as_type_ref())
                        .collect::<Vec<LLVMTypeRef>>()
                        .as_mut_ptr(),
                    param_types.len() as u32,
                    is_variadic as i32,
                ))
            };

            let func_linkage = self.build_func_linkage(new_metadata.func_decl.access_specifier.clone());

            let func_value = self.module.borrow_mut().deref_mut().add_function(
                &new_metadata.func_decl.name,
                func_type,
                Some(func_linkage),
            );

            new_metadata.ptr = func_value;
            (new_metadata.func_decl.get_usable_name(), new_metadata)
        } else {
            (metadata.func_decl.get_usable_name(), metadata.clone())
        }
    }

    fn build_imported_funcs(&mut self, func_table: FuncTable<'ctx>) -> HashMap<String, FuncMetadata<'ctx>> {
        let mut imported_funcs: HashMap<String, FuncMetadata> = HashMap::new();
        for metadata in func_table.values().cloned() {
            let (func_name, func_metadata) = self.build_decl_imported_func(metadata.clone());
            imported_funcs.insert(func_name, func_metadata);
        }
        imported_funcs
    }

    fn build_decl_imported_struct_methods(
        &mut self,
        imported_from: ModulePath,
        imported_module_id: String,
        internal_struct_type: InternalStructType<'ctx>,
    ) -> InternalStructType<'ctx> {
        let mut final_internal_struct_type = internal_struct_type.clone();

        for (idx, (mut method_decl, method_value, is_static_method)) in internal_struct_type
            .clone()
            .struct_metadata
            .methods
            .iter()
            .cloned()
            .enumerate()
        {
            let return_type = self.build_type(
                method_decl
                    .return_type
                    .clone()
                    .unwrap_or(TypeSpecifier::TypeToken(Token {
                        kind: TokenKind::Void,
                        span: Span::default(),
                        loc: Location::default(),
                    })),
                method_decl.loc.clone(),
                method_decl.span.end,
            );

            let params_metadata = self.build_func_params(
                method_decl.get_usable_name(),
                method_decl.loc.clone(),
                method_decl.span.end,
                method_decl.params.list.clone(),
                method_decl.params.variadic.clone(),
            );

            let struct_name = match internal_struct_type
                .struct_metadata
                .struct_name
                .segments
                .last()
                .unwrap()
            {
                ModuleSegment::SubModule(identifier) => identifier.name.clone(),
                ModuleSegment::Single(_) => unreachable!(),
            };

            method_decl.name = self.generate_method_abi_name(
                imported_module_id.clone(),
                struct_name.clone(),
                method_decl.name.clone(),
            );

            if is_static_method {
                let (_, func_metadata) = self.build_decl_imported_func(FuncMetadata {
                    func_decl: method_decl.clone(),
                    ptr: method_value,
                    is_method: true,
                    imported_from: Some(imported_from.clone()),
                    params_metadata,
                    return_type,
                });

                final_internal_struct_type.struct_metadata.methods[idx] = (
                    func_metadata.func_decl.clone(),
                    func_metadata.ptr,
                    is_static_method.clone(),
                );
            } else {
                let self_modifier = match method_decl.params.list.clone().first().unwrap() {
                    ast::ast::FuncParamKind::SelfModifier(self_modifier) => {
                        method_decl.params.list.remove(0);
                        self_modifier.clone()
                    }
                    ast::ast::FuncParamKind::FuncParam(_) => unreachable!(),
                };

                let (self_modifier_type, _) = self.build_self_modifier_param(
                    &self_modifier,
                    &struct_name,
                    &internal_struct_type,
                    &method_decl.loc,
                    &method_decl.span,
                );

                let (_, mut func_metadata) = self.build_decl_imported_instance_method(
                    FuncMetadata {
                        func_decl: method_decl.clone(),
                        ptr: method_value,
                        is_method: true,
                        imported_from: Some(imported_from.clone()),
                        params_metadata,
                        return_type,
                    },
                    self_modifier_type,
                );

                func_metadata
                    .func_decl
                    .params
                    .list
                    .insert(0, FuncParamKind::SelfModifier(self_modifier));

                final_internal_struct_type.struct_metadata.methods[idx] = (
                    func_metadata.func_decl.clone(),
                    func_metadata.ptr,
                    is_static_method.clone(),
                );
            }
        }

        final_internal_struct_type
    }

    fn build_imported_structs(
        &mut self,
        imported_from: ModulePath,
        imported_module_id: String,
        struct_table: StructTable<'ctx>,
    ) -> HashMap<String, InternalStructType<'ctx>> {
        let mut imported_structs: HashMap<String, InternalStructType> = HashMap::new();
        for (_, (struct_name, internal_struct_type)) in struct_table.iter().enumerate() {
            let new_internal_struct_type = self.build_decl_imported_struct_methods(
                imported_from.clone(),
                imported_module_id.clone(),
                internal_struct_type.clone(),
            );

            imported_structs.insert(struct_name.clone(), new_internal_struct_type);
        }
        imported_structs
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
}
