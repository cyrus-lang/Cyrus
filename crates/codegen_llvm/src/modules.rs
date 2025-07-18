use crate::{
    CodeGenLLVM,
    build::BuildManifest,
    diag::*,
    funcs::{FuncMetadata, FuncTable},
    structs::{StructMethodMetadata, StructTable},
    types::{InternalStructType, InternalType, TypedefMetadata, TypedefTable},
    variables::{GlobalVariableMetadata, GlobalVariablesTable},
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
    pub imports_single: bool,
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
        &self,
        module_path: ModulePath,
        mut segments: Vec<ModuleSegment>,
        loc: Location,
        span_end: usize,
    ) -> GeneratedModuleImportPath {
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
         -> GeneratedModuleImportPath {
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
                self.build_import_module_path(module_path.clone(), segments[0..idx].to_vec(), loc.clone(), span_end);

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

            GeneratedModuleImportPath {
                file_path: base_path.file_path,
                module_path: module_path.clone(),
                singles: Some(module_segment_singles.clone()),
                loc: loc.clone(),
                span_end,
            }
        };

        let lookup_file = |idx: usize, file_path: String| -> Option<GeneratedModuleImportPath> {
            if let Some(found_path) = find_file_from_sources(file_path, sources.clone()) {
                if idx == segments.len() - 1 {
                    // last segment is a file
                    Some(GeneratedModuleImportPath {
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
                imports_single: true,
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
                imports_single: false,
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
            None => panic!("Couldn't lookup imported module in codegen context."),
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

            new_metadata.params_metadata.param_types.insert(0, self_modifier_type);

            let is_var_args = metadata.func_decl.params.variadic.is_some();

            let fn_type = unsafe {
                FunctionType::new(LLVMFunctionType(
                    new_metadata.return_type.as_type_ref(),
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

            let func_type = unsafe {
                FunctionType::new(LLVMFunctionType(
                    metadata.return_type.as_type_ref(),
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

        for (idx, mut struct_method_metadata) in internal_struct_type
            .clone()
            .struct_metadata
            .methods
            .iter()
            .cloned()
            .enumerate()
        {
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

            struct_method_metadata.method_decl.name = self.generate_method_abi_name(
                imported_module_id.clone(),
                struct_name.clone(),
                struct_method_metadata.method_decl.name.clone(),
            );

            if struct_method_metadata.is_static_method {
                let (_, func_metadata) = self.build_decl_imported_func(FuncMetadata {
                    func_decl: struct_method_metadata.method_decl.clone(),
                    ptr: struct_method_metadata.method_value,
                    is_method: true,
                    imported_from: Some(imported_from.clone()),
                    params_metadata: struct_method_metadata.method_params_metadata.clone(),
                    return_type: struct_method_metadata.return_type.clone(),
                });

                final_internal_struct_type.struct_metadata.methods[idx] = StructMethodMetadata {
                    method_decl: func_metadata.func_decl.clone(),
                    method_value: func_metadata.ptr,
                    method_params_metadata: struct_method_metadata.method_params_metadata,
                    is_static_method: struct_method_metadata.is_static_method.clone(),
                    return_type: struct_method_metadata.return_type.clone(),
                    self_modifier_type: struct_method_metadata.self_modifier_type.clone(),
                };
            } else {
                let (self_modifier_type, self_modifier_kind) =
                    struct_method_metadata.self_modifier_type.clone().unwrap();

                let (_, mut func_metadata) = self.build_decl_imported_instance_method(
                    FuncMetadata {
                        func_decl: struct_method_metadata.method_decl.clone(),
                        ptr: struct_method_metadata.method_value,
                        is_method: true,
                        imported_from: Some(imported_from.clone()),
                        params_metadata: struct_method_metadata.method_params_metadata.clone(),
                        return_type: struct_method_metadata.return_type.clone(),
                    },
                    self_modifier_type,
                );

                func_metadata
                    .func_decl
                    .params
                    .list
                    .insert(0, FuncParamKind::SelfModifier(self_modifier_kind));

                final_internal_struct_type.struct_metadata.methods[idx] = StructMethodMetadata {
                    method_decl: func_metadata.func_decl,
                    method_value: func_metadata.ptr,
                    method_params_metadata: struct_method_metadata.method_params_metadata,
                    is_static_method: struct_method_metadata.is_static_method,
                    return_type: struct_method_metadata.return_type,
                    self_modifier_type: struct_method_metadata.self_modifier_type.clone(),
                };
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
