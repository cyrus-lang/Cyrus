use crate::context::{BlockRegistry, CodeGenLLVM};
use crate::enums::{EnumID, EnumMetadata, EnumTable};
use crate::structs::{StructID, StructMethodMetadata};
use crate::types::InternalType;
use crate::{
    build::BuildManifest,
    diag::*,
    funcs::{FuncMetadata, FuncTable},
    resolver::MetadataResolverResult,
    structs::{StructMetadata, StructTable},
    types::{TypedefMetadata, TypedefTable},
    variables::{GlobalVariableMetadata, GlobalVariablesTable},
};
use ast::{AccessSpecifier, Identifier};
use ast::{
    ast::{AccessSpecifier, Import, ModulePath, ModuleSegment, ModuleSegmentSingle},
    format::module_segments_as_string,
    token::Location,
};
use inkwell::AddressSpace;
use inkwell::module::Linkage;
use inkwell::types::StructType;
use inkwell::values::{AnyValue, BasicValueEnum, FunctionValue, GlobalValue};
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

pub type LocalIRValueID = u64;
pub type LocalIRValueRegistryRef<'a> = Rc<RefCell<LocalIRValueRegistry<'a>>>;
pub type LocalIRValueRegistry<'a> = HashMap<LocalIRValueID, LocalIRValue<'a>>;
pub type ModuleMetadataRegistryRef<'a> = Rc<RefCell<Vec<ModuleMetadata<'a>>>>;

#[derive(Debug, Clone)]
pub enum LocalIRValue<'a> {
    Func(FunctionValue<'a>),
    GlobalValue(GlobalValue<'a>),
}

#[derive(Debug, Clone)]
pub struct ModuleMetadata<'a> {
    pub module_id: ModuleID,
    pub module_file_path: String,
    pub func_table: FuncTable<'a>,
    pub struct_table: StructTable<'a>,
    pub global_variables_table: GlobalVariablesTable<'a>,
    pub enum_table: EnumTable<'a>,
    pub typedef_table: TypedefTable<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct ImportedModule {
    // reference to module metadata registry
    pub module_id: ModuleID,
    pub module_path: ModulePath,
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

    pub(crate) fn get_imported_module(&self, segments: Vec<ModuleSegment>) -> Option<ImportedModule> {
        let equal_segments = |segment1: &ModuleSegment, segment2: &ModuleSegment| {
            segment1.as_identifier().name == segment2.as_identifier().name
        };

        self.imported_modules
            .iter()
            .find(|m| {
                if m.module_path.segments.len() != segments.len() {
                    return false;
                }

                m.module_path
                    .segments
                    .iter()
                    .zip(segments.iter())
                    .all(|(s1, s2)| equal_segments(s1, s2))
            })
            .cloned()
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

    pub(crate) fn get_module_metadata_by_module_id(&self, module_id: ModuleID) -> Option<RefMut<ModuleMetadata<'ctx>>> {
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
            let module_name = self.get_local_module_name(module_path.clone());

            let import_path = self.build_imported_module_path(
                module_path.clone(),
                module_path.segments.clone(),
                import.loc.clone(),
                import.span.end,
            );

            self.error_if_module_already_loaded(import_path.module_path.clone());
            self.build_imported_module(module_name, import_path);
        }
    }


    fn build_module_with_sub_codegen(
        &self,
        module_name: String,
        import_path: ImportedModulePath,
    ) -> (BuildManifest, ModuleID) {
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
            enum_table: EnumTable::new(),
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

    fn build_imported_module(&mut self, module_name: String, mut import_path: ImportedModulePath) {
        let module_metadata = match self.get_module_metadata_by_file_path(import_path.file_path.clone()) {
            Some(module_metadata) => {
                // This module compiled before, let's use the same module_metadata.
                module_metadata
            }
            None => {
                let (build_manifest, module_id) = self.build_module_with_sub_codegen(module_name, import_path.clone());
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
            import_path.module_path.segments.pop();

            self.build_module_import_singles(
                module_metadata.module_id.clone(),
                module_segment_singles,
                import_path.module_path,
                import_path.loc.clone(),
                import_path.span_end,
            );
        } else {
            self.imported_modules.push(ImportedModule {
                module_id: module_metadata.module_id,
                module_path: import_path.module_path.clone(),
            });
        }
    }

    fn error_if_func_is_private(
        &self,
        func_name: String,
        vis: AccessSpecifier,
        loc: Location,
        span_end: usize,
    ) {
        if vis.is_private() {
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

    fn error_if_struct_is_private(
        &self,
        struct_name: String,
        vis: AccessSpecifier,
        loc: Location,
        span_end: usize,
    ) {
        if vis: AccessSpecifier.is_private() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::ImportingPrivateStruct(struct_name),
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

    fn error_if_typedef_is_private(
        &self,
        typedef_name: String,
        vis: AccessSpecifier,
        loc: Location,
        span_end: usize,
    ) {
        if vis: AccessSpecifier.is_private() {
            display_single_diag(Diag {
                level: DiagLevel::Error,
                kind: DiagKind::ImportingPrivateTypedef(typedef_name),
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

    fn build_imported_global_variable(
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
            .internal_value_to_basic_metadata(self.build_zero_initialized_internal_value(
                global_variable_metadata.variable_type,
                loc.clone(),
                span_end,
            ))
            .as_any_value_enum()
            .try_into()
            .unwrap();

        let global_value = module.add_global(variable_basic_type, None, &global_variable_metadata.name);
        global_value.set_initializer(&initialzier_basic_value);
        global_value.set_linkage(linkage);
        global_value
    }

    pub(crate) fn build_module_import_singles(
        &mut self,
        module_id: u64,
        module_segment_singles: Vec<ModuleSegmentSingle>,
        module_path: ModulePath,
        loc: Location,
        span_end: usize,
    ) {
        let import_func = |mut func_metadata: FuncMetadata<'ctx>, renamed: Option<Identifier>| {
            self.error_if_func_is_private(
                func_metadata.func_decl.get_usable_name(),
                func_metadata.func_decl.vis: AccessSpecifier.clone(),
                loc.clone(),
                span_end,
            );

            let func_name = match renamed {
                Some(identifier) => identifier.name,
                None => func_metadata.func_decl.get_usable_name(),
            };

            func_metadata.imported_from = Some(module_id);
            let func_value = self.build_func_decl(
                func_metadata.func_decl.clone(),
                func_metadata.params_metadata.clone(),
                func_metadata.return_type.clone(),
                false,
            );
            let local_ir_value_id = generate_local_ir_value_id();
            self.insert_local_ir_value(local_ir_value_id, LocalIRValue::Func(func_value));
            let mut module_metadata = match self.get_module_metadata_by_module_id(self.module_id) {
                Some(module_metadata) => module_metadata,
                None => panic!("Couldn't lookup module in the module metadata registry."),
            };
            func_metadata.local_ir_value_id = local_ir_value_id;
            module_metadata.insert_func(func_name, func_metadata);
            drop(module_metadata);
        };

        let import_struct = |mut struct_metadata: StructMetadata<'ctx>, renamed: Option<Identifier>| {
            let struct_name = match renamed {
                Some(identifier) => identifier.name,
                None => struct_metadata.struct_name.as_identifier().unwrap().name,
            };

            self.error_if_struct_is_private(
                struct_name.clone(),
                struct_metadata.vis: AccessSpecifier.clone(),
                loc.clone(),
                span_end,
            );

            struct_metadata.imported_from = Some(module_id);

            let mut module_metadata = match self.get_module_metadata_by_module_id(self.module_id) {
                Some(module_metadata) => module_metadata,
                None => panic!("Couldn't lookup module in the module metadata registry."),
            };

            module_metadata.insert_struct(struct_name, struct_metadata);
            drop(module_metadata);
        };

        let import_typedef = |typedef_name: String, typedef_metadata: TypedefMetadata<'ctx>| {
            self.error_if_typedef_is_private(
                typedef_name.clone(),
                typedef_metadata.vis: AccessSpecifier.clone(),
                loc.clone(),
                span_end,
            );

            let mut module_metadata = match self.get_module_metadata_by_module_id(self.module_id) {
                Some(module_metadata) => module_metadata,
                None => panic!("Couldn't lookup module in the module metadata registry."),
            };

            module_metadata.insert_typedef(typedef_name, typedef_metadata);
            drop(module_metadata);
        };

        let import_global_variable = |global_variable_metadata: GlobalVariableMetadata<'ctx>,
                                      renamed: Option<Identifier>| {
            let global_variable_name = match renamed {
                Some(identifier) => identifier.name,
                None => global_variable_metadata.name.clone(),
            };
            let global_value =
                self.build_imported_global_variable(global_variable_metadata.clone(), loc.clone(), span_end);

            self.insert_local_ir_value(
                global_variable_metadata.local_ir_value_id,
                LocalIRValue::GlobalValue(global_value),
            );

            let mut module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
            module_metadata.insert_global_variable(global_variable_name, global_variable_metadata);
            drop(module_metadata);
        };

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
                MetadataResolverResult::Func(func_metadata) => import_func(func_metadata, single.renamed.clone()),
                MetadataResolverResult::Struct(struct_metadata) => {
                    import_struct(struct_metadata, single.renamed.clone())
                }
                MetadataResolverResult::Typedef(typedef_metadata) => {
                    let typedef_identifier = single.renamed.clone().or(Some(single.identifier.clone())).unwrap();
                    import_typedef(typedef_identifier.name, typedef_metadata)
                }
                MetadataResolverResult::GlobalVariable(global_variable_metadata) => {
                    import_global_variable(global_variable_metadata, single.renamed.clone())
                }
                MetadataResolverResult::Enum(enum_metadata) => todo!(),
            }
        });
    }

    pub(crate) fn insert_local_ir_value(&self, id: LocalIRValueID, value: LocalIRValue<'ctx>) {
        let mut local_ir_value_registry = self.local_ir_value_registry.borrow_mut();
        local_ir_value_registry.insert(id, value);
        drop(local_ir_value_registry);
    }

    pub(crate) fn get_or_declare_local_func_ir_value(
        &mut self,
        id: LocalIRValueID,
        func_metadata: FuncMetadata<'ctx>,
    ) -> FunctionValue<'ctx> {
        match self.get_local_func_ir_value(id) {
            Some(func_value) => func_value,
            None => {
                let func_value = self.build_func_decl(
                    func_metadata.func_decl.clone(),
                    func_metadata.params_metadata.clone(),
                    func_metadata.return_type.clone(),
                    false,
                );

                self.insert_local_ir_value(func_metadata.local_ir_value_id, LocalIRValue::Func(func_value));
                func_value
            }
        }
    }

    pub(crate) fn get_local_func_ir_value(&self, id: LocalIRValueID) -> Option<FunctionValue<'ctx>> {
        let local_ir_value_registry = self.local_ir_value_registry.borrow();
        let local_ir_value = match local_ir_value_registry.get(&id).cloned() {
            Some(local_ir_value) => local_ir_value,
            None => return None,
        };

        drop(local_ir_value_registry);
        if let LocalIRValue::Func(func_value) = local_ir_value {
            Some(func_value)
        } else {
            None
        }
    }

    pub(crate) fn get_local_global_value_ir_value(&self, id: LocalIRValueID) -> Option<GlobalValue<'ctx>> {
        let local_ir_value_registry = self.local_ir_value_registry.borrow();
        let local_ir_value = match local_ir_value_registry.get(&id).cloned() {
            Some(local_ir_value) => local_ir_value,
            None => return None,
        };

        drop(local_ir_value_registry);
        if let LocalIRValue::GlobalValue(get_local_global_value_ir_value) = local_ir_value {
            Some(get_local_global_value_ir_value)
        } else {
            None
        }
    }
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

    pub fn insert_enum(&mut self, name: String, metadata: EnumMetadata<'a>) {
        self.enum_table.insert(name, metadata);
    }

    pub fn insert_typedef(&mut self, name: String, metadata: TypedefMetadata<'a>) {
        self.typedef_table.insert(name, metadata);
    }

    pub fn get_defined_type(&self, type_name: String) -> Option<InternalType<'a>> {
        match self.get_struct_metadata_by_name(type_name.clone()) {
            Some(struct_metadata) => Some(InternalType::StructType(struct_metadata.as_internal_struct_type())),
            None => match self.typedef_table.get(&type_name) {
                Some(typedef_metadata) => Some(typedef_metadata.internal_type.clone()),
                None => None,
            },
        }
    }

    pub fn get_enum_metadata_by_id(&mut self, enum_id: EnumID) -> &mut EnumMetadata<'a> {
        self.enum_table
            .iter_mut()
            .find(|r| r.1.enum_id == enum_id)
            .expect("Couldn't find enum from module metadata registry.")
            .1
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

    pub fn get_global_variable_by_name(&self, global_variable_name: String) -> Option<GlobalVariableMetadata<'a>> {
        self.global_variables_table.get(&global_variable_name).cloned()
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
