use crate::{CodeGenLLVM, build::BuildManifest, diag::*, funcs::FuncTable};
use ast::ast::{Field, FuncDecl, Identifier, Import, ModulePath, VisType};
use inkwell::module::Module;
use std::{cell::RefCell, collections::HashMap, process::exit, rc::Rc};
use utils::fs::find_file_from_sources;

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
    pub module: Rc<RefCell<Module<'ctx>>>,
    pub exported_funcs: HashMap<String, ExportedFuncMetadata>,
    pub exported_struct: HashMap<String, ExportedStructMetadata>,
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_import(&mut self, import: Import) {
        let sources = &self.opts.sources_dir;

        if let ModulePath::SubModule(identifier) = import.module_paths[0].clone() {
            if identifier.name == "std" {
                todo!("Import from standard library not implemented yet.");
            }
        }

        let mut imported_files: Vec<String> = Vec::new();

        for module_path in import.module_paths {
            match module_path {
                ModulePath::Wildcard => todo!(),
                ModulePath::SubModule(identifier) => {
                    let file_name = format!("{}.cyr", identifier.name);

                    match find_file_from_sources(file_name, sources.clone()) {
                        Some(file_path) => {
                            imported_files.push(file_path.to_str().unwrap().to_string());
                        }
                        None => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::Custom(format!(
                                    "The module '{}' could not be found in any of the specified source directories.",
                                    identifier.name
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
                    }
                }
            }
        }

        for file_path in imported_files {
            self.build_sub_module(file_path);
        }
    }

    fn build_sub_module(&mut self, file_path: String) {
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

        self.loaded_modules.insert(
            module_name,
            ModuleMetadata {
                module: Rc::clone(&sub_module),
                exported_funcs: self.collect_exported_funcs(sub_codegen.func_table),
                exported_struct: HashMap::new(), // TODO
            },
        );
    }

    fn collect_exported_funcs(&self, func_table: FuncTable) -> HashMap<String, ExportedFuncMetadata> {
        let mut exported_funcs: HashMap<String, ExportedFuncMetadata> = HashMap::new();

        for (_, (func_name, metadata)) in func_table.iter().enumerate() {
            // only pub funcs are exported imported from sub_module 
            if metadata.func_decl.vis_type == VisType::Pub {
                exported_funcs.insert(func_name.to_string(), ExportedFuncMetadata { func_decl: metadata.func_decl.clone() });
            }
        }

        exported_funcs
    }
}
