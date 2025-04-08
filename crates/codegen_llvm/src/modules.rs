use crate::{CodeGenLLVM, build::BuildManifest, diag::*};
use ast::ast::{Field, FuncDecl, Identifier, Import, ModulePath, VisType};
use inkwell::module::Module;
use std::{collections::HashMap, process::exit};
use utils::fs::find_file_from_sources;

pub struct ExportedFuncMetadata {
    pub func_decl: FuncDecl,
}

pub struct ExportedStructMetadata {
    pub name: String,
    pub vis_type: VisType,
    pub inherits: Vec<Identifier>,
    pub fields: Vec<Field>,
    pub methods: Vec<FuncDecl>,
}

pub struct ModuleMetadata<'ctx> {
    pub module: Module<'ctx>,
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
        let sub_module = self.context.create_module(&module_name);
        let sub_builder = self.context.create_builder();
        let target_machine = CodeGenLLVM::target_machine(&sub_module);
        let program = parser::parse_program(file_path.clone()).0;
        
        let mut sub_codegen = CodeGenLLVM {
            program,
            opts: self.opts.clone(),
            context: self.context,
            module: sub_module,
            builder: sub_builder,
            target_machine,
            build_manifest: self.build_manifest.clone(),
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
        dbg!(sub_codegen.build_manifest.clone());
        dbg!(self.build_manifest.clone());
        // self.build_manifest = sub_codegen.build_manifest.clone();
    }
}
