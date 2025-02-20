use ast::ast::*;
use ast::token::*;
use gccjit_sys::*;
use parser::parse_program;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CString;
use std::path::Path;
use utils::compiler_error;

use crate::funcs::FuncMetadata;
use crate::structs::StructMetadata;
use crate::Compiler;

impl Compiler {
    pub(crate) fn compile_import(&mut self, import: Import) {
        let file_path = self.file_path.clone();
        let current_dir = Path::new(&file_path).parent().unwrap().to_str().unwrap();

        let mut finding_std = false;
        let mut import_file_path = String::from(format!("{}/", current_dir));

        for sub_package in import.sub_packages.clone() {
            let package_name = sub_package.package_name.name.as_str();
            match package_name {
                "std" => {
                    finding_std = true;
                    import_file_path = format!("{}/", self.stdlib_path());
                }
                "./" => {
                    if finding_std {
                        compiler_error!("Importing package with relative path to find stdlib is not allowed.");
                    }
                    import_file_path += "./";
                }
                "../" => {
                    if finding_std {
                        compiler_error!("Importing package with relative path to find stdlib is not allowed.");
                    }
                    import_file_path += "../";
                }
                _ => {
                    let path_str = format!("{}{}", import_file_path, package_name);

                    if Path::new(&format!("{}", path_str)).is_dir() {
                        import_file_path += &format!("{}/", package_name);
                    } else if Path::new(&format!("{}.cyr", path_str)).is_file() {
                        import_file_path += &format!("{}.cyr", package_name);
                        break;
                    } else {
                        compiler_error!(format!(
                            "File '{}' not found to be imported by the compiler.",
                            import_file_path + package_name
                        ));
                    }
                }
            }
        }

        let output_library_path = Path::new(&import_file_path)
            .with_extension(self.object_file_extension())
            .to_string_lossy()
            .to_string();

        let (program, file_name) = parse_program(import_file_path.clone());
        let context = Compiler::new_child_context(self.context);
        let mut compiler = Compiler::new(context, program, import_file_path.clone(), file_name);

        compiler.compile();
        compiler.make_object_file(output_library_path.clone());

        let package_name = sub_packages_as_string(import.sub_packages.clone());

        self.compiled_object_files.push(output_library_path.clone());

        self.import_package(compiler, package_name, import);

        let optname = CString::new(output_library_path).unwrap();
        unsafe { gcc_jit_context_add_driver_option(self.context, optname.as_ptr()) };
    }

    fn import_package(&mut self, compiler: Compiler, package_name: String, import: Import) {
        // Import functions
        for (key, value) in compiler.func_table.borrow_mut().clone() {
            if value.func_type == VisType::Pub && !self.func_table.borrow_mut().contains_key(&key) {
                let func_ptr = self.define_imported_func_as_extern_decl(
                    key.clone(),
                    value.params.clone(),
                    Some(Token {
                        kind: value.return_type.clone(),
                        span: Span::default(),
                    }),
                    import.loc.clone(),
                );

                self.func_table.borrow_mut().insert(
                    key,
                    FuncMetadata {
                        func_type: VisType::Extern,
                        ptr: func_ptr,
                        params: value.params,
                        return_type: value.return_type,
                        imported_from: Some(package_name.clone()),
                    },
                );
            }
        }

        // Import structs
        for (key, value) in compiler.global_struct_table.borrow_mut().clone() {
            if value.vis_type == VisType::Pub && !self.global_struct_table.borrow_mut().contains_key(&key) {
                let mut struct_field_ptrs = self.compile_struct_fields(value.fields.clone());
                let struct_decl_name = CString::new(key.clone()).unwrap();
                let struct_decl = unsafe {
                    gcc_jit_context_new_struct_type(
                        self.context,
                        self.gccjit_location(import.loc.clone()),
                        struct_decl_name.as_ptr(),
                        value.fields.len().try_into().unwrap(),
                        struct_field_ptrs.as_mut_ptr(),
                    )
                };

                self.global_struct_table.borrow_mut().insert(
                    key.clone(),
                    StructMetadata {
                        vis_type: VisType::Internal,
                        struct_type: struct_decl,
                        fields: value.fields.clone(),
                        field_ptrs: struct_field_ptrs.clone(),
                        methods: Vec::new(),
                        method_ptrs: Vec::new(),
                        imported_from: Some(package_name.clone()),
                    },
                );

                let mut methods_decl: Vec<*mut gcc_jit_function> = Vec::new();
                for item in value.methods.clone() {
                    let method_ptr = self.define_imported_func_as_extern_decl(
                        self.make_struct_method_name(key.clone(), item.func_def.name),
                        item.func_def.params,
                        item.func_def.return_type,
                        item.func_def.loc,
                    );
                    methods_decl.push(method_ptr);
                }

                self.global_struct_table.borrow_mut().insert(
                    key,
                    StructMetadata {
                        vis_type: VisType::Internal,
                        struct_type: struct_decl,
                        fields: value.fields,
                        field_ptrs: struct_field_ptrs,
                        methods: value.methods,
                        method_ptrs: methods_decl,
                        imported_from: Some(package_name.clone()),
                    },
                );
            }
        }
    }

    fn define_imported_func_as_extern_decl(
        &mut self,
        func_name: String,
        params: FunctionParams,
        return_type: Option<Token>,
        loc: Location,
    ) -> *mut gcc_jit_function {
        let return_type = return_type
            .unwrap_or(Token {
                kind: TokenKind::Void,
                span: Span::default(),
            })
            .kind;

        let mut func_params = self.compile_func_params(func_name.clone(), params.list.clone(), loc.clone());
        let func_name_cstr = CString::new(func_name.clone()).unwrap();
        let decl_func = unsafe {
            gcc_jit_context_new_function(
                self.context,
                self.gccjit_location(loc.clone()),
                gcc_jit_function_kind::GCC_JIT_FUNCTION_IMPORTED,
                self.token_as_data_type(self.context, return_type.clone()),
                func_name_cstr.as_ptr(),
                func_params.len().try_into().unwrap(),
                func_params.as_mut_ptr(),
                self.cbool(params.is_variadic),
            )
        };

        decl_func
    }

    pub(crate) fn object_file_extension(&self) -> &'static str {
        "o"
    }
}
