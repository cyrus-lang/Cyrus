use ast::{
    ast::*,
    token::{Location, Span, Token, TokenKind},
};
use builtins::macros::retrieve_builtin_func;
use clap::ValueEnum;
use gccjit_sys::*;
use options::CompilerOptions;
use parser::parse_program;
use rand::{distributions::Alphanumeric, Rng};
use scope::{IdentifierMetadata, Scope, ScopeRef};
use std::{
    cell::RefCell,
    collections::HashMap,
    ffi::CString,
    fs::remove_file,
    path::Path,
    ptr::null_mut,
    rc::Rc,
    sync::{Arc, Mutex},
};
use utils::compiler_error;

mod builtins;
mod location;
pub mod options;
mod output;
mod scope;
mod types;

// Tracks the current GCC JIT block and function being compiled.
struct BlockFuncPair {
    block: Option<*mut gcc_jit_block>,
    func: Option<*mut gcc_jit_function>,
}

#[derive(Debug, Clone)]
struct FuncParamRecord {
    param_index: i32,
    param_name: String,
}

type FuncParamsRecords = Vec<FuncParamRecord>;

#[derive(Debug, Clone)]
struct LoopBlockPair {
    loop_end: *mut gcc_jit_block,
    increment_block: *mut gcc_jit_block,
}

#[derive(Debug, Clone)]
struct FuncMetadata {
    func_type: VisType,
    ptr: *mut gcc_jit_function,
    return_type: TokenKind,
    params: Vec<FunctionParam>,
}

#[derive(Debug, Clone)]
struct StructMethodMetadata {
    is_static: bool,
    func_def: FuncDef,
}

#[derive(Debug, Clone)]
struct StructMetadata {
    vis_type: VisType,
    struct_type: *mut gcc_jit_struct,
    fields: Vec<Field>,
    field_ptrs: Vec<*mut gcc_jit_field>,
    methods: Vec<StructMethodMetadata>,
    method_ptrs: Vec<*mut gcc_jit_function>,
}

pub struct Compiler {
    file_name: String,
    file_path: String,
    program: Program,
    context: *mut gcc_jit_context,
    func_table: RefCell<HashMap<String, FuncMetadata>>,
    global_struct_table: RefCell<HashMap<String, StructMetadata>>,
    global_vars_table: RefCell<HashMap<String, *mut gcc_jit_lvalue>>,
    param_table: RefCell<HashMap<*mut gcc_jit_function, FuncParamsRecords>>,
    block_func_ref: Arc<Mutex<Box<BlockFuncPair>>>,
    terminated_blocks: Vec<*mut gcc_jit_block>,
    parent_block: Option<*mut gcc_jit_block>,
    active_loop: Option<LoopBlockPair>,
    compiled_object_files: Vec<String>,
    opts: CompilerOptions,
}

impl Compiler {
    pub fn new_block_name(&mut self) -> String {
        let rng = rand::thread_rng();
        let rand_string: String = rng.sample_iter(&Alphanumeric).take(10).map(char::from).collect();
        rand_string
    }

    pub fn set_opts(&mut self, opts: CompilerOptions) {
        self.opts = opts.clone();

        for item in opts.library_path {
            let optname = CString::new(format!("-L{}", item)).unwrap();
            unsafe { gcc_jit_context_add_driver_option(self.context, optname.as_ptr()) };
        }

        for item in opts.libraries {
            let optname = CString::new(format!("-l{}", item)).unwrap();
            unsafe { gcc_jit_context_add_driver_option(self.context, optname.as_ptr()) };
        }

        let optname = CString::new(format!("-lm")).unwrap();
        unsafe { gcc_jit_context_add_driver_option(self.context, optname.as_ptr()) };
    }

    pub fn new_master_context() -> *mut gcc_jit_context {
        unsafe { gcc_jit_context_acquire() }
    }

    pub fn new_child_context(master: *mut gcc_jit_context) -> *mut gcc_jit_context {
        unsafe { gcc_jit_context_new_child_context(master) }
    }

    pub fn new(context: *mut gcc_jit_context, program: Program, file_path: String, file_name: String) -> Self {
        let file_name_cstr = CString::new(file_name.clone()).unwrap();
        unsafe {
            gcc_jit_context_set_str_option(
                context,
                gcc_jit_str_option::GCC_JIT_STR_OPTION_PROGNAME,
                file_name_cstr.as_ptr(),
            );
        }

        unsafe { gcc_jit_context_set_bool_allow_unreachable_blocks(context, 1) };

        Self {
            program,
            context,
            func_table: RefCell::new(HashMap::new()),
            global_struct_table: RefCell::new(HashMap::new()),
            global_vars_table: RefCell::new(HashMap::new()),
            param_table: RefCell::new(HashMap::new()),
            block_func_ref: Arc::new(Mutex::new(Box::new(BlockFuncPair {
                block: None,
                func: None,
            }))),
            terminated_blocks: Vec::new(),
            parent_block: None,
            active_loop: None,
            file_name,
            file_path,
            compiled_object_files: Vec::new(),
            opts: CompilerOptions::default(),
        }
    }

    pub fn compile(&mut self) {
        let scope = Rc::new(RefCell::new(Scope::new()));
        self.compile_statements(scope, self.program.body.clone());
    }

    pub fn compile_statements(&mut self, scope: ScopeRef, stmts: Vec<Statement>) {
        for stmt in stmts {
            self.compile_statement(Rc::clone(&scope), stmt.clone());
        }
    }

    pub fn compile_statement(&mut self, scope: ScopeRef, stmt: Statement) {
        match stmt {
            Statement::Variable(variable) => self.compile_variable(scope, variable.clone()),
            Statement::Expression(expr) => {
                self.compile_expression(scope, expr.clone());
            }
            Statement::FuncDef(func_def) => {
                let ptr = self.compile_func_def(scope, func_def.clone());
                let return_type = self.safe_func_return_type(func_def.return_type);
                self.func_table.borrow_mut().insert(
                    func_def.name,
                    FuncMetadata {
                        func_type: func_def.vis_type,
                        ptr,
                        params: func_def.params,
                        return_type,
                    },
                );
            }
            Statement::FuncDecl(function) => self.compile_func_decl(function.clone()),
            Statement::If(statement) => self.compile_if_statement(scope, statement),
            Statement::For(statement) => self.compile_for_statement(scope, statement),
            Statement::Match(_) => todo!(),
            Statement::Struct(statement) => self.compile_struct(Rc::clone(&scope), statement),
            Statement::Import(statement) => self.compile_import(statement),
            Statement::Return(statement) => self.compile_return(scope, statement),
            Statement::Break(loc) => self.compile_break_statement(loc),
            Statement::Continue(loc) => self.compile_continue_statement(loc),
            Statement::BlockStatement(statement) => self.compile_statements(
                Rc::new(RefCell::new(scope.borrow_mut().clone_immutable())),
                statement.body,
            ),
        }
    }

    fn safe_func_return_type(&mut self, return_type: Option<Token>) ->  TokenKind {
        return_type.unwrap_or(Token { kind: TokenKind::Void, span: Span::new_empty_span() }).kind
    }

    fn find_struct(&mut self, rvalue: *mut gcc_jit_rvalue) -> Option<(String, StructMetadata)> {
        let guard = self.global_struct_table.borrow();
        let founded_struct = {
            match guard
                .iter()
                .find(|&key| unsafe { gcc_jit_struct_as_type(key.1.struct_type) == gcc_jit_rvalue_get_type(rvalue) })
            {
                Some(founded_struct) => founded_struct,
                None => return None,
            }
        };
        Some((founded_struct.0.clone(), founded_struct.1.clone()))
    }

    fn compile_struct_method_call(
        &mut self,
        block: *mut gcc_jit_block,
        struct_name: String,
        struct_metadata: StructMetadata,
        method_name: String,
        mut arguments: Vec<*mut gcc_jit_rvalue>,
    ) -> *mut gcc_jit_rvalue {
        match struct_metadata
            .methods
            .iter()
            .position(|key| key.func_def.name == method_name.clone())
        {
            Some(method_idx) => {
                let func_def = struct_metadata.methods[method_idx].clone();
                let func_ptr = struct_metadata.method_ptrs[method_idx];

                let rvalue = unsafe {
                    gcc_jit_context_new_call(
                        self.context,
                        self.gccjit_location(func_def.func_def.loc.clone()),
                        func_ptr,
                        arguments.len().try_into().unwrap(),
                        arguments.as_mut_ptr(),
                    )
                };

                unsafe { gcc_jit_block_add_eval(block, self.gccjit_location(func_def.func_def.loc), rvalue) };

                return rvalue;
            }
            None => compiler_error!(format!(
                "Method '{}' not defined for struct '{}'",
                method_name, struct_name
            )),
        }
    }

    fn get_struct_method_def(
        &mut self,
        methods: Vec<StructMethodMetadata>,
        struct_name: String,
        method_name: String,
    ) -> FuncDef {
        if let Some(method_def) = methods.iter().find(|&key| key.func_def.name == method_name) {
            method_def.func_def.clone()
        } else {
            compiler_error!(format!(
                "Method definition not found for method '{}' of struct '{}'.",
                method_name, struct_name
            ))
        }
    }

    fn compile_struct_field_access(
        &mut self,
        scope: ScopeRef,
        mut statement: StructFieldAccess,
    ) -> *mut gcc_jit_rvalue {
        let mut method_call_chain = statement.chains.clone();

        let (func, block) = {
            let guard = self.block_func_ref.lock().unwrap();
            (guard.func, guard.block)
        };

        if let (Some(func), Some(block)) = (func, block) {
            let mut result: *mut gcc_jit_rvalue = null_mut();

            if let Expression::Identifier(identifier) = statement.expr.clone() {
                if self.is_user_defined_type(identifier.clone()) {
                    let struct_metadata = self
                        .global_struct_table
                        .borrow_mut()
                        .get(&identifier.name.clone())
                        .unwrap()
                        .clone();

                    if statement.chains.len() > 0 {
                        let item = statement.chains[0].clone();

                        if let Some(method_call) = item.method_call {
                            let method_def = self.get_struct_method_def(
                                struct_metadata.methods.clone(),
                                identifier.name.clone(),
                                method_call.function_name.name.clone(),
                            );

                            let arguments = {
                                // Isolate the mutable borrow to avoid conflict with immutable borrows.
                                self.compile_func_arguments(
                                    Rc::clone(&scope),
                                    Some(method_def.params),
                                    method_call.arguments,
                                )
                            };

                            result = self.compile_struct_method_call(
                                block,
                                identifier.name.clone(),
                                struct_metadata.clone(),
                                method_call.function_name.name,
                                arguments,
                            );

                            // consume current called method from the chain
                            method_call_chain.remove(0);
                        } else {
                            compiler_error!("Accessing static field not supported in cyrus lang.")
                        }
                    }
                } else {
                    result = self.compile_expression(Rc::clone(&scope), statement.expr.clone());
                }
            } else {
                result = self.compile_expression(Rc::clone(&scope), statement.expr.clone());
                dbg!(result);
            }

            if result == null_mut() {
                compiler_error!("Unexpected behaviour in struct field access compilation.");
            }

            for item in method_call_chain {
                unsafe { gcc_jit_type_is_struct(gcc_jit_rvalue_get_type(result)) }; // check to be struct

                if let Some(method_call) = item.method_call {
                    if let Some((struct_name, struct_metadata)) = self.find_struct(result) {
                        let method_def = self.get_struct_method_def(
                            struct_metadata.methods.clone(),
                            struct_name.clone(),
                            method_call.function_name.name.clone(),
                        );

                        // Inserting self argument
                        let mut arguments = self.compile_func_arguments(
                            Rc::clone(&scope),
                            Some(method_def.params.clone()),
                            method_call.arguments,
                        );
                        let self_param = method_def
                            .params
                            .iter()
                            .find(|&key| key.identifier.name == "self")
                            .unwrap();

                        let self_arg = {
                            match self_param.ty.clone().unwrap() {
                                TokenKind::UserDefinedType(_) => result,
                                TokenKind::Dereference(_) => unsafe {
                                    // This means it's a pointer to this type so we need to pass value by address
                                    let result_type = gcc_jit_rvalue_get_type(result);
                                    let temp_name = CString::new(format!("temp__{}", self.new_block_name())).unwrap();
                                    let lvalue = gcc_jit_function_new_local(
                                        func,
                                        self.gccjit_location(method_call.loc),
                                        result_type,
                                        temp_name.as_ptr(),
                                    );
                                    gcc_jit_block_add_assignment(block, null_mut(), lvalue, result);
                                    gcc_jit_lvalue_get_address(lvalue, null_mut())
                                },
                                _ => compiler_error!(format!("Invalid param type.")),
                            }
                        };
                        arguments.insert(0, self_arg);

                        result = self.compile_struct_method_call(
                            block,
                            struct_name.clone(),
                            struct_metadata.clone(),
                            method_call.function_name.name,
                            arguments,
                        );
                    }
                } else {
                    if let Some(field_access) = item.field_access {
                        if let Some((struct_name, struct_metadata)) = self.find_struct(result) {
                            match struct_metadata
                                .fields
                                .iter()
                                .position(|key| key.name == field_access.identifier.name)
                            {
                                Some(field_idx) => {
                                    let field_ptr = unsafe {
                                        gcc_jit_struct_get_field(
                                            struct_metadata.struct_type,
                                            field_idx.try_into().unwrap(),
                                        )
                                    };

                                    let field_rvalue =
                                        unsafe { gcc_jit_rvalue_access_field(result, null_mut(), field_ptr) };

                                    result = field_rvalue;
                                }
                                None => compiler_error!(format!(
                                    "Field '{}' not defined for struct '{}'",
                                    field_access.identifier.name, struct_name
                                )),
                            }
                        } else {
                            compiler_error!("Trying to find struct with rvalue gained from method call chain but could not find anything.")
                        }
                    }
                }
            }

            return result;
        } else {
            compiler_error!("Method call is only valid inside a block");
        }
    }

    fn compile_struct_init(&mut self, scope: ScopeRef, struct_init: StructInit) -> *mut gcc_jit_rvalue {
        let mut struct_metadata = {
            let struct_table = self.global_struct_table.borrow();
            match struct_table.get(&struct_init.name) {
                Some(struct_statement) => struct_statement.clone(),
                None => compiler_error!(format!("Undefined object '{}'", struct_init.name)),
            }
        };

        let struct_type = unsafe { gcc_jit_struct_as_type(struct_metadata.struct_type.clone()) };
        let mut values: Vec<*mut gcc_jit_rvalue> = Vec::new();

        for field in struct_metadata.fields.clone() {
            match struct_init.field_inits.iter().find(|&item| item.name == field.name) {
                Some(field_init) => {
                    let expr = self.compile_expression(Rc::clone(&scope), field_init.value.clone());
                    values.push(expr);
                }
                None => compiler_error!(format!(
                    "Field '{}' required to be set in struct '{}'",
                    field.name, struct_init.name
                )),
            }
        }

        unsafe {
            gcc_jit_context_new_struct_constructor(
                self.context,
                self.gccjit_location(struct_init.loc),
                struct_type,
                values.len().try_into().unwrap(),
                struct_metadata.field_ptrs.as_mut_ptr(),
                values.as_mut_ptr(),
            )
        }
    }

    fn compile_struct_fields(&mut self, fields: Vec<Field>) -> Vec<*mut gcc_jit_field> {
        let mut final_fields: Vec<*mut gcc_jit_field> = Vec::new();
        for item in fields {
            let field_name = CString::new(item.name).unwrap();
            let field_type = self.token_as_data_type(self.context, item.ty);
            let field = unsafe {
                gcc_jit_context_new_field(
                    self.context,
                    self.gccjit_location(item.loc),
                    field_type,
                    field_name.as_ptr(),
                )
            };
            final_fields.push(field);
        }
        final_fields
    }

    fn compile_struct(&mut self, scope: ScopeRef, statement: Struct) {
        let mut field_ptrs = self.compile_struct_fields(statement.fields.clone());

        let num_fields = statement.fields.len().try_into().unwrap();
        let struct_name = CString::new(statement.name.clone()).unwrap();
        let struct_type = unsafe {
            gcc_jit_context_new_struct_type(
                self.context,
                self.gccjit_location(statement.loc),
                struct_name.as_ptr(),
                num_fields,
                field_ptrs.as_mut_ptr(),
            )
        };

        self.global_struct_table.borrow_mut().insert(
            statement.name.clone(),
            StructMetadata {
                vis_type: statement.vis_type.clone(),
                struct_type,
                fields: statement.fields.clone(),
                field_ptrs: field_ptrs.clone(),
                methods: Vec::new(),
                method_ptrs: Vec::new(),
            },
        );

        let (methods, method_ptrs) =
            self.compile_struct_methods(Rc::clone(&scope), statement.name.clone(), statement.methods.clone());

        let struct_metadata = StructMetadata {
            vis_type: statement.vis_type,
            struct_type,
            fields: statement.fields,
            methods,
            field_ptrs,
            method_ptrs,
        };

        self.global_struct_table
            .borrow_mut()
            .insert(statement.name, struct_metadata);
    }

    fn compile_struct_methods(
        &mut self,
        scope: ScopeRef,
        struct_name: String,
        methods: Vec<FuncDef>,
    ) -> (Vec<StructMethodMetadata>, Vec<*mut gcc_jit_function>) {
        let mut method_metadatas: Vec<StructMethodMetadata> = Vec::new();
        let mut method_ptrs: Vec<*mut gcc_jit_function> = Vec::new();

        for item in methods.clone() {
            let mut is_static = true;

            if let Some(self_param) = item.params.iter().find(|&key| key.identifier.name == "self") {
                is_static = false;

                if !self.struct_self_param_valid(struct_name.clone(), self_param.ty.clone().unwrap()) {
                    compiler_error!(format!(
                        "Invalid self parameter in method '{}' of struct '{}'.

The self parameter must either be of the struct's type or a pointer to it.
Valid method definitions should look like:

struct {} {{
    fn do_something(self: {}) {{ /* valid */ }}
    fn do_another_thing(self: *{}) {{ /* valid */ }}
}}

Please ensure that the self parameter follows one of these forms.
                        ",
                        item.name.clone(),
                        struct_name.clone(),
                        struct_name.clone(),
                        struct_name.clone(),
                        struct_name.clone()
                    ));
                }
            } else {
                match item.vis_type.clone() {
                    VisType::Internal => {
                        compiler_error!(format!(
                            "Static method '{}' must be defined as pub fn.",
                            item.name.clone(),
                        ));
                    }
                    _ => {}
                }
            }

            match item.vis_type.clone() {
                VisType::Extern | VisType::Inline => {
                    compiler_error!(format!(
                        "Extern/Inline func definition is not allowed as a method for struct '{}'",
                        struct_name.clone()
                    ));
                }

                _ => {}
            }

            method_metadatas.push(StructMethodMetadata {
                is_static,
                func_def: item.clone(),
            });
            
            let method_ptr = self.compile_func_def(
                Rc::clone(&scope),
                FuncDef {
                    name: self.make_struct_method_name(struct_name.clone(), item.name),
                    params: item.params,
                    body: item.body,
                    return_type: item.return_type,
                    vis_type: item.vis_type,
                    span: item.span,
                    loc: item.loc,
                },
            );

            method_ptrs.push(method_ptr);
        }

        return (method_metadatas, method_ptrs);
    }

    /// Validates whether the given self parameter type is a valid reference to the struct.
    ///
    /// # Parameters
    /// - `struct_name`: The name of the struct the method belongs to.
    /// - `self_param_type`: The type of the `self` parameter in the method signature.
    ///
    /// # Returns
    /// - `true` if the self parameter is either the struct itself or a pointer to it.
    /// - `false` otherwise.
    ///
    fn struct_self_param_valid(&mut self, struct_name: String, self_param_type: TokenKind) -> bool {
        match self_param_type.clone() {
            TokenKind::UserDefinedType(identifier) => {
                if identifier.name == struct_name.clone() {
                    true
                } else {
                    false
                }
            }
            TokenKind::Dereference(token_kind) => {
                return self.struct_self_param_valid(struct_name, *token_kind);
            }
            _ => false,
        }
    }

    fn make_struct_method_name(&self, struct_name: String, method_name: String) -> String {
        format!("__struct__{}__{}", struct_name, method_name)
    }

    fn object_file_extension(&self) -> &'static str {
        "o"
    }

    fn compile_import(&mut self, import: Import) {
        let file_path = self.file_path.clone();
        let dir_path = Path::new(&file_path).parent().unwrap().to_str().unwrap();

        for sb in import.sub_packages {
            let package_file_name = if sb.is_relative {
                sb.package_name.name.clone()
            } else {
                format!("{}.cy", sb.package_name.name)
            };

            let package_file_path = if sb.is_relative {
                sb.package_name.name.clone()
            } else {
                format!("{}/{}", dir_path, package_file_name)
            };

            let library_path = if sb.is_relative {
                Path::new(&sb.package_name.name)
                    .with_extension(self.object_file_extension())
                    .to_string_lossy()
                    .to_string()
            } else {
                format!("{}/{}.{}", dir_path, sb.package_name.name, self.object_file_extension())
            };

            let (program, file_name) = parse_program(package_file_path.clone());
            let context = Compiler::new_child_context(self.context);
            let mut compiler = Compiler::new(context, program, package_file_path.clone(), file_name);

            compiler.compile();
            compiler.make_object_file(library_path.clone());
            
            for (key, value) in compiler.func_table.borrow_mut().clone() {
                if value.func_type == VisType::Pub && !self.func_table.borrow_mut().contains_key(&key) {
                    let func_ptr = self.define_imported_func_as_extern_decl(
                        key.clone(),
                        value.params.clone(),
                        Some(Token {
                            kind: value.return_type.clone(),
                            span: Span::new_empty_span(),
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
                        },
                    );
                }
            }

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

                    self.global_struct_table.borrow_mut().insert(key, StructMetadata {
                        vis_type: VisType::Internal,
                        struct_type: struct_decl,
                        fields: value.fields,
                        field_ptrs: struct_field_ptrs,
                        methods: value.methods,
                        method_ptrs: methods_decl,
                    });
                }
            }

            self.compiled_object_files.push(library_path.clone());

            let optname = CString::new(library_path).unwrap();
            unsafe { gcc_jit_context_add_driver_option(self.context, optname.as_ptr()) };
        }
    }

    fn compile_func_params(
        &mut self,
        func_name: String,
        params: Vec<FunctionParam>,
        loc: Location,
    ) -> Vec<*mut gcc_jit_param> {
        let mut func_params: Vec<*mut gcc_jit_param> = Vec::new();
        for item in params.clone() {
            let param_name = CString::new(item.identifier.name.clone()).unwrap();
            func_params.push(unsafe {
                gcc_jit_context_new_param(
                    self.context,
                    self.gccjit_location(loc.clone()),
                    self.token_as_data_type(
                        self.context,
                        item.ty.expect(&format!(
                            "Function '{}' has an untyped param '{}' that is invalid.",
                            func_name,
                            item.identifier.name.clone()
                        )),
                    ),
                    param_name.as_ptr(),
                )
            });
        }
        func_params
    }

    fn define_imported_func_as_extern_decl(
        &mut self,
        func_name: String,
        params: Vec<FunctionParam>,
        return_type: Option<Token>,
        loc: Location,
    ) -> *mut gcc_jit_function {
        let return_type = return_type
            .unwrap_or(Token {
                kind: TokenKind::Void,
                span: Span::new_empty_span(),
            })
            .kind;

        let mut func_params = self.compile_func_params(func_name.clone(), params.clone(), loc.clone());
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
                0, // FIXME Variadic
            )
        };

        decl_func
    }

    fn compile_func_def(&mut self, scope: ScopeRef, func_def: FuncDef) -> *mut gcc_jit_function {
        let func_type = match func_def.vis_type {
            VisType::Extern => gcc_jit_function_kind::GCC_JIT_FUNCTION_IMPORTED, // imported function
            VisType::Pub => gcc_jit_function_kind::GCC_JIT_FUNCTION_EXPORTED,
            VisType::Internal => gcc_jit_function_kind::GCC_JIT_FUNCTION_INTERNAL,
            VisType::Inline => gcc_jit_function_kind::GCC_JIT_FUNCTION_ALWAYS_INLINE,
        };

        let return_type_token = func_def
            .return_type
            .clone()
            .unwrap_or(Token {
                kind: TokenKind::Void,
                span: Span::new_empty_span(),
            })
            .kind;

        let return_type = self.token_as_data_type(self.context, return_type_token.clone());

        let mut params: Vec<*mut gcc_jit_param> = Vec::new();
        let mut func_params = FuncParamsRecords::new();

        for (idx, func_def_param) in func_def.params.iter().enumerate() {
            let name = CString::new(func_def_param.identifier.name.clone()).unwrap();

            let ty_token = if let Some(user_def) = &func_def_param.ty {
                user_def
            } else {
                &TokenKind::Void
            };

            let ty = self.token_as_data_type(self.context, ty_token.clone());

            let param = unsafe {
                gcc_jit_context_new_param(
                    self.context,
                    self.gccjit_location(func_def_param.loc.clone()),
                    ty,
                    name.as_ptr(),
                )
            };

            params.push(param);

            func_params.push(FuncParamRecord {
                param_index: idx as i32,
                param_name: func_def_param.identifier.name.clone(),
            });
        }

        let func_name = CString::new(func_def.name.clone()).unwrap();
        let func = unsafe {
            gcc_jit_context_new_function(
                self.context,
                self.gccjit_location(func_def.loc.clone()),
                func_type,
                return_type.clone(),
                func_name.as_ptr(),
                params.len().try_into().unwrap(),
                params.as_mut_ptr(),
                0,
            )
        };

        self.param_table.borrow_mut().insert(func, func_params.clone());

        // Build func block
        let name = CString::new("entry").unwrap();
        let block = unsafe { gcc_jit_function_new_block(func, name.as_ptr()) };
        let mut return_compiled = false;

        let mut guard = self.block_func_ref.lock().unwrap();
        guard.block = Some(block);
        guard.func = Some(func);
        drop(guard);

        for item in func_def.body.body {
            if let Statement::Return(_) = item.clone() {
                return_compiled = true;
            }

            self.compile_statement(Rc::clone(&scope), item);
        }

        if func_def.return_type.is_some() {
            if !return_compiled {
                compiler_error!(format!(
                    "Explicit return statement required for the function '{}'.",
                    func_def.name
                ));
            }
        } else {
            if !self.block_is_terminated(block) {
                unsafe { gcc_jit_block_end_with_void_return(block, null_mut()) };
            }
        }

        return func;
    }

    fn compile_func_decl(&mut self, func_decl: FuncDecl) {
        // FIXME Make a macro to compile func_decl and func_def similarly
        todo!();

        // let func_type = match func_decl.vis_type {
        //     VisType::Extern => gcc_jit_function_kind::GCC_JIT_FUNCTION_IMPORTED, // imported function
        //     VisType::Pub => gcc_jit_function_kind::GCC_JIT_FUNCTION_EXPORTED,
        //     VisType::Internal => gcc_jit_function_kind::GCC_JIT_FUNCTION_INTERNAL,
        //     VisType::Inline => gcc_jit_function_kind::GCC_JIT_FUNCTION_ALWAYS_INLINE,
        // };

        // let return_type_token = func_decl
        //     .return_type
        //     .unwrap_or(Token {
        //         kind: TokenKind::Void,
        //         span: Span::new_empty_span(),
        //     })
        //     .kind;
        // let return_type = self.token_as_data_type(self.context, return_type_token.clone());

        // let mut params: Vec<*mut gcc_jit_param> = Vec::new();
        // let mut func_params = FuncParamsRecords::new();

        // for (idx, func_decl_param) in func_decl.params.iter().enumerate() {
        //     let name = CString::new(func_decl_param.identifier.name.clone()).unwrap();

        //     let ty_token = if let Some(user_def) = &func_decl_param.ty {
        //         user_def
        //     } else {
        //         &TokenKind::Void
        //     };

        //     let ty = self.token_as_data_type(self.context, ty_token.clone());

        //     let param = unsafe {
        //         gcc_jit_context_new_param(
        //             self.context,
        //             self.gccjit_location(func_decl_param.loc.clone()),
        //             ty,
        //             name.as_ptr(),
        //         )
        //     };

        //     params.push(param);

        //     func_params.push(FuncParamRecord {
        //         param_index: idx as i32,
        //         param_name: func_decl_param.identifier.name.clone(),
        //     });
        // }

        // let func_name = CString::new(func_decl.name.clone()).unwrap();
        // let func = unsafe {
        //     gcc_jit_context_new_function(
        //         self.context,
        //         self.gccjit_location(func_decl.loc.clone()),
        //         func_type,
        //         return_type.clone(),
        //         func_name.as_ptr(),
        //         params.len().try_into().unwrap(),
        //         params.as_mut_ptr(),
        //         0,
        //     )
        // };

        // self.param_table.borrow_mut().insert(func, func_params.clone());

        // self.func_table.borrow_mut().insert(
        //     func_decl.name,
        //     FuncMetadata {
        //         func_type: func_decl.vis_type,
        //         ptr: func,
        //         params: func_decl.params,
        //         return_type: return_type_token,
        //     },
        // );
    }

    fn compile_continue_statement(&mut self, loc: Location) {
        if let (Some(active_loop), Some(active_block)) = (self.active_loop.clone(), self.active_block()) {
            if !self.block_is_terminated(active_block) {
                unsafe {
                    gcc_jit_block_end_with_jump(active_block, self.gccjit_location(loc), active_loop.increment_block)
                }
                self.mark_block_terminated(active_block);
            }
        }
    }

    fn compile_break_statement(&mut self, loc: Location) {
        if let (Some(active_loop), Some(active_block)) = (self.active_loop.clone(), self.active_block()) {
            if !self.block_is_terminated(active_block) {
                unsafe { gcc_jit_block_end_with_jump(active_block, self.gccjit_location(loc), active_loop.loop_end) }
                self.mark_block_terminated(active_block);
            }
        }
    }

    fn compile_if_statement(&mut self, scope: ScopeRef, statement: If) {
        let guard = self.block_func_ref.lock().unwrap();

        if let (Some(active_block), Some(func)) = (guard.block, guard.func) {
            drop(guard);

            // Build blocks
            let true_block_name = CString::new(format!("true_block_{}", self.new_block_name())).unwrap();
            let false_block_name = CString::new(format!("false_block_{}", self.new_block_name())).unwrap();
            let final_block_name = CString::new(format!("final_block_{}", self.new_block_name())).unwrap();
            let true_block = unsafe { gcc_jit_function_new_block(func, true_block_name.as_ptr()) };
            let false_block = unsafe { gcc_jit_function_new_block(func, false_block_name.as_ptr()) };
            let final_block = unsafe { gcc_jit_function_new_block(func, final_block_name.as_ptr()) };
            let cond = self.compile_expression(Rc::clone(&scope), statement.condition);

            // Store the current block as the parent block for nested structures
            let previous_parent_block = self.parent_block;
            self.parent_block = Some(final_block);

            unsafe {
                gcc_jit_block_end_with_conditional(
                    active_block,
                    self.gccjit_location(statement.loc.clone()),
                    cond,
                    true_block,
                    false_block,
                )
            };
            self.mark_block_terminated(active_block);

            // Build true_block body
            if !self.block_is_terminated(true_block) {
                self.switch_active_block(true_block);
                self.compile_statements(Rc::clone(&scope), statement.consequent.body);
            }

            // Build else-if and else branches
            let mut current_block = false_block;

            for else_if_statement in statement.branches {
                let else_if_cond = self.compile_expression(Rc::clone(&scope), else_if_statement.condition);

                let else_if_true_block_name =
                    CString::new(format!("else_if_true_block_{}", self.new_block_name())).unwrap();
                let else_if_false_block_name =
                    CString::new(format!("else_if_false_block_{}", self.new_block_name())).unwrap();

                let else_if_true_block = unsafe { gcc_jit_function_new_block(func, else_if_true_block_name.as_ptr()) };

                let else_if_false_block =
                    unsafe { gcc_jit_function_new_block(func, else_if_false_block_name.as_ptr()) };

                if !self.block_is_terminated(current_block) {
                    unsafe {
                        gcc_jit_block_end_with_conditional(
                            current_block,
                            self.gccjit_location(else_if_statement.loc.clone()),
                            else_if_cond,
                            else_if_true_block,
                            else_if_false_block,
                        );
                    }
                    self.mark_block_terminated(current_block);
                }

                // Process true block for else-if
                if !self.block_is_terminated(else_if_true_block) {
                    self.switch_active_block(else_if_true_block);
                    self.compile_statements(Rc::clone(&scope), else_if_statement.consequent.body);
                }

                if !self.block_is_terminated(else_if_true_block) {
                    unsafe {
                        gcc_jit_block_end_with_jump(
                            else_if_true_block,
                            self.gccjit_location(else_if_statement.loc.clone()),
                            final_block,
                        );
                    }

                    self.mark_block_terminated(else_if_true_block);
                }

                current_block = else_if_false_block;
            }

            // Process else block if no conditions matched
            if let Some(else_statements) = statement.alternate {
                self.switch_active_block(current_block);
                self.compile_statements(Rc::clone(&scope), else_statements.body);

                if !self.block_is_terminated(current_block) {
                    unsafe {
                        gcc_jit_block_end_with_jump(
                            current_block,
                            self.gccjit_location(else_statements.loc),
                            final_block,
                        );
                    }

                    self.mark_block_terminated(current_block);
                }
            } else if !self.block_is_terminated(current_block) {
                unsafe {
                    gcc_jit_block_end_with_jump(
                        current_block,
                        self.gccjit_location(statement.loc.clone()),
                        final_block,
                    );
                }
                self.mark_block_terminated(current_block);
            }

            // Ensure true block ends with jump to final block
            if !self.block_is_terminated(true_block) {
                self.switch_active_block(true_block);
                unsafe {
                    gcc_jit_block_end_with_jump(true_block, self.gccjit_location(statement.loc.clone()), final_block);
                }
                self.mark_block_terminated(true_block);
            }

            // Restore the parent block after finishing nested structures
            self.parent_block = previous_parent_block;

            // If there is a parent block, ensure the final block jumps back to it
            if let Some(parent_block) = self.parent_block {
                if !self.block_is_terminated(final_block) {
                    unsafe {
                        gcc_jit_block_end_with_jump(
                            final_block,
                            self.gccjit_location(statement.loc.clone()),
                            parent_block,
                        )
                    }
                    self.mark_block_terminated(final_block);
                    self.switch_active_block(parent_block);
                    return;
                }
            }

            self.switch_active_block(final_block);
        }
    }

    fn compile_for_statement(&mut self, scope: ScopeRef, statement: For) {
        let loc = self.gccjit_location(statement.loc.clone());
        let guard = self.block_func_ref.lock().unwrap();

        if let (Some(active_block), Some(func)) = (guard.block, guard.func) {
            drop(guard);

            // Create blocks
            let for_body_name = CString::new(format!("for_body_block_{}", self.new_block_name())).unwrap();
            let for_end_name = CString::new(format!("for_end_block_{}", self.new_block_name())).unwrap();
            let for_increment_name = CString::new(format!("for_increment_block_{}", self.new_block_name())).unwrap();
            let for_body = unsafe { gcc_jit_function_new_block(func, for_body_name.as_ptr()) };
            let for_end = unsafe { gcc_jit_function_new_block(func, for_end_name.as_ptr()) };
            let for_increment_block = unsafe { gcc_jit_function_new_block(func, for_increment_name.as_ptr()) };

            self.active_loop = Some(LoopBlockPair {
                loop_end: for_end,
                increment_block: for_increment_block,
            });

            // Initialize incremental variable
            if let Some(initializer) = statement.initializer {
                if let Some(expr) = initializer.expr {
                    let init_rvalue = self.compile_expression(Rc::clone(&scope), expr);
                    let init_type = unsafe { gcc_jit_rvalue_get_type(init_rvalue) };
                    let init_name = CString::new(initializer.name.clone()).unwrap();
                    let init_lvalue = unsafe { gcc_jit_function_new_local(func, loc, init_type, init_name.as_ptr()) };

                    unsafe { gcc_jit_block_add_assignment(active_block, loc.clone(), init_lvalue, init_rvalue) };
                    Rc::clone(&scope).borrow_mut().insert(
                        initializer.name,
                        IdentifierMetadata {
                            lvalue: init_lvalue,
                            lvalue_type: init_type,
                        },
                    );
                } else {
                    compiler_error!("For statement variable must be initialized with a valid value.");
                }
            }

            let cond = if let Some(expr) = statement.condition.clone() {
                self.compile_expression(Rc::clone(&scope), expr)
            } else {
                unsafe { gcc_jit_context_new_rvalue_from_int(self.context, Compiler::bool_type(self.context), 1) }
            };

            // Begin the loop
            if !self.block_is_terminated(active_block) {
                unsafe {
                    gcc_jit_block_end_with_conditional(active_block, loc.clone(), cond, for_body, for_end);
                }
            }

            // Evaluate increment
            self.switch_active_block(for_increment_block);
            if let Some(ref increment) = statement.increment {
                self.compile_expression(Rc::clone(&scope), increment.clone());
            }

            self.switch_active_block(for_body);

            // Compile the body of the loop
            for stmt in statement.body.body {
                match stmt {
                    Statement::Break(loc) => {
                        self.compile_break_statement(loc);
                        break;
                    }
                    Statement::Continue(loc) => {
                        self.compile_continue_statement(loc);
                        break;
                    }
                    _ => self.compile_statement(Rc::clone(&scope), stmt),
                }
            }

            // Safely terminate active_block as a recurisve-jump that points into current for_loop
            let guard = self.block_func_ref.lock().unwrap();
            if let Some(active_block) = guard.block {
                if !self.block_is_terminated(active_block) {
                    unsafe {
                        gcc_jit_block_end_with_conditional(
                            active_block,
                            loc.clone(),
                            cond,
                            for_increment_block,
                            for_end,
                        )
                    }

                    unsafe {
                        gcc_jit_block_end_with_conditional(for_increment_block, loc.clone(), cond, for_body, for_end)
                    }
                } else {
                    unsafe { gcc_jit_block_end_with_jump(for_increment_block, loc, for_end) }
                }
            }
            drop(guard);

            // End the loop
            self.switch_active_block(for_end);
        }
    }

    fn switch_active_block(&mut self, active_block: *mut gcc_jit_block) {
        let mut guard = self.block_func_ref.lock().unwrap();
        guard.block = Some(active_block);
        drop(guard);
    }

    fn active_block(&mut self) -> Option<*mut gccjit_sys::gcc_jit_block> {
        let guard = self.block_func_ref.lock().unwrap();
        return guard.block;
    }

    fn mark_block_terminated(&mut self, block: *mut gcc_jit_block) {
        if !self.block_is_terminated(block) {
            self.terminated_blocks.push(block);
        }
    }

    fn block_is_terminated(&self, block: *mut gcc_jit_block) -> bool {
        self.terminated_blocks.contains(&block)
    }

    fn compile_return(&mut self, scope: ScopeRef, statement: Return) {
        let guard = self.block_func_ref.lock().unwrap();

        if let Some(block) = guard.block {
            drop(guard);

            let ret_value = self.compile_expression(scope, statement.argument);

            if !self.block_is_terminated(block) {
                unsafe { gcc_jit_block_end_with_return(block, self.gccjit_location(statement.loc), ret_value) };
            }
        } else {
            compiler_error!("Incorrect usage of the return statement. It must be used inside a function definition.");
        }
    }

    fn compile_variable(&mut self, scope: ScopeRef, variable: Variable) {
        let guard = self.block_func_ref.lock().unwrap();

        if let (Some(block), Some(func)) = (guard.block, guard.func) {
            drop(guard);

            let mut var_type: *mut gcc_jit_type = null_mut();
            let mut rvalue = null_mut();

            if let Some(token) = variable.ty.clone() {
                var_type = self.token_as_data_type(self.context, token);
            }

            if let Some(expr) = variable.expr {
                rvalue = match expr {
                    Expression::Array(array) => self.compile_array(Rc::clone(&scope), array, var_type),
                    _ => self.compile_expression(Rc::clone(&scope), expr),
                };

                if variable.ty.is_none() {
                    var_type = unsafe { gcc_jit_rvalue_get_type(rvalue) };
                }
            }

            // if var_type.is_null() {
            //     compiler_error!("Undefined behaviour in variable declaration. Explicit type definition required.");
            // }

            let name = CString::new(variable.name.clone()).unwrap();
            let lvalue = unsafe {
                gcc_jit_function_new_local(
                    func,
                    self.gccjit_location(variable.loc.clone()),
                    var_type,
                    name.as_ptr(),
                )
            };

            if !rvalue.is_null() {
                let mut casted_rvalue = rvalue.clone();

                if let Some(var_token_type) = variable.ty {
                    if self.auto_castable_data_types(var_token_type) {
                        casted_rvalue = unsafe {
                            gcc_jit_context_new_cast(
                                self.context,
                                self.gccjit_location(variable.loc.clone()),
                                rvalue,
                                var_type,
                            )
                        };
                    }
                }

                unsafe {
                    gcc_jit_block_add_assignment(
                        block,
                        self.gccjit_location(variable.loc.clone()),
                        lvalue,
                        casted_rvalue,
                    )
                };
            }

            scope.borrow_mut().insert(
                variable.name,
                IdentifierMetadata {
                    lvalue,
                    lvalue_type: var_type,
                },
            );
        } else {
            compiler_error!("Invalid usage of local variable.");
        }
    }

    fn access_identifier_values(
        &mut self,
        scope: ScopeRef,
        identifier: Identifier,
    ) -> (*mut gcc_jit_lvalue, *mut gcc_jit_rvalue) {
        match scope.borrow_mut().get(identifier.name.clone()) {
            Some(lvalue) => {
                let rvalue = unsafe { gcc_jit_lvalue_as_rvalue(lvalue.borrow_mut().lvalue) };
                return (lvalue.borrow_mut().lvalue, rvalue);
            }
            None => {
                let guard = self.block_func_ref.lock().unwrap();

                if let Some(func) = guard.func {
                    if let Some(func_param_records) = self.param_table.borrow_mut().get(&func) {
                        for param in func_param_records {
                            if param.param_name == identifier.name {
                                let param = unsafe { gcc_jit_function_get_param(func, param.param_index) };
                                let rvalue = unsafe { gcc_jit_param_as_rvalue(param) };
                                let lvalue = unsafe { gcc_jit_param_as_lvalue(param) };
                                return (lvalue, rvalue);
                            }
                        }
                    }
                }

                compiler_error!(format!("'{}' is not defined in this scope.", identifier.name))
            }
        }
    }

    fn compile_identifier(&mut self, scope: ScopeRef, identifier: Identifier) -> *mut gcc_jit_rvalue {
        self.access_identifier_values(scope, identifier).1
    }

    fn compile_expression(&mut self, scope: ScopeRef, expr: Expression) -> *mut gcc_jit_rvalue {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => self.compile_identifier(scope, identifier),
            Expression::Prefix(unary_expression) => self.compile_prefix_expression(scope, unary_expression),
            Expression::Infix(binary_expression) => self.compile_infix_expression(scope, binary_expression),
            Expression::FunctionCall(func_call) => self.compile_func_call(scope, func_call),
            Expression::UnaryOperator(unary_operator) => self.compile_unary_operator(scope, unary_operator),
            Expression::Array(array) => self.compile_array(Rc::clone(&scope), array, null_mut()),
            Expression::ArrayIndex(array_index) => self.compile_array_index(Rc::clone(&scope), array_index),
            Expression::Assignment(assignment) => self.compile_assignment(scope, *assignment),
            Expression::ArrayIndexAssign(array_index_assign) => {
                self.compile_array_index_assign(Rc::clone(&scope), *array_index_assign)
            }
            Expression::AddressOf(expression) => self.compile_address_of(Rc::clone(&scope), expression),
            Expression::Dereference(expression) => self.compile_dereference(Rc::clone(&scope), expression),
            Expression::StructInit(struct_init) => self.compile_struct_init(scope, struct_init),
            Expression::StructFieldAccess(struct_field_access) => {
                self.compile_struct_field_access(scope, *struct_field_access)
            }
        }
    }

    fn compile_dereference(&mut self, scope: ScopeRef, expression: Box<Expression>) -> *mut gcc_jit_rvalue {
        let rvalue = self.compile_expression(scope, *expression.clone());

        unsafe { gcc_jit_lvalue_as_rvalue(gcc_jit_rvalue_dereference(rvalue, null_mut())) }
    }

    fn compile_address_of(&mut self, scope: ScopeRef, expression: Box<Expression>) -> *mut gcc_jit_rvalue {
        match *expression {
            Expression::Identifier(identifier) => {
                let lvalue = self.access_identifier_values(Rc::clone(&scope), identifier).0;
                unsafe { gcc_jit_lvalue_get_address(lvalue, null_mut()) }
            }
            _ => self.compile_expression(scope, *expression),
        }
    }

    fn compile_array_index_assign(
        &mut self,
        scope: ScopeRef,
        array_index_assign: ArrayIndexAssign,
    ) -> *mut gcc_jit_rvalue {
        match scope.borrow_mut().get(array_index_assign.identifier.name.clone()) {
            Some(variable) => {
                let lvalue = self.array_dimension_as_lvalue(
                    Rc::clone(&scope),
                    variable.borrow_mut().lvalue,
                    array_index_assign.dimensions,
                );

                let rvalue = self.compile_expression(Rc::clone(&scope), array_index_assign.expr);

                let block_func = self.block_func_ref.lock().unwrap();
                if let Some(block) = block_func.block {
                    drop(block_func);

                    unsafe {
                        gcc_jit_block_add_assignment(
                            block,
                            self.gccjit_location(array_index_assign.loc.clone()),
                            lvalue,
                            rvalue,
                        )
                    };

                    rvalue
                } else {
                    compiler_error!("Array index assignment in invalid block.");
                }
            }
            None => compiler_error!(format!(
                "'{}' is not defined in this scope.",
                array_index_assign.identifier.name
            )),
        }
    }

    fn array_dimension_as_lvalue(
        &mut self,
        scope: ScopeRef,
        variable: *mut gcc_jit_lvalue,
        dimensions: Vec<Expression>,
    ) -> *mut gcc_jit_lvalue {
        let mut result: *mut gcc_jit_lvalue = variable;

        for dim in dimensions {
            if let Expression::Array(index_expr) = dim {
                if let Expression::Array(value) = index_expr.elements[0].clone() {
                    // TODO Implement ranges here

                    let idx = self.compile_expression(Rc::clone(&scope), value.elements[0].clone());

                    let lvalue = unsafe {
                        gcc_jit_context_new_array_access(
                            self.context,
                            null_mut(),
                            gcc_jit_lvalue_as_rvalue(result),
                            idx,
                        )
                    };

                    result = lvalue;
                }
            }
        }

        result
    }

    fn compile_array_index(&mut self, scope: ScopeRef, array_index: ArrayIndex) -> *mut gcc_jit_rvalue {
        match scope.borrow_mut().get(array_index.identifier.name.clone()) {
            Some(variable) => {
                let lvalue = self.array_dimension_as_lvalue(
                    Rc::clone(&scope),
                    variable.borrow_mut().lvalue,
                    array_index.dimensions,
                );

                unsafe { gcc_jit_lvalue_as_rvalue(lvalue) }
            }
            None => compiler_error!(format!(
                "'{}' is not defined in this scope.",
                array_index.identifier.name
            )),
        }
    }

    fn compile_array(
        &mut self,
        scope: ScopeRef,
        array: Array,
        mut array_type: *mut gcc_jit_type,
    ) -> *mut gcc_jit_rvalue {
        let mut array_elements: Vec<*mut gcc_jit_rvalue> = Vec::new();
        for expr in array.elements {
            array_elements.push(self.compile_expression(Rc::clone(&scope), expr));
        }

        let element_type = unsafe { gcc_jit_rvalue_get_type(array_elements[0]) };

        if array_type.is_null() {
            array_type = unsafe {
                gcc_jit_context_new_array_type(
                    self.context,
                    self.gccjit_location(array.loc.clone()),
                    element_type,
                    array_elements.len() as u64,
                )
            }
        }

        unsafe {
            gcc_jit_context_new_array_constructor(
                self.context,
                self.gccjit_location(array.loc),
                array_type,
                array_elements.len() as i32,
                array_elements.as_mut_ptr(),
            )
        }
    }

    fn compile_assignment(&mut self, scope: ScopeRef, assignment: Assignment) -> *mut gcc_jit_rvalue {
        let (lvalue, _) = self.access_identifier_values(Rc::clone(&scope), assignment.identifier);

        let block_func = self.block_func_ref.lock().unwrap();
        if let Some(block) = block_func.block {
            drop(block_func);

            let new_rvalue = self.compile_expression(scope, assignment.expr);

            unsafe {
                gcc_jit_block_add_assignment(block, self.gccjit_location(assignment.loc.clone()), lvalue, new_rvalue);
            };

            return new_rvalue;
        } else {
            compiler_error!("Incorrect usage of the assignment. Assignments must be performed inside a valid block.");
        }
    }

    fn compile_func_arguments(
        &mut self,
        scope: ScopeRef,
        func_params: Option<Vec<FunctionParam>>,
        arguments: Vec<Expression>,
    ) -> Vec<*mut gcc_jit_rvalue> {
        let mut args: Vec<*mut gcc_jit_rvalue> = Vec::new();

        for (idx, expr) in arguments.iter().enumerate() {
            let mut expr = self.compile_expression(Rc::clone(&scope), expr.clone());

            if let Some(ref func_params) = func_params {
                let param = func_params[idx].clone();

                if let Some(var_token_type) = param.ty {
                    if self.auto_castable_data_types(var_token_type.clone()) {
                        expr = unsafe {
                            gcc_jit_context_new_cast(
                                self.context,
                                self.gccjit_location(param.loc.clone()),
                                expr,
                                self.token_as_data_type(self.context, var_token_type),
                            )
                        };
                    }
                }
            }

            args.push(expr);
        }

        args
    }

    fn compile_func_call(&mut self, scope: ScopeRef, func_call: FunctionCall) -> *mut gcc_jit_rvalue {
        let guard = self.block_func_ref.lock().unwrap();

        if let Some(block) = guard.block {
            drop(guard);

            let loc = self.gccjit_location(func_call.loc.clone());

            let mut args = self.compile_func_arguments(Rc::clone(&scope), None, func_call.arguments.clone());

            let func = {
                let func_table = self.func_table.borrow_mut();
                match func_table.get(&func_call.function_name.name) {
                    Some(func) => func.clone(),
                    None => match retrieve_builtin_func(func_call.function_name.name.clone()) {
                        Some(func_def) => {
                            return func_def(self.context, block, &mut args);
                        }
                        None => compiler_error!(format!(
                            "Function '{}' not defined at this module.",
                            func_call.function_name.name
                        )),
                    },
                }
            };

            args = self.compile_func_arguments(Rc::clone(&scope), Some(func.params.clone()), func_call.arguments);

            let rvalue = unsafe {
                gcc_jit_context_new_call(
                    self.context,
                    loc.clone(),
                    func.ptr,
                    args.len().try_into().unwrap(),
                    args.as_mut_ptr(),
                )
            };

            unsafe { gcc_jit_block_add_eval(block, loc, rvalue) };

            return rvalue;
        } else {
            compiler_error!("Calling any function at top-level nodes isn't allowed.");
        }
    }

    fn compile_unary_operator(&mut self, scope: ScopeRef, unary_operator: UnaryOperator) -> *mut gcc_jit_rvalue {
        let loc = self.gccjit_location(unary_operator.loc.clone());

        match scope.borrow_mut().get(unary_operator.identifer.name.clone()) {
            Some(lvalue) => {
                let rvalue = unsafe { gcc_jit_lvalue_as_rvalue(lvalue.borrow_mut().lvalue) };
                let rvalue_type = unsafe { gcc_jit_rvalue_get_type(rvalue) };

                if !self.is_int_data_type(rvalue_type) {
                    compiler_error!("Unary operations are only valid for integer types.");
                }

                let fixed_number = unsafe { gcc_jit_context_new_rvalue_from_int(self.context, rvalue_type, 1) };

                let bin_op = match unary_operator.ty.clone() {
                    UnaryOperatorType::PostIncrement | UnaryOperatorType::PreIncrement => {
                        gcc_jit_binary_op::GCC_JIT_BINARY_OP_PLUS
                    }
                    UnaryOperatorType::PostDecrement | UnaryOperatorType::PreDecrement => {
                        gcc_jit_binary_op::GCC_JIT_BINARY_OP_MINUS
                    }
                };

                let guard = self.block_func_ref.lock().unwrap();

                let tmp_local: *mut gcc_jit_lvalue;
                if let (Some(block), Some(func)) = (guard.block, guard.func) {
                    let tmp_local_name = CString::new("temp").unwrap();

                    tmp_local = unsafe { gcc_jit_function_new_local(func, loc, rvalue_type, tmp_local_name.as_ptr()) };

                    if !self.block_is_terminated(block) {
                        unsafe { gcc_jit_block_add_assignment(block, loc, tmp_local, rvalue) };
                    }
                } else {
                    compiler_error!("Unary operators (++, --, etc.) are only allowed inside functions.");
                }

                let tmp_rvalue = unsafe { gcc_jit_lvalue_as_rvalue(tmp_local) };

                // Assign incremented/decremented value in the variable
                if let Some(block) = guard.block {
                    if !self.block_is_terminated(block) {
                        unsafe {
                            gcc_jit_block_add_assignment_op(
                                block,
                                loc,
                                lvalue.borrow_mut().lvalue,
                                bin_op,
                                gcc_jit_context_new_cast(self.context, loc, fixed_number, rvalue_type),
                            )
                        };
                    }
                }

                let result = rvalue.clone();

                let result = match unary_operator.ty {
                    UnaryOperatorType::PreIncrement => result,
                    UnaryOperatorType::PostIncrement => tmp_rvalue,
                    UnaryOperatorType::PreDecrement => result,
                    UnaryOperatorType::PostDecrement => tmp_rvalue,
                };

                result
            }
            None => {
                compiler_error!(format!(
                    "'{}' is not defined in this scope.",
                    unary_operator.identifer.name
                ))
            }
        }
    }

    fn compile_prefix_expression(&mut self, scope: ScopeRef, unary_expression: UnaryExpression) -> *mut gcc_jit_rvalue {
        let op = match unary_expression.operator.kind {
            TokenKind::Minus => gcc_jit_unary_op::GCC_JIT_UNARY_OP_MINUS,
            TokenKind::Bang => gcc_jit_unary_op::GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
            _ => compiler_error!("Invalid operator given for the prefix expression."),
        };

        let expr = self.compile_expression(scope, *unary_expression.operand);
        let ty = unsafe { gcc_jit_rvalue_get_type(expr) };

        unsafe { gcc_jit_context_new_unary_op(self.context, self.gccjit_location(unary_expression.loc), op, ty, expr) }
    }

    fn compile_infix_expression(
        &mut self,
        scope: ScopeRef,
        binary_expression: BinaryExpression,
    ) -> *mut gcc_jit_rvalue {
        let left = self.compile_expression(Rc::clone(&scope), *binary_expression.left);
        let right = self.compile_expression(Rc::clone(&scope), *binary_expression.right);
        let left_type = unsafe { gcc_jit_rvalue_get_type(left) };
        let right_type = unsafe { gcc_jit_rvalue_get_type(right) };

        let widest_data_type = self.widest_data_type(left_type, right_type);

        let casted_left = unsafe {
            gcc_jit_context_new_cast(
                self.context,
                self.gccjit_location(binary_expression.loc.clone()),
                left,
                widest_data_type,
            )
        };
        let casted_right = unsafe {
            gcc_jit_context_new_cast(
                self.context,
                self.gccjit_location(binary_expression.loc.clone()),
                right,
                widest_data_type,
            )
        };

        match binary_expression.operator.kind {
            bin_op @ TokenKind::Plus
            | bin_op @ TokenKind::Minus
            | bin_op @ TokenKind::Slash
            | bin_op @ TokenKind::Asterisk
            | bin_op @ TokenKind::Percent => self.compile_binary_operation(
                bin_op,
                widest_data_type,
                casted_left,
                casted_right,
                binary_expression.loc,
            ),
            bin_op @ TokenKind::LessThan
            | bin_op @ TokenKind::LessEqual
            | bin_op @ TokenKind::GreaterThan
            | bin_op @ TokenKind::GreaterEqual
            | bin_op @ TokenKind::Equal
            | bin_op @ TokenKind::NotEqual => {
                self.compile_comparison_operation(bin_op, casted_left, casted_right, binary_expression.loc)
            }
            _ => compiler_error!("Invalid operator given for the infix expression."),
        }
    }

    fn compile_comparison_operation(
        &mut self,
        bin_op: TokenKind,
        left: *mut gcc_jit_rvalue,
        right: *mut gcc_jit_rvalue,
        loc: Location,
    ) -> *mut gcc_jit_rvalue {
        let op = match bin_op {
            TokenKind::LessThan => gcc_jit_comparison::GCC_JIT_COMPARISON_LT,
            TokenKind::LessEqual => gcc_jit_comparison::GCC_JIT_COMPARISON_LE,
            TokenKind::GreaterThan => gcc_jit_comparison::GCC_JIT_COMPARISON_GT,
            TokenKind::GreaterEqual => gcc_jit_comparison::GCC_JIT_COMPARISON_GE,
            TokenKind::Equal => gcc_jit_comparison::GCC_JIT_COMPARISON_EQ,
            TokenKind::NotEqual => gcc_jit_comparison::GCC_JIT_COMPARISON_NE,
            _ => panic!(),
        };

        unsafe { gcc_jit_context_new_comparison(self.context, self.gccjit_location(loc), op, left, right) }
    }

    fn compile_binary_operation(
        &mut self,
        bin_op: TokenKind,
        data_type: *mut gcc_jit_type,
        left: *mut gcc_jit_rvalue,
        right: *mut gcc_jit_rvalue,
        loc: Location,
    ) -> *mut gcc_jit_rvalue {
        let op = match bin_op {
            TokenKind::Plus => gcc_jit_binary_op::GCC_JIT_BINARY_OP_PLUS,
            TokenKind::Minus => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MINUS,
            TokenKind::Slash => gcc_jit_binary_op::GCC_JIT_BINARY_OP_DIVIDE,
            TokenKind::Asterisk => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MULT,
            TokenKind::Percent => gcc_jit_binary_op::GCC_JIT_BINARY_OP_MODULO,
            _ => panic!(),
        };

        unsafe { gcc_jit_context_new_binary_op(self.context, self.gccjit_location(loc), op, data_type, left, right) }
    }

    fn compile_literal(&mut self, literal: Literal) -> *mut gcc_jit_rvalue {
        match literal {
            Literal::Integer(integer_literal) => match integer_literal {
                IntegerLiteral::I8(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i8_type(self.context), value as i32)
                },
                IntegerLiteral::I16(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i16_type(self.context), value as i32)
                },
                IntegerLiteral::I32(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i32_type(self.context), value as i32)
                },
                IntegerLiteral::I64(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i64_type(self.context), value as i32)
                },
                IntegerLiteral::I128(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i128_type(self.context), value as i32)
                },
                IntegerLiteral::U8(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::u8_type(self.context), value as i32)
                },
                IntegerLiteral::U16(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::u16_type(self.context), value as i32)
                },
                IntegerLiteral::U32(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::u32_type(self.context), value as i32)
                },
                IntegerLiteral::U64(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::u64_type(self.context), value as i32)
                },
                IntegerLiteral::U128(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::u128_type(self.context), value as i32)
                },
                IntegerLiteral::CSize(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_int(self.context, Compiler::csize_type(self.context), value as i32)
                },
            },
            Literal::Float(float_literal) => match float_literal {
                FloatLiteral::Float(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_double(self.context, Compiler::f32_type(self.context), value as f64)
                },
                FloatLiteral::Double(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_double(self.context, Compiler::f64_type(self.context), value as f64)
                },
            },
            Literal::Bool(bool_literal) => {
                let value = if bool_literal.raw { 1 } else { 0 };
                unsafe { gcc_jit_context_new_rvalue_from_int(self.context, Compiler::i8_type(self.context), value) }
            }
            Literal::String(string_literal) => unsafe {
                let value = CString::new(self.purify_string(string_literal.raw)).unwrap();
                gcc_jit_context_new_string_literal(self.context, value.as_ptr())
            },
            Literal::Char(char_literal) => unsafe {
                gcc_jit_context_new_rvalue_from_int(
                    self.context,
                    Compiler::char_type(self.context),
                    char_literal.raw as i32,
                )
            },
            Literal::Null => unsafe { gcc_jit_context_null(self.context, Compiler::void_ptr_type(self.context)) },
        }
    }
}

impl Drop for Compiler {
    fn drop(&mut self) {
        unsafe { gcc_jit_context_release(self.context) };

        for item in self.compiled_object_files.clone() {
            remove_file(item).unwrap();
        }
    }
}
