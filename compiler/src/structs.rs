use std::ffi::CString;
use std::ptr::null_mut;
use std::rc::Rc;

use crate::scope::ScopeRef;
use crate::Compiler;
use ast::ast::*;
use ast::token::*;
use gccjit_sys::*;
use utils::compiler_error;
use utils::generate_random_hex::generate_random_hex;

#[derive(Debug, Clone)]
pub struct StructMethodMetadata {
    #[allow(dead_code)]
    pub(crate) is_static: bool,
    pub(crate) func_def: FuncDef,
}

#[derive(Debug, Clone)]
pub struct StructMetadata {
    pub(crate) vis_type: VisType,
    pub(crate) struct_type: *mut gcc_jit_struct,
    pub(crate) fields: Vec<Field>,
    pub(crate) field_ptrs: Vec<*mut gcc_jit_field>,
    pub(crate) methods: Vec<StructMethodMetadata>,
    pub(crate) method_ptrs: Vec<*mut gcc_jit_function>,
}

impl Compiler {
    pub(crate) fn make_struct_method_name(&self, struct_name: String, method_name: String) -> String {
        format!("__struct__{}__{}", struct_name, method_name)
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
                let func_def = struct_metadata.methods[method_idx].clone().func_def;
                let func_ptr = struct_metadata.method_ptrs[method_idx];

                unsafe {
                    gcc_jit_context_new_call(
                        self.context,
                        self.gccjit_location(func_def.loc.clone()),
                        func_ptr,
                        arguments.len().try_into().unwrap(),
                        arguments.as_mut_ptr(),
                    )
                }
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

    pub(crate) fn struct_field_access_or_method_call(
        &mut self,
        scope: ScopeRef,
        first_chain_func_name: String,
        chains: Vec<FieldAccessOrMethodCall>,
        rvalue: *mut gcc_jit_rvalue,
    ) -> *mut gcc_jit_rvalue {
        let (func, block) = {
            let guard = self.block_func_ref.lock().unwrap();
            (guard.func, guard.block)
        };

        if let (Some(func), Some(block)) = (func, block) {
            let mut result: *mut gcc_jit_rvalue = rvalue;

            if result == null_mut() {
                compiler_error!(format!(
                    "Func '{}' returns null value, hence chained func call is an undefined behaviour.",
                    first_chain_func_name
                ));
            }

            for item in chains {
                unsafe { gcc_jit_type_is_struct(gcc_jit_rvalue_get_type(result)) }; // check to be struct

                if let Some(method_call) = item.method_call {
                    if let Some((struct_name, struct_metadata)) = self.find_struct(result) {
                        let method_def = self.get_struct_method_def(
                            struct_metadata.methods.clone(),
                            struct_name.clone(),
                            method_call.func_name.name.clone(),
                        );

                        // Inserting self argument
                        let mut arguments = self.compile_func_arguments(
                            Rc::clone(&scope),
                            Some(method_def.params.list.clone()),
                            method_call.arguments,
                        );

                        if let Some(self_param) =
                            method_def.params.list.iter().find(|&key| key.identifier.name == "self")
                        {
                            let self_arg = {
                                match self_param.ty.clone().unwrap() {
                                    TokenKind::UserDefinedType(_) => result,
                                    TokenKind::Dereference(_) => unsafe {
                                        // This means it's a pointer to this type so we need to pass value by address
                                        let result_type = gcc_jit_rvalue_get_type(result);
                                        let temp_name =
                                            CString::new(format!("temp__{}", generate_random_hex())).unwrap();
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
                        }

                        result = self.compile_struct_method_call(
                            struct_name.clone(),
                            struct_metadata.clone(),
                            method_call.func_name.name,
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
            compiler_error!("Func call is only valid inside a block");
        }
    }

    pub(crate) fn compile_struct_field_access(
        &mut self,
        scope: ScopeRef,
        statement: StructFieldAccess,
    ) -> *mut gcc_jit_rvalue {
        let mut method_call_chain = statement.chains.clone();

        let (func, block) = {
            let guard = self.block_func_ref.lock().unwrap();
            (guard.func, guard.block)
        };

        let mut first_chain_func_name = String::from("<UB>");

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
                        first_chain_func_name = method_call.func_name.name.clone();

                        let method_def = self.get_struct_method_def(
                            struct_metadata.methods.clone(),
                            identifier.name.clone(),
                            method_call.func_name.name.clone(),
                        );

                        let arguments = {
                            // Isolate the mutable borrow to avoid conflict with immutable borrows.
                            self.compile_func_arguments(
                                Rc::clone(&scope),
                                Some(method_def.params.list),
                                method_call.arguments,
                            )
                        };

                        result = self.compile_struct_method_call(
                            identifier.name.clone(),
                            struct_metadata.clone(),
                            method_call.func_name.name,
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
        }

        if result == null_mut() {
            compiler_error!("Unexpected behaviour in struct field access compilation.");
        }

        self.struct_field_access_or_method_call(
            Rc::clone(&scope),
            first_chain_func_name.to_string(),
            statement.chains,
            result,
        )
    }

    pub(crate) fn compile_struct_init(&mut self, scope: ScopeRef, struct_init: StructInit) -> *mut gcc_jit_rvalue {
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

    pub(crate) fn compile_struct_fields(&mut self, fields: Vec<Field>) -> Vec<*mut gcc_jit_field> {
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

    pub(crate) fn compile_struct(&mut self, scope: ScopeRef, statement: Struct) {
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

            if let Some(self_param) = item.params.list.iter().find(|&key| key.identifier.name == "self") {
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
                            "Static method '{}' must be defined as pub(crate) fn.",
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
}
