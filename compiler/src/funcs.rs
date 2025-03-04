use std::ffi::CString;
use std::ptr::null_mut;
use std::rc::Rc;

use crate::Compiler;
use crate::scope::IdentifierMetadata;
use crate::scope::ScopeRef;
use ast::ast::*;
use ast::token::*;
use gccjit_sys::*;
use utils::compile_time_errors::errors::*;
use utils::compiler_error;
use utils::generate_random_hex::generate_random_hex;

#[derive(Debug, Clone)]
pub struct FuncMetadata {
    pub(crate) func_type: VisType,
    pub(crate) ptr: *mut gcc_jit_function,
    pub(crate) return_type: TokenKind,
    pub(crate) params: FunctionParams,
    pub(crate) fixed_params_count: usize,
    pub(crate) imported_from: Option<String>,
}

#[derive(Debug, Clone)]
pub struct FuncParamRecord {
    pub(crate) param_index: i32,
    pub(crate) param_name: String,
}

pub type FuncParamsRecords = Vec<FuncParamRecord>;

impl Compiler {
    pub fn safe_func_return_type_token(&mut self, return_type: Option<Token>) -> TokenKind {
        return_type
            .clone()
            .unwrap_or(Token {
                kind: TokenKind::Void,
                span: Span::default(),
            })
            .kind
    }

    pub fn safe_func_return_type(&mut self, return_type: Option<Token>) -> *mut gcc_jit_type {
        let return_type_token = self.safe_func_return_type_token(return_type);
        self.token_as_data_type(self.context, return_type_token.clone())
    }

    fn declare_function(&mut self, func_decl: FuncDecl) -> (*mut gcc_jit_function, FuncParamsRecords) {
        let func_type = match func_decl.vis_type {
            VisType::Extern => gcc_jit_function_kind::GCC_JIT_FUNCTION_IMPORTED, // imported function
            VisType::Pub => gcc_jit_function_kind::GCC_JIT_FUNCTION_EXPORTED,
            VisType::Internal => gcc_jit_function_kind::GCC_JIT_FUNCTION_INTERNAL,
            VisType::Inline => gcc_jit_function_kind::GCC_JIT_FUNCTION_ALWAYS_INLINE,
        };

        let return_type = self.safe_func_return_type(func_decl.return_type);

        let mut params: Vec<*mut gcc_jit_param> = Vec::new();
        let mut func_params = FuncParamsRecords::new();

        for (idx, func_def_param) in func_decl.params.list.iter().enumerate() {
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

        let func_name = CString::new(func_decl.name.clone()).unwrap();
        let func = unsafe {
            gcc_jit_context_new_function(
                self.context,
                self.gccjit_location(func_decl.loc.clone()),
                func_type,
                return_type.clone(),
                func_name.as_ptr(),
                params.len().try_into().unwrap(),
                params.as_mut_ptr(),
                self.cbool(func_decl.params.variadic.is_some()),
            )
        };

        return (func, func_params);
    }

    pub(crate) fn compile_func_def(&mut self, scope: ScopeRef, func_def: FuncDef) -> *mut gcc_jit_function {
        let (func_ptr, func_params) = self.declare_function(FuncDecl {
            name: func_def.name.clone(),
            params: func_def.params.clone(),
            return_type: func_def.return_type.clone(),
            vis_type: func_def.vis_type.clone(),
            renamed_as: None,
            span: func_def.span,
            loc: func_def.loc,
        });

        self.param_table.borrow_mut().insert(func_ptr, func_params.clone());

        // Build func block
        let name = CString::new("entry").unwrap();
        let block = unsafe { gcc_jit_function_new_block(func_ptr, name.as_ptr()) };
        let mut return_compiled = false;

        let mut guard = self.block_func_ref.lock().unwrap();
        guard.block = Some(block);
        guard.func = Some(func_ptr);
        drop(guard);

        for item in func_def.body.body {
            if let Statement::Return(_) = item.clone() {
                return_compiled = true;
            }

            self.compile_statement(Rc::clone(&scope), item);
        }

        if func_def.return_type.is_some() {
            if !return_compiled {
                compiler_error!(
                    format!(
                        "Explicit return statement required for the function '{}'.",
                        func_def.name
                    ),
                    self.file_path.clone()
                );
            }
        } else {
            let guard = self.block_func_ref.lock().unwrap();
            if let Some(block) = guard.block {
                drop(guard);
                if !self.block_is_terminated(block) {
                    unsafe { gcc_jit_block_end_with_void_return(block, null_mut()) };
                }
            }
        }

        return func_ptr;
    }

    pub(crate) fn compile_func_decl(&mut self, declare_function: FuncDecl) {
        let (func, _) = self.declare_function(declare_function.clone());
        let return_type = self.safe_func_return_type_token(declare_function.return_type);
        let func_name = if let Some(renamed_as) = declare_function.renamed_as {
            renamed_as
        } else {
            declare_function.name
        };

        let fixed_params_count = declare_function.params.list.len();

        self.func_table.borrow_mut().insert(
            func_name,
            FuncMetadata {
                func_type: declare_function.vis_type,
                ptr: func,
                params: declare_function.params,
                return_type,
                imported_from: None,
                fixed_params_count,
            },
        );
    }

    pub(crate) fn compile_func_params(
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

    pub(crate) fn compile_func_arguments(
        &mut self,
        scope: ScopeRef,
        func_params: Option<Vec<FunctionParam>>,
        arguments: Vec<Expression>,
    ) -> Vec<*mut gcc_jit_rvalue> {
        let mut args: Vec<*mut gcc_jit_rvalue> = Vec::new();

        for (idx, expr) in arguments.iter().enumerate() {
            let mut expr = self.compile_expression(Rc::clone(&scope), expr.clone());

            if let Some(ref func_params) = func_params {
                if let Some(param) = func_params.get(idx) {
                    if let Some(var_token_type) = &param.ty {
                        if self.auto_castable_data_types(var_token_type.clone()) {
                            expr = unsafe {
                                gcc_jit_context_new_cast(
                                    self.context,
                                    self.gccjit_location(param.loc.clone()),
                                    expr,
                                    self.token_as_data_type(self.context, var_token_type.clone()),
                                )
                            };
                        }
                    }
                }
            }

            args.push(expr);
        }

        args
    }

    pub(crate) fn get_func(&mut self, from_package: FromPackage) -> FuncMetadata {
        let binding = self.func_table.borrow_mut();
        let func_metadata = binding.iter().find(|&item| {
            (if let Some(imported_from) = &item.1.imported_from {
                *imported_from == sub_packages_as_string(from_package.sub_packages.clone())
            } else {
                true
            }) && from_package.identifier.name == *item.0
        });

        if let Some(func_metadata) = func_metadata {
            return func_metadata.1.clone();
        }

        compiler_error!(
            format!("Function '{}' not defined at this module.", from_package.to_string()),
            self.file_path.clone()
        )
    }

    pub(crate) fn compile_func_call(&mut self, scope: ScopeRef, func_call: FuncCall) -> *mut gcc_jit_rvalue {
        let loc = self.gccjit_location(func_call.loc.clone());

        let metadata = self.get_func(func_call.func_name.clone());

        let func_metadata = self.get_func(func_call.func_name.clone());
        let fixed_params_count = func_metadata.fixed_params_count;

        let mut args = self.compile_func_arguments(
            Rc::clone(&scope),
            Some(metadata.params.list.clone()),
            func_call.arguments,
        );

        // FIXME Find a better solution
        // Add variadic_arguments identifier to current scope
        // if let Some(variadic) = func_metadata.params.variadic {
        //     let vargs_type = self.token_as_data_type(
        //         self.context,
        //         TokenKind::Array(
        //             Box::new(variadic),
        //             vec![Some(TokenKind::Literal(Literal::Integer(IntegerLiteral::SizeT(
        //                 fixed_params_count,
        //             ))))],
        //         ),
        //     );
        //     let mut vargs_values = args[0..fixed_params_count - 1].to_vec();

        //     let vargs_rvalue = unsafe {
        //         gcc_jit_context_new_array_constructor(
        //             self.context,
        //             null_mut(),
        //             vargs_type,
        //             vargs_values.len().try_into().unwrap(),
        //             vargs_values.as_mut_ptr(),
        //         )
        //     };

        //     let vargs_lvalue_name = CString::new(format!("__func_vargs__{}", generate_random_hex())).unwrap();
        //     let vargs_lvalue = unsafe {
        //         gcc_jit_context_new_global(
        //             self.context,
        //             self.gccjit_location(func_call.loc.clone()),
        //             gcc_jit_global_kind::GCC_JIT_GLOBAL_INTERNAL,
        //             vargs_type,
        //             vargs_lvalue_name.as_ptr(),
        //         )
        //     };

        //     unsafe { gcc_jit_global_set_initializer_rvalue(vargs_lvalue, vargs_rvalue) };

        //     scope.borrow_mut().insert(
        //         String::from("vargs"),
        //         IdentifierMetadata {
        //             lvalue_type: vargs_type,
        //             lvalue: vargs_lvalue,
        //         },
        //     );
        // }

        let rvalue = unsafe {
            gcc_jit_context_new_call(
                self.context,
                loc.clone(),
                metadata.ptr,
                args.len().try_into().unwrap(),
                args.as_mut_ptr(),
            )
        };

        rvalue
    }

    pub(crate) fn eval_func_call(&mut self, func_call: *mut gcc_jit_rvalue, loc: Location) {
        let guard = self.block_func_ref.lock().unwrap();

        if let Some(block) = guard.block {
            drop(guard);

            unsafe { gcc_jit_block_add_eval(block, self.gccjit_location(loc), func_call) };
        }
    }

    pub(crate) fn access_current_func_param(
        &mut self,
        identifier: Identifier,
    ) -> Option<(*mut gcc_jit_lvalue, *mut gcc_jit_rvalue)> {
        let guard = self.block_func_ref.lock().unwrap();
        if let Some(func) = guard.func {
            if let Some(func_param_records) = self.param_table.borrow_mut().get(&func) {
                for param in func_param_records {
                    if param.param_name == identifier.name {
                        let param = unsafe { gcc_jit_function_get_param(func, param.param_index) };
                        let rvalue = unsafe { gcc_jit_param_as_rvalue(param) };
                        let lvalue = unsafe { gcc_jit_param_as_lvalue(param) };
                        return Some((lvalue, rvalue));
                    }
                }
            }
        }
        None
    }
}
