use std::{ffi::CString, ptr::null_mut, rc::Rc};

use ast::ast::*;
use gccjit_sys::*;
use utils::compiler_error;

use crate::{
    scope::{IdentifierMetadata, ScopeRef},
    Compiler,
};

impl Compiler {
    pub(crate) fn compile_variable(&mut self, scope: ScopeRef, variable: Variable) {
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
                    Expression::FieldAccessOrMethodCall(mut chains) => {
                        let rvalue = self.eval_first_item_of_chains(Rc::clone(&scope), chains.clone());
                        chains.remove(0);
                        self.field_access_or_method_call(Rc::clone(&scope), rvalue, chains)
                    }
                    Expression::StructFieldAccess(struct_field_access) => {
                        self.compile_struct_field_access(Rc::clone(&scope), *struct_field_access.clone())
                    }
                    _ => self.compile_expression(Rc::clone(&scope), expr),
                };

                if variable.ty.is_none() {
                    var_type = unsafe { gcc_jit_rvalue_get_type(rvalue) };
                }
            }

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
}
