use crate::scope::IdentifierMetadata;
use crate::scope::ScopeRef;
use crate::Compiler;
use ast::ast::*;
use ast::token::*;
use gccjit_sys::*;
use std::ffi::CString;
use std::rc::Rc;
use utils::compiler_error;
use utils::generate_random_hex::generate_random_hex;

#[derive(Debug, Clone)]
pub struct LoopBlockPair {
    loop_end: *mut gcc_jit_block,
    increment_block: *mut gcc_jit_block,
}

impl Compiler {
    pub(crate) fn compile_continue_statement(&mut self, loc: Location) {
        if let (Some(active_loop), Some(active_block)) = (self.active_loop.clone(), self.active_block()) {
            if !self.block_is_terminated(active_block) {
                unsafe {
                    gcc_jit_block_end_with_jump(active_block, self.gccjit_location(loc), active_loop.increment_block)
                }
                self.mark_block_terminated(active_block);
            }
        }
    }

    pub(crate) fn compile_break_statement(&mut self, loc: Location) {
        if let (Some(active_loop), Some(active_block)) = (self.active_loop.clone(), self.active_block()) {
            if !self.block_is_terminated(active_block) {
                unsafe { gcc_jit_block_end_with_jump(active_block, self.gccjit_location(loc), active_loop.loop_end) }
                self.mark_block_terminated(active_block);
            }
        }
    }

    pub(crate) fn compile_if_statement(&mut self, scope: ScopeRef, statement: If) {
        let guard = self.block_func_ref.lock().unwrap();

        if let (Some(active_block), Some(func)) = (guard.block, guard.func) {
            drop(guard);

            // Build blocks
            let true_block_name = CString::new(format!("true_block_{}", generate_random_hex())).unwrap();
            let false_block_name = CString::new(format!("false_block_{}", generate_random_hex())).unwrap();
            let final_block_name = CString::new(format!("final_block_{}", generate_random_hex())).unwrap();
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
                    CString::new(format!("else_if_true_block_{}", generate_random_hex())).unwrap();
                let else_if_false_block_name =
                    CString::new(format!("else_if_false_block_{}", generate_random_hex())).unwrap();

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

    pub(crate) fn compile_for_statement(&mut self, scope: ScopeRef, statement: For) {
        let loc = self.gccjit_location(statement.loc.clone());
        let guard = self.block_func_ref.lock().unwrap();

        if let (Some(active_block), Some(func)) = (guard.block, guard.func) {
            drop(guard);

            // Create blocks
            let for_body_name = CString::new(format!("for_body_block_{}", generate_random_hex())).unwrap();
            let for_end_name = CString::new(format!("for_end_block_{}", generate_random_hex())).unwrap();
            let for_increment_name = CString::new(format!("for_increment_block_{}", generate_random_hex())).unwrap();
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

    pub(crate) fn compile_return(&mut self, scope: ScopeRef, statement: Return) {
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
}
