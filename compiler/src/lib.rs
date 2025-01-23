use ast::{
    ast::*,
    token::{Location, TokenKind},
};
use builtins::macros::retrieve_builtin_func;
use gccjit_sys::*;
use parser::{parse_program, Parser};
use rand::{distributions::Alphanumeric, Rng};
use scope::{Scope, ScopeRef};
use std::{
    cell::RefCell,
    collections::HashMap,
    env::current_dir,
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
mod output;
mod scope;
mod types;

// Tracks the current GCC JIT block and function being compiled.
struct BlockFuncPair {
    block: Option<*mut gcc_jit_block>,
    func: Option<*mut gcc_jit_function>,
}

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

pub struct Compiler {
    file_name: String,
    file_path: String,
    program: Program,
    context: *mut gcc_jit_context,
    func_table: RefCell<HashMap<String, *mut gcc_jit_function>>,
    global_vars_table: RefCell<HashMap<String, *mut gcc_jit_lvalue>>,
    param_table: RefCell<HashMap<*mut gcc_jit_function, FuncParamsRecords>>,
    block_func_ref: Arc<Mutex<Box<BlockFuncPair>>>,
    terminated_blocks: Vec<*mut gcc_jit_block>,
    parent_block: Option<*mut gcc_jit_block>,
    active_loop: Option<LoopBlockPair>,
}

impl Compiler {
    pub fn new_block_name(&mut self) -> String {
        let rng = rand::thread_rng();
        let rand_string: String = rng.sample_iter(&Alphanumeric).take(10).map(char::from).collect();
        rand_string
    }

    pub fn new(program: Program, file_path: String, file_name: String) -> Self {
        let context = unsafe { gcc_jit_context_acquire() };

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
            Statement::FuncDef(function) => self.compile_func_def(scope, function.clone()),
            Statement::If(statement) => self.compile_if_statement(scope, statement),
            Statement::For(statement) => self.compile_for_statement(scope, statement),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Import(statement) => self.compile_import(statement),
            Statement::Return(statement) => self.compile_return(scope, statement),
            Statement::Break(loc) => self.compile_break_statement(loc),
            Statement::Continue(loc) => self.compile_continue_statement(loc),
            Statement::BlockStatement(statement) => self.compile_statements(
                Rc::new(RefCell::new(scope.borrow_mut().clone_immutable())),
                statement.body,
            ),
            _ => compiler_error!(format!("Invalid statement: {:?}", stmt)),
        }
    }

    fn compile_import(&mut self, import: Import) {
        let file_path = self.file_path.clone();
        let dir_path = Path::new(&file_path).parent().unwrap().to_str().unwrap();

        for sb in import.sub_packages {
            let package_file_name = if sb.is_relative {
                todo!()
            } else {
                format!("{}.cy", sb.package_name.name)
            };
            let package_file_path = format!("{}/{}", dir_path, package_file_name);
            let object_file_path = format!(
                "{}/{}_{}{}",
                dir_path,
                self.new_block_name(),
                sb.package_name.name,
                ".o"
            );

            let (program, file_name) = parse_program(package_file_path.clone());
            let compiler = Compiler::new(program, package_file_path, file_name);
            compiler.make_object_file(object_file_path.clone());

            remove_file(object_file_path).unwrap();

            // TODO
            // Add genereated object file to GCCJTT compiler arguments (linkage stage)
        }
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
                    Rc::clone(&scope).borrow_mut().insert(initializer.name, init_lvalue);
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

            let mut variable_type: *mut gcc_jit_type = null_mut();
            let mut rvalue = null_mut();

            if let Some(token) = variable.ty.clone() {
                variable_type = Compiler::token_as_data_type(self.context, token);
            }

            if let Some(expr) = variable.expr {
                rvalue = match expr {
                    Expression::Array(array) => self.compile_array(Rc::clone(&scope), array, variable_type),
                    _ => self.compile_expression(Rc::clone(&scope), expr),
                };

                if variable.ty.is_none() {
                    variable_type = unsafe { gcc_jit_rvalue_get_type(rvalue) };
                }
            }

            if variable_type.is_null() {
                compiler_error!("Undefined behaviour in variable declaration. Explicit type definition required.");
            }

            let name = CString::new(variable.name.clone()).unwrap();
            let lvalue = unsafe {
                gcc_jit_function_new_local(
                    func,
                    self.gccjit_location(variable.loc.clone()),
                    variable_type,
                    name.as_ptr(),
                )
            };

            if !rvalue.is_null() {
                unsafe {
                    gcc_jit_block_add_assignment(block, self.gccjit_location(variable.loc.clone()), lvalue, rvalue)
                };
            }

            scope.borrow_mut().insert(variable.name, lvalue);
        } else {
            compiler_error!("Invalid usage of local variable.");
        }
    }

    fn compile_func_def(&mut self, scope: ScopeRef, func_def: FuncDef) {
        let func_type = match func_def.vis_type {
            FuncVisType::Extern => gcc_jit_function_kind::GCC_JIT_FUNCTION_EXPORTED,
            FuncVisType::Pub => gcc_jit_function_kind::GCC_JIT_FUNCTION_EXPORTED,
            FuncVisType::Internal => gcc_jit_function_kind::GCC_JIT_FUNCTION_INTERNAL,
            FuncVisType::Inline => gcc_jit_function_kind::GCC_JIT_FUNCTION_ALWAYS_INLINE,
        };

        let mut ret_type: *mut gcc_jit_type = Compiler::void_type(self.context);

        if let Some(token) = func_def.return_type {
            ret_type = Compiler::token_as_data_type(self.context, token.kind);
        }

        let mut params: Vec<*mut gcc_jit_param> = Vec::new();
        let mut func_param_records = FuncParamsRecords::new();

        for (idx, func_def_param) in func_def.params.iter().enumerate() {
            let name = CString::new(func_def_param.identifier.name.clone()).unwrap();

            let ty_token = if let Some(user_def) = &func_def_param.ty {
                user_def
            } else {
                &TokenKind::Void
            };

            let ty = Compiler::token_as_data_type(self.context, ty_token.clone());

            let param = unsafe {
                gcc_jit_context_new_param(
                    self.context,
                    self.gccjit_location(func_def_param.loc.clone()),
                    ty,
                    name.as_ptr(),
                )
            };

            params.push(param);

            func_param_records.push(FuncParamRecord {
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
                ret_type,
                func_name.as_ptr(),
                params.len().try_into().unwrap(),
                params.as_mut_ptr(),
                0,
            )
        };

        self.param_table.borrow_mut().insert(func, func_param_records);

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

        if !return_compiled {
            compiler_error!(format!(
                "Explicit return statement required for the function '{}'.",
                func_def.name
            ));
        }

        self.func_table.borrow_mut().insert(func_def.name, func);
    }

    fn load_lvalue_rvalue(
        &mut self,
        scope: ScopeRef,
        identifier: Identifier,
    ) -> (*mut gcc_jit_lvalue, *mut gcc_jit_rvalue) {
        match scope.borrow_mut().get(identifier.name.clone()) {
            Some(lvalue) => {
                let rvalue = unsafe { gcc_jit_lvalue_as_rvalue(*lvalue.borrow_mut()) };
                return (*lvalue.borrow_mut(), rvalue);
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
        self.load_lvalue_rvalue(scope, identifier).1
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
                    *variable.borrow_mut(),
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
                let lvalue =
                    self.array_dimension_as_lvalue(Rc::clone(&scope), *variable.borrow_mut(), array_index.dimensions);

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
        let (lvalue, _) = self.load_lvalue_rvalue(Rc::clone(&scope), assignment.identifier);

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

    fn compile_func_call(&mut self, scope: ScopeRef, func_call: FunctionCall) -> *mut gcc_jit_rvalue {
        let guard = self.block_func_ref.lock().unwrap();

        if let Some(block) = guard.block {
            drop(guard);

            let mut args: Vec<*mut gcc_jit_rvalue> = Vec::new();

            for arg_expr in func_call.arguments {
                let arg_rvalue = self.compile_expression(Rc::clone(&scope), arg_expr);
                args.push(arg_rvalue);
            }

            let loc = self.gccjit_location(func_call.loc.clone());
            let func_table = self.func_table.borrow_mut();

            match func_table.get(&func_call.function_name.name) {
                Some(func) => unsafe {
                    let rvalue = gcc_jit_context_new_call(
                        self.context,
                        loc.clone(),
                        *func,
                        args.len().try_into().unwrap(),
                        args.as_mut_ptr(),
                    );

                    gcc_jit_block_add_eval(block, loc, rvalue);

                    rvalue
                },
                None => match retrieve_builtin_func(func_call.function_name.name.clone()) {
                    Some(func_def) => func_def(self.context, block, &mut args),
                    None => compiler_error!(format!(
                        "Function '{}' not defined at this module.",
                        func_call.function_name.name
                    )),
                },
            }
        } else {
            compiler_error!("Calling any function at top-level nodes isn't allowed.");
        }
    }

    fn compile_unary_operator(&mut self, scope: ScopeRef, unary_operator: UnaryOperator) -> *mut gcc_jit_rvalue {
        let loc = self.gccjit_location(unary_operator.loc.clone());

        match scope.borrow_mut().get(unary_operator.identifer.name.clone()) {
            Some(lvalue) => {
                let rvalue = unsafe { gcc_jit_lvalue_as_rvalue(*lvalue.borrow_mut()) };
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
                                *lvalue.borrow_mut(),
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
            },
            Literal::Float(float_literal) => match float_literal {
                FloatLiteral::F32(value) => unsafe {
                    gcc_jit_context_new_rvalue_from_double(self.context, Compiler::f32_type(self.context), value as f64)
                },
                FloatLiteral::F64(value) => unsafe {
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
            Literal::Char(char_literal) => todo!(),
        }
    }
}

impl Drop for Compiler {
    fn drop(&mut self) {
        unsafe { gcc_jit_context_release(self.context) };
    }
}
