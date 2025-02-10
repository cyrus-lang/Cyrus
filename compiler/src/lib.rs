use ast::ast::*;
use control_flow::LoopBlockPair;
use funcs::{FuncMetadata, FuncParamsRecords};
use gccjit_sys::*;
use options::CompilerOptions;
use scope::{Scope, ScopeRef};
use std::{
    cell::RefCell,
    collections::HashMap,
    ffi::CString,
    fs::remove_file,
    rc::Rc,
    sync::{Arc, Mutex},
};
use structs::StructMetadata;

mod blocks;
mod context;
mod control_flow;
mod expressions;
mod funcs;
mod import;
mod location;
pub mod options;
mod output;
mod scope;
mod stdlib;
mod structs;
mod types;
mod variables;

// Tracks the current GCC JIT block and function being compiled.
struct BlockFuncPair {
    block: Option<*mut gcc_jit_block>,
    func: Option<*mut gcc_jit_function>,
}

pub struct Compiler {
    file_name: String,
    file_path: String,
    program: Program,
    context: *mut gcc_jit_context,
    func_table: RefCell<HashMap<String, FuncMetadata>>,
    global_struct_table: RefCell<HashMap<String, StructMetadata>>,
    #[allow(dead_code)] // FIXME
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

        // Explicit C_STDLIB link
        let optname = CString::new(format!("-lc")).unwrap();
        unsafe { gcc_jit_context_add_driver_option(self.context, optname.as_ptr()) };
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

    pub(crate) fn compile_statements(&mut self, scope: ScopeRef, stmts: Vec<Statement>) {
        for stmt in stmts {
            self.compile_statement(Rc::clone(&scope), stmt.clone());
        }
    }

    pub(crate) fn compile_statement(&mut self, scope: ScopeRef, stmt: Statement) {
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
}

impl Drop for Compiler {
    fn drop(&mut self) {
        unsafe { gcc_jit_context_release(self.context) };

        for item in self.compiled_object_files.clone() {
            remove_file(item).unwrap();
        }
    }
}
