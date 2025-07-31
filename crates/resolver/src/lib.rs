pub mod declsign;
pub mod scope;
mod tests;

use std::collections::HashMap;
use typed_ast::{ModuleID, TypedASTModule, TypedBlockStatement, TypedFuncDef};

use crate::{
    declsign::FuncSig,
    scope::{ModuleSymbolTable, ResolvedFunction, SymbolEntry},
};

pub struct Resolver {
    pub global_symbols: HashMap<ModuleID, ModuleSymbolTable>,
    pub current_module: Option<ModuleID>,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            global_symbols: HashMap::new(),
            current_module: None,
        }
    }

    pub fn resolve_module(&mut self, module_id: ModuleID, ast: &TypedASTModule) {
        self.current_module = Some(module_id);

        let mut symbol_table = ModuleSymbolTable {
            symbols: HashMap::new(),
            parent: None,
        };

        // Collect function declarations
        for stmt in &ast.body {
            match stmt {
                typed_ast::TypedStatement::Import(typed_import) => todo!(),
                typed_ast::TypedStatement::Typedef(typed_typedef) => todo!(),
                typed_ast::TypedStatement::GlobalVariable(typed_global_variable) => todo!(),
                typed_ast::TypedStatement::FuncDef(typed_func_def) => {
                    let func_sig = FuncSig {
                        name: typed_func_def.name.clone(),
                        params: typed_func_def.params.clone(),
                        return_type: typed_func_def.return_type.clone(),
                        vis: typed_func_def.vis.clone(),
                        loc: typed_func_def.loc.clone(),
                    };

                    let resolved_fn = ResolvedFunction {
                        module_id,
                        func_sig: func_sig.clone(),
                        body: Some(*typed_func_def.body.clone()),
                    };

                    symbol_table
                        .symbols
                        .insert(func_sig.name, SymbolEntry::Func(resolved_fn));
                }
                typed_ast::TypedStatement::FuncDecl(typed_func_decl) => todo!(),
                typed_ast::TypedStatement::Struct(typed_struct) => todo!(),
                typed_ast::TypedStatement::Enum(typed_enum) => todo!(),
                _ => unreachable!(),
            }
        }

        self.global_symbols.insert(module_id, symbol_table);
    }

    pub fn lookup_symbol(&self, module_id: &ModuleID, name: &str) -> Option<&SymbolEntry> {
        self.global_symbols
            .get(module_id)
            .and_then(|table| table.symbols.get(name))
    }
}
