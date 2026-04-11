/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::{BodyID, GenericParamID, decls::*, stmts::TypedGenericParam};
use std::sync::RwLock;

#[derive(Debug)]
pub struct DeclTablesRegistry {
    tables: RwLock<DeclTables>,
}

#[derive(Debug)]
pub struct DeclTables {
    pub structs: Vec<StructDecl>,
    pub enums: Vec<EnumDecl>,
    pub unions: Vec<UnionDecl>,
    pub funcs: Vec<FuncDecl>,
    pub methods: Vec<MethodDecl>,
    pub interfaces: Vec<InterfaceDecl>,
    pub global_vars: Vec<GlobalVarDecl>,
    pub vars: Vec<VarDecl>,
    pub typedefs: Vec<TypedefDecl>,
    pub generic_params: Vec<TypedGenericParam>,

    // Holds body block of the generic functions and methods.
    pub bodies: Vec<TypedBlockStmt>,
}

impl DeclTablesRegistry {
    pub fn new() -> Self {
        Self {
            tables: RwLock::new(DeclTables {
                structs: Vec::new(),
                enums: Vec::new(),
                unions: Vec::new(),
                funcs: Vec::new(),
                methods: Vec::new(),
                interfaces: Vec::new(),
                global_vars: Vec::new(),
                vars: Vec::new(),
                typedefs: Vec::new(),
                generic_params: Vec::new(),
                bodies: Vec::new(),
            }),
        }
    }
}

impl DeclTablesRegistry {
    #[inline]
    pub fn insert_struct(&self, decl: StructDecl) -> StructDeclID {
        let mut tables = self.tables.write().unwrap();
        let id = StructDeclID(tables.structs.len() as u32);
        tables.structs.push(decl);
        id
    }

    #[inline]
    pub fn insert_enum(&self, decl: EnumDecl) -> EnumDeclID {
        let mut tables = self.tables.write().unwrap();
        let id = EnumDeclID(tables.enums.len() as u32);
        tables.enums.push(decl);
        id
    }

    #[inline]
    pub fn insert_union(&self, decl: UnionDecl) -> UnionDeclID {
        let mut tables = self.tables.write().unwrap();
        let id = UnionDeclID(tables.unions.len() as u32);
        tables.unions.push(decl);
        id
    }

    #[inline]
    pub fn insert_func(&self, decl: FuncDecl) -> FuncDeclID {
        let mut tables = self.tables.write().unwrap();
        let id = FuncDeclID(tables.funcs.len() as u32);
        tables.funcs.push(decl);
        id
    }

    #[inline]
    pub fn insert_method(&self, decl: MethodDecl) -> MethodDeclID {
        let mut tables = self.tables.write().unwrap();
        let id = MethodDeclID(tables.methods.len() as u32);
        tables.methods.push(decl);
        id
    }

    #[inline]
    pub fn insert_interface(&self, decl: InterfaceDecl) -> InterfaceDeclID {
        let mut tables = self.tables.write().unwrap();
        let id = InterfaceDeclID(tables.interfaces.len() as u32);
        tables.interfaces.push(decl);
        id
    }

    #[inline]
    pub fn insert_global_var(&self, decl: GlobalVarDecl) -> GlobalVarDeclID {
        let mut tables = self.tables.write().unwrap();
        let id = GlobalVarDeclID(tables.global_vars.len() as u32);
        tables.global_vars.push(decl);
        id
    }

    #[inline]
    pub fn insert_var(&self, decl: VarDecl) -> VarDeclID {
        let mut tables = self.tables.write().unwrap();
        let id = VarDeclID(tables.vars.len() as u32);
        tables.vars.push(decl);
        id
    }

    #[inline]
    pub fn insert_typedef(&self, decl: TypedefDecl) -> TypedefDeclID {
        let mut tables = self.tables.write().unwrap();
        let id = TypedefDeclID(tables.typedefs.len() as u32);
        tables.typedefs.push(decl);
        id
    }

    #[inline]
    pub fn insert_generic_param(&self, generic_param: TypedGenericParam) -> GenericParamID {
        let mut tables = self.tables.write().unwrap();
        let id = GenericParamID(tables.generic_params.len() as u32);
        tables.generic_params.push(generic_param);
        id
    }

    #[inline]
    pub fn insert_body(&self, body: TypedBlockStmt) -> BodyID {
        let mut tables = self.tables.write().unwrap();
        let id = BodyID(tables.bodies.len() as u32);
        tables.bodies.push(body);
        id
    }
}

impl DeclTablesRegistry {
    #[inline]
    pub fn struct_decl(&self, id: StructDeclID) -> StructDecl {
        let tables = self.tables.read().unwrap();
        tables.structs[id.0 as usize].clone()
    }

    #[inline]
    pub fn enum_decl(&self, id: EnumDeclID) -> EnumDecl {
        let tables = self.tables.read().unwrap();
        tables.enums[id.0 as usize].clone()
    }

    #[inline]
    pub fn union_decl(&self, id: UnionDeclID) -> UnionDecl {
        let tables = self.tables.read().unwrap();
        tables.unions[id.0 as usize].clone()
    }

    #[inline]
    pub fn func_decl(&self, id: FuncDeclID) -> FuncDecl {
        let tables = self.tables.read().unwrap();
        tables.funcs[id.0 as usize].clone()
    }

    #[inline]
    pub fn method_decl(&self, id: MethodDeclID) -> MethodDecl {
        let tables = self.tables.read().unwrap();
        tables.methods[id.0 as usize].clone()
    }

    #[inline]
    pub fn interface_decl(&self, id: InterfaceDeclID) -> InterfaceDecl {
        let tables = self.tables.read().unwrap();
        tables.interfaces[id.0 as usize].clone()
    }

    #[inline]
    pub fn global_var_decl(&self, id: GlobalVarDeclID) -> GlobalVarDecl {
        let tables = self.tables.read().unwrap();
        tables.global_vars[id.0 as usize].clone()
    }

    #[inline]
    pub fn var_decl(&self, id: VarDeclID) -> VarDecl {
        let tables = self.tables.read().unwrap();
        tables.vars[id.0 as usize].clone()
    }

    #[inline]
    pub fn typedef_decl(&self, id: TypedefDeclID) -> TypedefDecl {
        let tables = self.tables.read().unwrap();
        tables.typedefs[id.0 as usize].clone()
    }

    #[inline]
    pub fn generic_param(&self, id: GenericParamID) -> TypedGenericParam {
        let tables = self.tables.read().unwrap();
        tables.generic_params[id.0 as usize].clone()
    }

    #[inline]
    pub fn body(&self, id: BodyID) -> TypedBlockStmt {
        let tables = self.tables.read().unwrap();
        tables.bodies[id.0 as usize].clone()
    }
}

impl DeclTablesRegistry {
    #[inline]
    pub fn with_struct_decl_mut<R>(&self, id: StructDeclID, f: impl FnOnce(&mut StructDecl) -> R) -> R {
        let mut tables = self.tables.write().unwrap();
        f(&mut tables.structs[id.0 as usize])
    }

    #[inline]
    pub fn with_enum_decl_mut<R>(&self, id: EnumDeclID, f: impl FnOnce(&mut EnumDecl) -> R) -> R {
        let mut tables = self.tables.write().unwrap();
        f(&mut tables.enums[id.0 as usize])
    }

    #[inline]
    pub fn with_union_decl_mut<R>(&self, id: UnionDeclID, f: impl FnOnce(&mut UnionDecl) -> R) -> R {
        let mut tables = self.tables.write().unwrap();
        f(&mut tables.unions[id.0 as usize])
    }

    #[inline]
    pub fn with_func_decl_mut<R>(&self, id: FuncDeclID, f: impl FnOnce(&mut FuncDecl) -> R) -> R {
        let mut tables = self.tables.write().unwrap();
        f(&mut tables.funcs[id.0 as usize])
    }

    #[inline]
    pub fn with_method_decl_mut<R>(&self, id: MethodDeclID, f: impl FnOnce(&mut MethodDecl) -> R) -> R {
        let mut tables = self.tables.write().unwrap();
        f(&mut tables.methods[id.0 as usize])
    }

    #[inline]
    pub fn with_interface_decl_mut<R>(&self, id: InterfaceDeclID, f: impl FnOnce(&mut InterfaceDecl) -> R) -> R {
        let mut tables = self.tables.write().unwrap();
        f(&mut tables.interfaces[id.0 as usize])
    }

    #[inline]
    pub fn with_global_var_decl_mut<R>(&self, id: GlobalVarDeclID, f: impl FnOnce(&mut GlobalVarDecl) -> R) -> R {
        let mut tables = self.tables.write().unwrap();
        f(&mut tables.global_vars[id.0 as usize])
    }

    #[inline]
    pub fn with_var_decl_mut<R>(&self, id: VarDeclID, f: impl FnOnce(&mut VarDecl) -> R) -> R {
        let mut tables = self.tables.write().unwrap();
        f(&mut tables.vars[id.0 as usize])
    }

    #[inline]
    pub fn with_typedef_decl_mut<R>(&self, id: TypedefDeclID, f: impl FnOnce(&mut TypedefDecl) -> R) -> R {
        let mut tables = self.tables.write().unwrap();
        f(&mut tables.typedefs[id.0 as usize])
    }

    #[inline]
    pub fn with_body_mut<R>(&self, id: BodyID, f: impl FnOnce(&mut TypedBlockStmt) -> R) -> R {
        let mut tables = self.tables.write().unwrap();
        f(&mut tables.bodies[id.0 as usize])
    }
}
