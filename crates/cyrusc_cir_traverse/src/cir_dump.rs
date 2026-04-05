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

use cyrusc_ast::operators::UnaryOperator;
use cyrusc_diagcentral::exit_with_msg;
use cyrusc_internal::cir::{cir::*, types::CIRType};
use std::{fs, path::PathBuf};

pub struct CIRPrinter {
    out: String,
    indent: usize,
}

pub fn process_cir_dump_for_program_trees(program_trees: &[Box<CIRProgramTree>], output_path: PathBuf) {
    debug_assert!(output_path.is_dir());

    for program_tree in program_trees {
        let mut printer = CIRPrinter::new();
        let dump = printer.print_program_tree(program_tree);

        // derive file name: <module>.cir
        let file_name = format!("{}.cir", program_tree.module_name);

        let file_path = output_path.join(file_name);

        if let Err(err) = fs::write(&file_path, dump) {
            exit_with_msg!(format!(
                "Failed to write CIR dump for module '{}' to '{}': {}",
                program_tree.module_name,
                file_path.display(),
                err
            ));
        }
    }
}

impl CIRPrinter {
    pub fn new() -> Self {
        Self {
            out: String::new(),
            indent: 0,
        }
    }
}

impl CIRPrinter {
    pub fn print_program_tree(&mut self, program_tree: &CIRProgramTree) -> &str {
        for stmt in &program_tree.body {
            self.print_stmt(stmt);
        }

        &self.out
    }

    fn print_stmt(&mut self, stmt: &CIRStmt) {
        match stmt {
            CIRStmt::Variable(var) => self.print_var_stmt(var),
            CIRStmt::GlobalVar(global_var) => self.print_global_var_stmt(global_var),
            CIRStmt::FuncDef(func_def_stmt) => self.print_func_def(func_def_stmt),
            CIRStmt::FuncDecl(func_decl_stmt) => self.print_func_decl(func_decl_stmt),
            CIRStmt::Block(block) => self.print_block(block),
            CIRStmt::Struct(struct_stmt) => self.print_struct(struct_stmt),
            CIRStmt::Enum(enum_stmt) => self.print_enum(enum_stmt),
            CIRStmt::Union(union_stmt) => self.print_union(union_stmt),
            CIRStmt::Expr(expr) => {
                let expr = self.print_expr(expr);
                self.push_line(format!("{expr};"));
            }
            CIRStmt::If(if_stmt) => self.print_if(if_stmt),
            CIRStmt::For(for_stmt) => self.print_for(for_stmt),
            CIRStmt::While(while_stmt) => self.print_while(while_stmt),
            CIRStmt::Switch(switch) => self.print_switch(switch),
            CIRStmt::SwitchOnEnum(switch_on_enum) => self.print_switch_enum(switch_on_enum),
            CIRStmt::Return(return_stmt) => self.print_return(return_stmt),
            CIRStmt::Label(label) => self.push_line(format!("{}:", label.name)),
            CIRStmt::Goto(goto) => self.push_line(format!("goto L{};", goto.label_id)),
            CIRStmt::Defer(deref) => {
                self.push_line("defer {");
                self.indent();
                self.print_stmt(&deref.operand);
                self.dedent();
                self.push_line("}");
            }
            CIRStmt::Continue(_) => self.push_line("continue;"),
            CIRStmt::Break(_) => self.push_line("break;"),
        }
    }

    fn print_union(&mut self, u: &CIRUnionStmt) {
        self.push_line(format!("union {} {{", u.name));
        self.indent();

        for (idx, (fname, _loc)) in u.fields_info.iter().enumerate() {
            let fty = &u.fields[idx];
            self.push_line(format!("{fname}: {}", self.print_type(fty)));
        }

        self.dedent();
        self.push_line("}");
    }

    fn print_enum(&mut self, e: &CIREnumStmt) {
        self.push_line(format!("enum {} {{", e.name));
        self.indent();

        for variant in &e.variants {
            match variant {
                CIREnumVariant::Unit(name) => {
                    self.push_line(name.clone());
                }

                CIREnumVariant::Valued(name, expr) => {
                    let v = self.print_expr(expr);
                    self.push_line(format!("{name} = {v}"));
                }

                CIREnumVariant::Tuple(name, types) => {
                    let list = types.iter().map(|t| self.print_type(t)).collect::<Vec<_>>().join(", ");

                    self.push_line(format!("{name}({list})"));
                }
            }
        }

        self.dedent();
        self.push_line("}");
    }

    fn print_struct(&mut self, s: &CIRStructStmt) {
        self.push_line(format!("struct {} {{", s.name));
        self.indent();

        for (idx, (fname, _loc)) in s.fields_info.iter().enumerate() {
            let fty = &s.fields[idx];
            self.push_line(format!("{fname}: {}", self.print_type(fty)));
        }

        self.dedent();
        self.push_line("}");
    }

    fn print_for(&mut self, s: &CIRForStmt) {
        let init = s
            .initializer
            .as_ref()
            .map(|v| {
                let expr = v
                    .expr
                    .as_ref()
                    .map(|e| format!(" = {}", self.print_expr(e)))
                    .unwrap_or_default();

                format!("var {}: {}{}", v.name, self.print_type(&v.ty), expr)
            })
            .unwrap_or_default();

        let cond = s.cond.as_ref().map(|c| self.print_expr(c)).unwrap_or_default();

        let inc = s.increment.as_ref().map(|inc| self.print_expr(inc)).unwrap_or_default();

        self.push_line(format!("for ({init}; {cond}; {inc}) {{"));

        self.indent();
        self.print_block(&s.body);
        self.dedent();

        self.push_line("}");
    }

    fn print_switch(&self, switch: &CIRSwitchStmt) {
        todo!();
    }

    fn print_switch_enum(&self, switch_on_enum: &CIRSwitchOnEnumStmt) {
        todo!();
    }

    fn print_var_stmt(&mut self, var: &CIRVarStmt) {
        let init = var
            .expr
            .as_ref()
            .map(|e| format!(" = {}", self.print_expr(e)))
            .unwrap_or_default();

        self.push_line(format!("var {}: {}{};", var.name, self.print_type(&var.ty), init));
    }

    fn print_global_var_stmt(&mut self, v: &CIRGlobalVarStmt) {
        let init = v
            .expr
            .as_ref()
            .map(|e| format!(" = {}", self.print_expr(e)))
            .unwrap_or_default();

        self.push_line(format!("global {}: {}{};", v.name, self.print_type(&v.ty), init));
    }

    fn print_func_def(&mut self, f: &CIRFuncDefStmt) {
        let params = self.print_params(&f.params);

        self.push_line(format!("fn {}({}) {} {{", f.name, params, self.print_type(&f.ret)));

        self.indent();
        self.print_block(&f.body);
        self.dedent();

        self.push_line("}");
    }

    fn print_func_decl(&mut self, f: &CIRFuncDeclStmt) {
        let params = self.print_params(&f.params);

        self.push_line(format!("fn {}({}) {};", f.name, params, self.print_type(&f.ret)));
    }

    fn print_block(&mut self, block: &CIRBlockStmt) {
        for stmt in &block.stmts {
            self.print_stmt(stmt);
        }

        if !block.defers.is_empty() {
            self.push_line("// defers");

            for stmt in &block.defers {
                self.print_stmt(stmt);
            }
        }
    }

    fn print_if(&mut self, s: &CIRIfStmt) {
        let cond = self.print_expr(&s.cond);

        self.push_line(format!("if {} {{", cond));

        self.indent();
        self.print_block(&s.then_block);
        self.dedent();

        self.push_line("}");

        if let Some(else_block) = &s.else_block {
            self.push_line("else {");
            self.indent();
            self.print_block(else_block);
            self.dedent();
            self.push_line("}");
        }
    }

    fn print_while(&mut self, s: &CIRWhileStmt) {
        let cond = self.print_expr(&s.cond);

        self.push_line(format!("while {} {{", cond));

        self.indent();
        self.print_block(&s.body);
        self.dedent();

        self.push_line("}");
    }

    fn print_return(&mut self, return_stmt: &CIRReturnStmt) {
        match &return_stmt.arg {
            Some(expr) => {
                let expr_str = self.print_expr(&expr);
                self.push_line(format!("return {};", expr_str))
            }
            None => self.push_line("return;"),
        }
    }

    fn print_expr(&mut self, expr: &CIRExpr) -> String {
        match &expr.kind {
            CIRExprKind::Load(v) => format!("%{}", v.irv_id.0),
            CIRExprKind::Literal(l) => self.print_literal(l),
            CIRExprKind::Infix(i) => format!("({} {} {})", self.print_expr(&i.lhs), i.op, self.print_expr(&i.rhs)),
            CIRExprKind::Prefix(p) => format!("{}{}", p.op, self.print_expr(&p.operand)),
            CIRExprKind::Call(c) => {
                let args = c.args.iter().map(|a| self.print_expr(a)).collect::<Vec<_>>().join(", ");

                format!("{}({})", self.print_expr(&c.operand), args)
            }
            CIRExprKind::Assign(assign) => {
                format!("{} = {}", self.print_expr(&assign.lhs), self.print_expr(&assign.rhs))
            }
            CIRExprKind::AddrOf(addr_of) => format!("&{}", self.print_expr(&addr_of.operand)),
            CIRExprKind::Deref(deref) => format!("*{}", self.print_expr(&deref.operand)),
            CIRExprKind::Tuple(tuple) => {
                let elements = tuple
                    .elements
                    .iter()
                    .map(|e| self.print_expr(e))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({})", elements)
            }
            CIRExprKind::TupleAccess(tuple_access) => {
                format!("{}.{}", self.print_expr(&tuple_access.operand), tuple_access.index)
            }
            CIRExprKind::StructFieldAccess(field_access) => {
                format!("{}.f{}", self.print_expr(&field_access.operand), field_access.field_idx)
            }
            CIRExprKind::Cast(cast) => format!(
                "cast({}, {})",
                self.print_type(&cast.ty),
                self.print_expr(&cast.operand)
            ),
            CIRExprKind::SizeOf(sizeof) => format!("sizeof({})", self.print_type(&sizeof.ty)),

            CIRExprKind::Unary(unary) => {
                let op = unary.op.to_string();
                let rhs = self.print_expr(&unary.operand);

                match unary.op {
                    UnaryOperator::PreIncrement | UnaryOperator::PreDecrement => {
                        format!("({op}{rhs})")
                    }
                    UnaryOperator::PostIncrement | UnaryOperator::PostDecrement => {
                        format!("({rhs}{op})")
                    }
                }
            }
            CIRExprKind::Array(array) => {
                let elements = array
                    .elements
                    .iter()
                    .map(|e| self.print_expr(e))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{}[{}]", self.print_type(&array.ty), elements)
            }
            CIRExprKind::ArrayIndex(idx) => {
                let base = self.print_expr(&idx.operand);
                let i = self.print_expr(&idx.index);
                format!("{base}[{i}]")
            }
            CIRExprKind::StructInit(struct_init) => {
                let mut parts = Vec::new();

                for (idx, expr) in struct_init.fields.iter().enumerate() {
                    let fname = format!("f{}", idx); // fallback field name
                    let v = self.print_expr(expr);
                    parts.push(format!("{fname}: {v}"));
                }

                let body = parts.join(", ");
                format!(
                    "{} {{ {} }}",
                    self.print_type(&CIRType::Struct(struct_init.ty.clone())),
                    body
                )
            }
            CIRExprKind::UnionInit(union_init) => {
                let inner = self.print_expr(&union_init.expr);
                let t = self.print_type(&union_init.ty);
                format!("{t} {{ {inner} }}")
            }
            CIRExprKind::EnumInit(enum_init) => {
                // FIXME
                todo!();

                // let enum_type = self.print_type(&CIRType::Enum(enum_init.enum_type.clone()));

                // match &enum_init.variant {
                //     CIREnumInitVariant::Ident => format!("{enum_type}.{variant_name}"),

                //     CIREnumInitVariant::Valued(expr) => {
                //         let v = self.print_expr(expr);
                //         format!("{enum_type}.{variant_name}({v})")
                //     }
                //     CIREnumInitVariant::Fielded(exprs) => {
                //         let parts = exprs.iter().map(|e| self.print_expr(e)).collect::<Vec<_>>().join(", ");

                //         format!("{enum_type}.{variant_name}({parts})")
                //     }
                // }
            }

            CIRExprKind::UnionFieldAccess(field_access) => {
                let base = self.print_expr(&field_access.operand);
                format!("{base}.({})", self.print_type(&field_access.field_type))
            }
            CIRExprKind::Lambda(l) => {
                let params = self.print_params(&l.params);
                let ret = self.print_type(&l.ret);
                let id = l.irv_id.0;

                let inline_flag = if l.inline { " inline" } else { "" };

                // header
                let mut s = format!("lambda%{}{} ({}) {} {{", id, inline_flag, params, ret);

                // body
                self.indent();
                let saved_len = self.out.len();
                self.print_block(&l.body);
                let body_str = self.out[saved_len..].to_string();
                self.out.truncate(saved_len);
                self.dedent();

                s.push('\n');
                s.push_str(&body_str);
                s.push('}');

                s
            }
            CIRExprKind::Dynamic(dynamic) => {
                let data = self.print_expr(&dynamic.data_expr);
                let vtable = &dynamic.vtable_abi_name;
                let gid = dynamic.global_var_id.0;

                let mut s = String::new();
                s.push_str("dynamic {\n");

                self.indent();
                s.push_str(&format!("{}data: {}\n", self.indent_str(), data));
                s.push_str(&format!("{}vtable: @{}\n", self.indent_str(), vtable));
                s.push_str(&format!("{}global: %{}\n", self.indent_str(), gid));

                if !dynamic.method_decls.is_empty() {
                    s.push_str(&format!("{}methods {{\n", self.indent_str()));

                    self.indent();
                    for decl in &dynamic.method_decls {
                        let params = self.print_params(&decl.params);
                        let ret = self.print_type(&decl.ret);
                        s.push_str(&format!("{}fn {}({}) {};\n", self.indent_str(), decl.name, params, ret));
                    }
                    self.dedent();

                    s.push_str(&format!("{}}}\n", self.indent_str()));
                }

                self.dedent();
                s.push('}');

                s
            }
        }
    }

    fn print_literal(&self, literal: &CIRLiteral) -> String {
        match &literal.kind {
            CIRLiteralKind::Integer(v, _) => v.to_string(),
            CIRLiteralKind::Float(v) => v.to_string(),
            CIRLiteralKind::Bool(v) => v.to_string(),
            CIRLiteralKind::Char(c) => format!("'{}'", c),
            CIRLiteralKind::Null => "null".into(),
            CIRLiteralKind::CString(s) => format!("\"{}\"", s),
            CIRLiteralKind::ByteString(s) => format!("b\"{}\"", s),
        }
    }

    fn print_params(&self, params: &CIRFuncParams) -> String {
        let mut list = Vec::new();

        for p in &params.list {
            let name = p.name.as_deref().unwrap_or("_");
            list.push(format!("{}: {}", name, self.print_type(&p.ty)));
        }

        if params.is_var {
            list.push("...".into());
        }

        list.join(", ")
    }

    fn print_type(&self, ty: &CIRType) -> String {
        format!("{:?}", ty)
    }
}

// Helpers.
impl CIRPrinter {
    fn push_line(&mut self, s: impl AsRef<str>) {
        for _ in 0..self.indent {
            self.out.push_str("    ");
        }

        self.out.push_str(s.as_ref());
        self.out.push('\n');
    }

    #[inline]
    fn indent_str(&self) -> String {
        // 4 spaces * indent level
        let count = self.indent;
        let mut s = String::with_capacity(count * 4);
        for _ in 0..count {
            s.push_str("    ");
        }
        s
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        self.indent -= 1;
    }
}
