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
use cyrusc_strescape::escape_string;
use std::{fs, path::PathBuf};

pub struct CIRPrinter<'a> {
    module: &'a CIRModule,
    out: String,
    indent: usize,
}

pub fn process_cir_dump_for_modules(modules: &[Box<CIRModule>], output_path: PathBuf) {
    debug_assert!(output_path.is_dir());

    for module in modules {
        let mut printer = CIRPrinter::new(module);
        let dump = printer.print_module();

        // derive file name: <module>.cir
        let file_name = format!("{}.cir", module.module_name);

        let file_path = output_path.join(file_name);

        if let Err(err) = fs::write(&file_path, dump) {
            exit_with_msg!(format!(
                "Failed to write CIR dump for module '{}' to '{}': {}",
                module.module_name,
                file_path.display(),
                err
            ));
        }
    }
}

impl<'a> CIRPrinter<'a> {
    pub fn new(module: &'a CIRModule) -> Self {
        Self {
            module,
            out: String::new(),
            indent: 0,
        }
    }
}

impl<'a> CIRPrinter<'a> {
    pub fn print_module(&mut self) -> &str {
        for stmt in &self.module.stmts {
            self.print_stmt(stmt);
            self.push_line("");
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
            CIRStmt::Expr(expr) => {
                let expr_str = self.print_expr(expr);

                if expr_str.contains('\n') {
                    for (i, line) in expr_str.lines().enumerate() {
                        if i == 0 {
                            self.push_line(line.to_string());
                        } else {
                            self.push_line(line.to_string());
                        }
                    }
                } else {
                    // normal single-line expression
                    self.push_line(format!("{expr_str}"));
                }
            }
            CIRStmt::If(if_stmt) => self.print_if(if_stmt),
            CIRStmt::For(for_stmt) => self.print_for(for_stmt),
            CIRStmt::While(while_stmt) => self.print_while(while_stmt),
            CIRStmt::Switch(switch) => self.print_switch(switch),
            CIRStmt::SwitchOnEnum(switch_on_enum) => self.print_switch_enum(switch_on_enum),
            CIRStmt::Return(return_stmt) => self.print_return(return_stmt),
            CIRStmt::Label(label) => self.push_line(format!("{}:", label.name)),
            CIRStmt::Goto(goto) => self.push_line(format!("goto L{}", goto.label_id)),
            CIRStmt::Defer(deref) => {
                self.push_line("defer {");
                self.indent();
                self.print_stmt(&deref.operand);
                self.dedent();
                self.push_line("}");
            }
            CIRStmt::Continue(_) => self.push_line("continue"),
            CIRStmt::Break(_) => self.push_line("break"),
        }
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

    // TODO
    fn print_switch(&self, switch: &CIRSwitchStmt) {
        todo!();
    }

    // TODO
    fn print_switch_enum(&self, switch_on_enum: &CIRSwitchOnEnumStmt) {
        todo!();
    }

    fn print_var_stmt(&mut self, var: &CIRVarStmt) {
        let ty = self.print_type(&var.ty);

        match &var.expr {
            None => {
                // Simple no-init var
                self.push_line(format!("%{}: {}", var.irv_id.0, ty));
            }

            Some(expr) => {
                let expr_str = self.print_expr(expr);

                if !expr_str.contains('\n') {
                    // single-line initializer (normal case)
                    self.push_line(format!("%{}: {} = {}", var.irv_id.0, ty, expr_str));
                } else {
                    // multi-line initializer
                    self.push_line(format!("%{}: {} =", var.irv_id.0, ty));

                    self.indent();
                    for line in expr_str.lines() {
                        self.push_line(line.to_string());
                    }
                    self.dedent();
                }
            }
        }
    }

    fn print_global_var_stmt(&mut self, global_var: &CIRGlobalVarStmt) {
        let init = global_var
            .expr
            .as_ref()
            .map(|e| format!(" = {}", self.print_expr(e)))
            .unwrap_or_default();

        let ty = self.print_type(&global_var.ty);

        self.push_line(format!("global {}: {}{}", global_var.name, ty, init));
    }

    fn print_func_def(&mut self, func: &CIRFuncDefStmt) {
        let params = self.print_params(&func.params);

        let ret_type = self.print_type(&func.ret_type);
        self.push_line(format!("fn {}({params}) {ret_type} {{", func.name));

        self.indent();
        self.print_block(&func.body);
        self.dedent();

        self.push_line("}");
    }

    fn print_func_decl(&mut self, func_decl: &CIRFuncDeclStmt) {
        let params = self.print_params(&func_decl.params);

        let ret = self.print_type(&func_decl.ret_type);
        self.push_line(format!("fn {}({}) {}", func_decl.name, params, ret));
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
                self.push_line(format!("return {}", expr_str))
            }
            None => self.push_line("return"),
        }
    }

    fn print_expr(&mut self, expr: &CIRExpr) -> String {
        match &expr.kind {
            CIRExprKind::Load(value_ref) => match &value_ref.kind {
                CIRValueKind::Func => {
                    let func_decl = self.module.func_decls.get(&value_ref.irv_id).unwrap();
                    func_decl.name.clone()
                }
                CIRValueKind::GlobalVar => {
                    let global_var_decl = self.module.global_var_decls.get(&value_ref.irv_id).unwrap();
                    global_var_decl.name.clone()
                }
                CIRValueKind::LocalVariable => {
                    format!("%{}", value_ref.irv_id.0)
                }
            },
            CIRExprKind::Literal(literal) => self.print_literal(literal),
            CIRExprKind::Infix(infix) => format!(
                "({} {} {})",
                self.print_expr(&infix.lhs),
                infix.op,
                self.print_expr(&infix.rhs)
            ),
            CIRExprKind::Prefix(prefix) => format!("{}{}", prefix.op, self.print_expr(&prefix.operand)),
            CIRExprKind::Call(call) => {
                let mut args = call.args.iter().map(|a| self.print_expr(a)).collect::<Vec<_>>();

                let dispatch = match &call.dispatch {
                    CIRCallDispatch::Normal { abi_name, .. } => abi_name.clone(),
                    CIRCallDispatch::FunctionPointer { operand } => self.print_expr(operand),
                    CIRCallDispatch::Interface {
                        operand,
                        method_idx,
                        methods_len,
                        func_type: _,
                    } => {
                        let obj = self.print_expr(operand);
                        format!("iface_call({}, method={}/{})", obj, method_idx, methods_len)
                    }
                    CIRCallDispatch::Method {
                        self_meta, abi_name, ..
                    } => {
                        if let Some(self_meta) = self_meta {
                            args.insert(0, self.print_expr(&self_meta.operand))
                        }
                        
                        abi_name.clone()
                    }
                };

                format!("{}({})", dispatch, args.join(", "))
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
            CIRExprKind::FieldAccess(field_access) => match &field_access.kind {
                CIRFieldAccessKind::Struct { index, .. } => {
                    format!("{}.{}", self.print_expr(&field_access.operand), index)
                }
                CIRFieldAccessKind::Union { field_type } => {
                    format!(
                        "{}.{}",
                        self.print_expr(&field_access.operand),
                        self.print_type(&field_type)
                    )
                }
            },
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

                for expr in struct_init.fields.iter() {
                    let expr = self.print_expr(expr);
                    parts.push(expr);
                }

                let body = parts.join(", ");
                format!("struct {{ {body} }}")
            }
            CIRExprKind::UnionInit(union_init) => {
                let value = self.print_expr(&union_init.expr);

                format!("union {{ {value} }}")
            }
            CIRExprKind::EnumInit(enum_init) => {
                let variant_name = enum_init
                    .enum_type
                    .variants
                    .get(enum_init.tag)
                    .map(|variant| match variant {
                        CIREnumVariant::Unit(name)
                        | CIREnumVariant::Valued(name, _)
                        | CIREnumVariant::Tuple(name, _) => name.clone(),
                    })
                    .unwrap();

                match &enum_init.variant {
                    CIREnumInitVariant::Ident => {
                        format!(".{variant_name}")
                    }
                    CIREnumInitVariant::Valued(_) => {
                        format!(".{variant_name}")
                    }
                    CIREnumInitVariant::Payload(exprs) => {
                        let exprs = exprs.iter().map(|e| self.print_expr(e)).collect::<Vec<_>>().join(", ");

                        format!(".{variant_name}({exprs})")
                    }
                }
            }
            CIRExprKind::Lambda(lambda) => {
                let params = self.print_params(&lambda.params);
                let ret = self.print_type(&lambda.ret);
                let id = lambda.irv_id.0;

                let inline_flag = if lambda.inline { " inline" } else { "" };

                // header
                let mut out = format!("lambda%{}{} ({}) {} {{", id, inline_flag, params, ret);

                // body
                let saved_len = self.out.len();
                self.print_block(&lambda.body);
                let body_str = self.out[saved_len..].to_string();
                self.out.truncate(saved_len);

                // re‑indent body lines
                if !body_str.trim().is_empty() {
                    for line in body_str.lines() {
                        out.push('\n');
                        out.push_str(&self.indent_str());
                        out.push_str(line);
                    }
                } else {
                    out.push('\n');
                    out.push_str(&self.indent_str());
                }

                // closing brace
                out.push('\n');
                out.push('}');

                out
            }
            CIRExprKind::Dynamic(dynamic) => {
                let data = self.print_expr(&dynamic.data_expr);
                let vtable = &dynamic.vtable_abi_name;
                let global_var_id = dynamic.global_var_id.0;

                let mut out = String::new();
                out.push_str("dynamic {\n");

                self.indent();
                out.push_str(&format!("{}data: {}\n", self.indent_str(), data));
                out.push_str(&format!("{}vtable: @{}\n", self.indent_str(), vtable));
                out.push_str(&format!("{}global: %{}\n", self.indent_str(), global_var_id));

                if !dynamic.method_decls.is_empty() {
                    out.push_str(&format!("{}methods {{\n", self.indent_str()));

                    self.indent();
                    for decl in &dynamic.method_decls {
                        let params = self.print_params(&decl.params);
                        let ret = self.print_type(&decl.ret_type);
                        out.push_str(&format!("{}fn {}({}) {};\n", self.indent_str(), decl.name, params, ret));
                    }
                    self.dedent();

                    out.push_str(&format!("{}}}\n", self.indent_str()));
                }

                self.dedent();
                out.push('}');

                out
            }
        }
    }

    fn print_literal(&self, literal: &CIRLiteral) -> String {
        match &literal.kind {
            CIRLiteralKind::Integer(v, _) => v.to_string(),
            CIRLiteralKind::Float(v) => v.to_string(),
            CIRLiteralKind::Bool(v) => v.to_string(),
            CIRLiteralKind::Char(c) => {
                let escaped = escape_string(&c.to_string());
                format!("'{}'", escaped)
            }
            CIRLiteralKind::Null => "null".into(),
            CIRLiteralKind::CString(s) => {
                let escaped = escape_string(s);
                format!("\"{}\"", escaped)
            }
            CIRLiteralKind::ByteString(s) => {
                let escaped = escape_string(s);
                format!("b\"{}\"", escaped)
            }
        }
    }

    fn print_params(&mut self, params: &CIRFuncParams) -> String {
        let mut list = Vec::new();

        for param in &params.list {
            let name = param.irv_id.map(|irv_id| format!("%{}", irv_id.0));

            if let Some(name) = name {
                list.push(format!("{name}: {}", self.print_type(&param.ty)));
            } else {
                // used for function declaration
                list.push(format!("{}", self.print_type(&param.ty)));
            }
        }

        if params.is_var {
            list.push("...".into());
        }

        list.join(", ")
    }

    fn print_type(&mut self, ty: &CIRType) -> String {
        match ty {
            CIRType::Plain(plain_type) => plain_type.to_string(),
            CIRType::Const(inner) => format!("const {}", self.print_type(inner)),
            CIRType::Pointer(inner) => format!("{}*", self.print_type(inner)),
            CIRType::Struct(struct_type) => {
                let mut out = String::from("struct");

                out.push_str(" { ");

                let mut parts = Vec::new();
                for (idx, (fname, _loc)) in struct_type.fields_info.iter().enumerate() {
                    let fty = &struct_type.fields[idx];
                    parts.push(format!("{fname}: {}", self.print_type(fty)));
                }

                out.push_str(&parts.join(", "));
                out.push_str(" }");
                out
            }
            CIRType::Enum(enum_type) => {
                let mut out = String::from("enum");
                out.push_str(" { ");

                let mut parts = Vec::new();
                for variant in &enum_type.variants {
                    match variant {
                        CIREnumVariant::Unit(name) => parts.push(name.clone()),
                        CIREnumVariant::Valued(name, expr) => {
                            parts.push(format!("{name} = {}", self.print_expr(expr)));
                        }
                        CIREnumVariant::Tuple(name, types) => {
                            let elements = types.iter().map(|t| self.print_type(t)).collect::<Vec<_>>().join(", ");
                            parts.push(format!("{name}({elements})"));
                        }
                    }
                }

                out.push_str(&parts.join(", "));
                out.push_str(" }");
                out
            }
            CIRType::Union(union_type) => {
                let mut out = String::from("union");
                out.push_str(" { ");

                let mut parts = Vec::new();
                for (idx, (fname, _loc)) in union_type.fields_info.iter().enumerate() {
                    let fty = &union_type.fields[idx];
                    parts.push(format!("{fname}: {}", self.print_type(fty)));
                }

                out.push_str(&parts.join(", "));
                out.push_str(" }");
                out
            }
            CIRType::FuncType(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|p| self.print_type(p))
                    .collect::<Vec<_>>()
                    .join(", ");
                let variadic = if func.is_var { ", ..." } else { "" };
                format!("fn({}{}) {}", params, variadic, self.print_type(&func.ret_type))
            }
            CIRType::Tuple(tuple) => {
                let elements = tuple
                    .elements
                    .iter()
                    .map(|e| self.print_type(e))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("tuple({})", elements)
            }
            CIRType::Array(array) => format!("{}[{}]", self.print_type(&array.element_type), array.len),
            CIRType::Dynamic(dynamic) => {
                format!("dynamic(vtable#{})", dynamic.vtable_id.0)
            }
        }
    }
}

// Helpers.
impl<'a> CIRPrinter<'a> {
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
