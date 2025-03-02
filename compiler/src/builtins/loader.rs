use ast::{
    ast::{FunctionParam, FunctionParams, Identifier, VisType},
    token::{Location, Span, TokenKind},
};

use super::funcs::builtin_func__sizeof;
use crate::{Compiler, funcs::FuncMetadata};

pub fn builtins_loader(mut compiler: Compiler) -> Compiler {
    let mut func_table = compiler.func_table.borrow_mut().clone();

    if func_table.keys().find(|&key| key.clone() == String::from("sizeof")).is_none() {
        dbg!("no exist");
    }
    func_table.insert(
        String::from("sizeof"),
        FuncMetadata {
            func_type: VisType::Internal,
            ptr: builtin_func__sizeof(compiler.context),
            return_type: TokenKind::SizeT,
            params: FunctionParams {
                list: vec![FunctionParam {
                    identifier: Identifier {
                        name: String::from("rvalue"),
                        span: Span::default(),
                        loc: Location::default(),
                    },
                    ty: Some(TokenKind::Dereference(Box::new(TokenKind::Void))),
                    default_value: None,
                    span: Span::default(),
                    loc: Location::default(),
                }],
                is_variadic: false,
            },
            imported_from: None,
        },
    );

    compiler.func_table = func_table.into();
    return compiler;
}
