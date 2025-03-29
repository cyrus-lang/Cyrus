#[cfg(test)]
mod tests {
    use crate::{opts::Options, CodeGenLLVM};
    use inkwell::context::Context;

    #[test]
    fn test_new_codegen_llvm() {
        let context = Context::create();
        let _ = CodeGenLLVM::new(
            &context,
            String::from("./main.cyr"),
            String::from("main.cyr"),
            ast::ast::ProgramTree {
                body: vec![],
                span: ast::token::Span::default(),
            },
            Options::default()
        );
    }
}
