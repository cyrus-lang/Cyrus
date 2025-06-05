#[cfg(test)]
mod tests {
    use crate::{CodeGenLLVM, build::OutputKind, opts::Options};
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
            Options::default(),
            true,
            OutputKind::None,
        );
    }
}
