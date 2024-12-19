#[macro_export]
macro_rules! compiler_error {
    ($s:expr) => {{
        panic!("(compiler) cyrus: {}", $s);
    }};
}

#[macro_export]
macro_rules! undefined_expression_error {
    () => {{
        compiler_error!("undefined expression");
    }};
}
