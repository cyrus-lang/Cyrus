#[macro_export]
macro_rules! compiler_error {
    ($s:expr) => {{
        println!("(compiler) cyrus: {}", $s);
        std::process::exit(1);
    }};
}

#[macro_export]
macro_rules! undefined_expression_error {
    () => {{
        compiler_error!("undefined expression");
    }};
}
