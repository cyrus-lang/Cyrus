#[macro_export]
macro_rules! compiler_error {
    ($s:expr) => {{
        println!("(compiler) cyrus: {}", $s);
        std::process::exit(1);
    }};
}
