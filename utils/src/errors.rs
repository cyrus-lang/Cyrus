#[macro_export]
macro_rules! compiler_error {
    ($s:expr) => {{
        println!("(compiler) cyrus: {}", $s);
        std::process::exit(1);
    }};
}

#[macro_export]
macro_rules! lexer_error {
    ($s:expr) => {{
        println!("(lexer) cyrus: {}", $s);
        std::process::exit(1);
    }};
}


