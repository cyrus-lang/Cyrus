#[macro_export]
macro_rules! compiler_error {
    ($s:expr) => {{
        println!("(compiler) cyrusc: {}", $s);
        std::process::exit(1);
    }};
}

#[macro_export]
macro_rules! lexer_error {
    ($s:expr) => {{
        println!("(lexer) cyrusc: {}", $s);
        std::process::exit(1);
    }};
}


