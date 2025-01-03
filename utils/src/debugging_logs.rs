#[macro_export]
macro_rules! debugging_log {
    ($s:expr) => {{
        #[cfg(debug_assertions)]
        println!("cyrusdbglog: {}", $s);
    }};
}
