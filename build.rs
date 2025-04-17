use std::{env, path::PathBuf, process::Command};

fn main() {
    let boehm_static_path = PathBuf::from(
        env::var("BOEHM_GC_STATIC_LIB_PATH").expect("BOEHM_GC_STATIC_LIB_PATH environment variable not set"),
    );
    println!("cargo:rustc-link-search=native={}", boehm_static_path.display());

    Command::new("gcc")
        .args(&[
            "./internals/internal_funcs.c",
            "-c",
            "-fPIC",
            "-o",
            "./internals/internal_linux.o",
        ])
        .status()
        .unwrap();
}
