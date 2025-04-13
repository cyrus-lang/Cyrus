use std::process::Command;

fn main() {
    Command::new("gcc")
    .args(&[
        "./internals/internal_funcs.c",
        "-c",
        "-fPIC",
        "-o",
        "./internals/internals_linux.o"
    ])
    .status().unwrap();
}