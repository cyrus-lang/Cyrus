use std::process::Command;

fn main() {
    Command::new("gcc")
        .args(&[
            "-c",
            "-fPIC",
            "-o",
        ])
        .status()
        .unwrap();
}
