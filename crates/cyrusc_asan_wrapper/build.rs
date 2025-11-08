use std::env;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let cpp_file = "src/asan_wrapper.cpp";
    println!("cargo:rerun-if-changed={}", cpp_file);

    let cxx = env::var("CXX").unwrap_or_else(|_| "clang++".to_string());

    let llvm_config = env::var("LLVM_CONFIG").unwrap_or_else(|_| "llvm-config".to_string());

    let cxxflags = Command::new(&llvm_config)
        .arg("--cxxflags")
        .output()
        .expect("Failed to run llvm-config --cxxflags");
    let cxxflags = String::from_utf8(cxxflags.stdout).expect("llvm-config --cxxflags returned invalid UTF-8.");

    let ldflags = Command::new(&llvm_config)
        .args(&["--ldflags", "--system-libs", "--libs", "core", "passes"])
        .output()
        .expect("Failed to run llvm-config --ldflags --system-libs --libs core passes.");
    let ldflags = String::from_utf8(ldflags.stdout).expect("llvm-config --ldflags returned invalid UTF-8.");

    let mut build = cc::Build::new();
    build
        .cpp(true)
        .compiler(cxx)
        .file(cpp_file)
        .warnings(true)
        .flag_if_supported("-std=c++17")
        .flag_if_supported("-fPIC")
        .flags(cxxflags.split_whitespace())
        .flags(ldflags.split_whitespace());

    if let Ok(glibc_include) = env::var("GLIBC_INCLUDE_PATH") {
        build.include(glibc_include);
    }

    build.compile("cyrusc_asan_wrapper");
}
