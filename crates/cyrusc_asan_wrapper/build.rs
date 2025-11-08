use std::env;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=CXX");
    println!("cargo:rerun-if-env-changed=LLVM_CONFIG");
    println!("cargo:rerun-if-env-changed=GLIBC_INCLUDE_PATH");

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
        .flag_if_supported("-O0")
        .flags(cxxflags.split_whitespace())
        .flags(ldflags.split_whitespace());

    if let Ok(cpath) = std::env::var("CPATH") {
        for path in cpath.split(':') {
            if !path.is_empty() {
                build.include(path);
            }
        }
    }

    if let Ok(glibc_include) = env::var("GLIBC_INCLUDE_PATH") {
        build.include(&glibc_include);
    }

    build.compile("cyrusc_asan_wrapper");
}
