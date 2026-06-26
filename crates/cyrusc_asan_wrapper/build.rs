use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=CXX");
    println!("cargo:rerun-if-env-changed=LLVM_CONFIG");
    println!("cargo:rerun-if-env-changed=GLIBC_INCLUDE_PATH");

    let is_windows = cfg!(target_os = "windows");
    let llvm_config = env::var("LLVM_CONFIG").unwrap_or_else(|_| "llvm-config".to_string());

    let llvm_config_ok = if is_windows {
        false
    } else {
        match Command::new(&llvm_config).arg("--cxxflags").output() {
            Ok(output) => output.status.success(),
            Err(_) => false,
        }
    };

    if !llvm_config_ok {
        println!("cargo:warning=llvm-config not found or target is Windows. Skipping C++ compilation of ASan wrapper and using stub.");
        let out_dir = env::var("OUT_DIR").unwrap();
        let dummy_path = Path::new(&out_dir).join("asan_dummy.cpp");
        let dummy_content = "extern \"C\" {\n    typedef struct {\n        bool address_sanitize;\n        bool thread_sanitize;\n        bool mem_sanitize;\n        bool hwaddress_sanitize;\n        bool recover;\n        bool asan_use_after_scope;\n        bool asan_use_after_return;\n    } SanitizerOptions;\n    void run_sanitizer_passes(void* module_ref, void* tm_ref, int opt_level, SanitizerOptions opts) {}\n}";
        fs::write(&dummy_path, dummy_content).unwrap();
        cc::Build::new()
            .cpp(true)
            .file(dummy_path)
            .warnings(false)
            .compile("cyrusc_asan_wrapper");
        return;
    }

    let cpp_file = "src/asan_wrapper.cpp";
    println!("cargo:rerun-if-changed={}", cpp_file);

    let cxx = env::var("CXX").unwrap_or_else(|_| "clang++".to_string());

    let cxxflags_output = Command::new(&llvm_config)
        .arg("--cxxflags")
        .output()
        .expect("Failed to run llvm-config --cxxflags");
    let cxxflags = String::from_utf8(cxxflags_output.stdout).expect("llvm-config --cxxflags returned invalid UTF-8.");

    let ldflags_output = Command::new(&llvm_config)
        .args(["--ldflags", "--system-libs", "--libs", "core", "passes"])
        .output()
        .expect("Failed to run llvm-config for ldflags");

    let ldflags = String::from_utf8(ldflags_output.stdout).unwrap();

    for token in ldflags.split_whitespace() {
        if token.starts_with("-L") {
            println!("cargo:rustc-link-search=native={}", &token[2..]);
        } else if token.starts_with("-l") {
            println!("cargo:rustc-link-lib={}", &token[2..]);
        } else {
            println!("cargo:rustc-link-arg={}", token);
        }
    }

    let mut build = cc::Build::new();
    build
        .cpp(true)
        .compiler(cxx)
        .file(cpp_file)
        .warnings(false)
        .flag_if_supported("-std=c++17")
        .flag_if_supported("-fPIC")
        .flag_if_supported("-O2")
        .flags(cxxflags.split_whitespace());

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
