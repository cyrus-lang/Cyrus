/* 
 * Copyright (c) 2026 The Cyrus Language
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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
        .args(["--ldflags", "--system-libs", "--libs", "core", "passes"])
        .output()
        .expect("Failed to run llvm-config for ldflags");

    let ldflags = String::from_utf8(ldflags.stdout).unwrap();

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
