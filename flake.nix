{
  description = "Cyrus language flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, rust-overlay }:
    let
      system = "x86_64-linux";
      overlays = [ (import rust-overlay) ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };
    in
    {
      ## -----------------------------
      ## Packages
      ## -----------------------------
      packages.${system} = {
        linux = pkgs.rustPlatform.buildRustPackage {
          pname = "cyrus";
          version = "latest";
          src = ./.;
          cargoLock = { lockFile = ./Cargo.lock; };

          nativeBuildInputs = with pkgs; [
            rustup
            gcc
            libgcc
            gcc_multi
            clang-tools
            clang
            libffi
            libffi.dev
            isl
            llvm_18.lib
            llvm_18.dev
            libxml2
            ncurses
            zlib
            pkg-config
            stdenv.cc
            upx
          ];

          buildPhase = ''
            export CXX=clang++
            export CC=clang

            export GLIBC_INCLUDE_PATH="${pkgs.glibc.dev}/include:${pkgs.stdenv.cc.cc}/include"
            export CPATH="${pkgs.glibc.dev}/include:${pkgs.stdenv.cc.cc}/include/c++/${pkgs.stdenv.cc.cc.version}:${pkgs.stdenv.cc.cc}/include/c++/${pkgs.stdenv.cc.cc.version}/x86_64-unknown-linux-gnu" 

            export LIBRARY_PATH="${pkgs.glibc.static}/lib:${pkgs.glibc}/lib:${pkgs.stdenv.cc.cc.lib}/lib:${pkgs.llvm_18.lib}/lib:${pkgs.libxml2}/lib:$LIBRARY_PATH"
            export LLVM_SYS_180_PREFIX="${pkgs.llvm_18.dev}"

            export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig:${pkgs.ncurses.dev}/lib/pkgconfig:${pkgs.libxml2.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
            
            cargo build --release
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp target/release/cyrus $out/bin/
          '';
        };

        windows = pkgs.rustPlatform.buildRustPackage {
          pname = "cyrus";
          version = "latest";
          src = ./.;
          cargoLock = { lockFile = ./Cargo.lock; };

          llvmPackages_18.override = {
            enableSharedLibraries = false;
          };

          nativeBuildInputs = with pkgs; [
            rustup
            pkgsCross.mingwW64.llvm_18.lib
            pkgsCross.mingwW64.llvm_18.dev
            pkgsCross.mingwW64.stdenv.cc
            pkgsCross.mingwW64.buildPackages.binutils
            pkgsCross.mingwW64.zlib
            pkgsCross.mingwW64.libffi
            pkgsCross.mingwW64.libxml2
            pkgsCross.mingwW64.ncurses
          ];

          buildPhase = ''
            export LLVM_SYS_180_PREFIX="${pkgs.llvm_18.dev}"
            cargo build --release --target x86_64-pc-windows-gnu -Zbuild-std
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp target/x86_64-pc-windows-gnu/release/cyrus.exe $out/bin/
          '';
        };
      };

      defaultDevShell.${system} = self.devShells.${system}.linux;

      ## -----------------------------
      ## Dev shells
      ## -----------------------------
      devShells.${system} = {
        linux = pkgs.mkShell {
          name = "cyrus-dev-linux";

          buildInputs = with pkgs; [
            rustup
            gcc
            libgcc
            gcc_multi
            clang-tools
            clang
            libffi
            libffi.dev
            isl
            llvm_18.lib
            llvm_18.dev
            lldb_18

            libxml2
            libxml2.dev

            zlib
            zlib.dev
            ncurses
            ncurses.dev

            pkg-config
            stdenv.cc

            upx
          ];
          shellHook = ''
            export CXX=clang++
            export CC=clang

            export CPATH="${pkgs.stdenv.cc.cc}/include/c++/${pkgs.stdenv.cc.cc.version}:${pkgs.stdenv.cc.cc}/include/c++/${pkgs.stdenv.cc.cc.version}/x86_64-unknown-linux-gnu" 
            export GLIBC_INCLUDE_PATH="${pkgs.stdenv.cc.cc}/include"

            export LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib}/lib:${pkgs.llvm_18.lib}/lib:${pkgs.libxml2}/lib:$LIBRARY_PATH"
            export LLVM_SYS_180_PREFIX="${pkgs.llvm_18.dev}"

            alias cyrus="cargo run -j24 --"
          '';
        };

        windows = pkgs.mkShell {
          name = "cyrus-dev-windows";
          buildInputs = with pkgs; [
            rustup
            zig
            cargo-zigbuild
            pkgsCross.mingwW64.stdenv.cc
            pkgsCross.mingwW64.buildPackages.binutils
            pkgsCross.mingwW64.buildPackages.gcc
            pkgsCross.mingwW64.zlib
            pkgsCross.mingwW64.libffi
            pkgsCross.mingwW64.libxml2
            pkgsCross.mingwW64.ncurses
            lldb_18
          ];
          shellHook = ''    
            rustup install nightly 2>/dev/null || true
            rustup default nightly

            export CC_x86_64_pc_windows_gnu=x86_64-w64-mingw32-gcc
            export CXX_x86_64_pc_windows_gnu=x86_64-w64-mingw32-g++

            rustup target add x86_64-pc-windows-gnu --toolchain nightly 2>/dev/null || true

            export LIBRARY_PATH="${pkgs.pkgsCross.mingwW64.libffi}/lib:${pkgs.pkgsCross.mingwW64.zlib}/lib:${pkgs.pkgsCross.mingwW64.libxml2}/lib:${pkgs.pkgsCross.mingwW64.ncurses}/lib:$LIBRARY_PATH"
            export C_INCLUDE_PATH="${pkgs.pkgsCross.mingwW64.libffi}/include:${pkgs.pkgsCross.mingwW64.zlib}/include:${pkgs.pkgsCross.mingwW64.libxml2}/include:${pkgs.pkgsCross.mingwW64.ncurses}/include:$C_INCLUDE_PATH"
    
            alias build-cyrus-win="RUSTFLAGS="-C link-args=" cargo zigbuild --target x86_64-pc-windows-gnu -j24 --release"
          '';
        };
      };
    };
}
