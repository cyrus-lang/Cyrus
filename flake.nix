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

      rustToolchain = pkgs.rust-bin.nightly.latest.default.override {
        extensions = [ "rust-src" "rust-analyzer" "cargo" "clippy" ];
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
            rustToolchain
            gcc
            libgcc
            glibc
            glibc.static
            gcc_multi
            clang-tools
            clang
            libffi
            libffi.dev
            isl
            llvm_18.lib
            llvm_18.dev
            libxml2
          ];

          buildPhase = ''
            export LIBRARY_PATH="${pkgs.glibc.static}/lib:${pkgs.glibc}/lib:${pkgs.gcc_multi}/lib:${pkgs.llvm_18.lib}/lib:${pkgs.libxml2}/lib:$LIBRARY_PATH"
            export LLVM_SYS_180_PREFIX="${pkgs.llvm_18.dev}"
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

          nativeBuildInputs = [
            rustToolchain
            pkgs.pkgsCross.mingwW64.llvm_18.lib
            pkgs.pkgsCross.mingwW64.llvm_18.dev
            pkgs.pkgsCross.mingwW64.stdenv.cc
            pkgs.pkgsCross.mingwW64.buildPackages.binutils
            pkgs.pkgsCross.mingwW64.zlib
            pkgs.pkgsCross.mingwW64.libffi
            pkgs.pkgsCross.mingwW64.libxml2
            pkgs.pkgsCross.mingwW64.ncurses
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

      ## -----------------------------
      ## Dev shells
      ## -----------------------------
      devShells.${system} = {
        linux = pkgs.mkShell {
          name = "cyrus-dev-linux";
          buildInputs = with pkgs; [
            rustToolchain
            gcc
            libgcc
            glibc
            glibc.static
            gcc_multi
            clang-tools
            clang
            libffi
            libffi.dev
            isl
            llvm_18.lib
            llvm_18.dev
            libxml2
          ];
          shellHook = ''
            export LIBRARY_PATH="${pkgs.glibc.static}/lib:${pkgs.glibc}/lib:${pkgs.gcc_multi}/lib:${pkgs.llvm_18.lib}/lib:${pkgs.libxml2}/lib:$LIBRARY_PATH"
            export LLVM_SYS_180_PREFIX="${pkgs.llvm_18.dev}"
            alias cyrus="cargo run -j24 --"
          '';
        };

        windows = pkgs.mkShell {
          name = "cyrus-dev-windows";
          buildInputs = [
            rustToolchain
            pkgs.pkgsCross.mingwW64.stdenv.cc
            pkgs.pkgsCross.mingwW64.buildPackages.binutils
            pkgs.pkgsCross.mingwW64.buildPackages.gcc
            pkgs.pkgsCross.mingwW64.zlib
            pkgs.pkgsCross.mingwW64.libffi
            pkgs.pkgsCross.mingwW64.libxml2
            pkgs.pkgsCross.mingwW64.ncurses
          ];
          shellHook = ''
            export PATH="$PATH:/home/taha/.cargo/bin"
            export LLVM_SYS_180_PREFIX="${pkgs.llvm_18.dev}"
            alias cyrus-win="cargo build --target x86_64-pc-windows-gnu -Zbuild-std"
          '';
        };
      };
    };
}
