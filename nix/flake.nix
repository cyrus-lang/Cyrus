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

      # Shared derivation (also usable without flakes via nix-build nix/default.nix).
      mkCyrus = args: pkgs.callPackage ./default.nix ({
        inherit pkgs;
        # The flake lives in nix/, the repository root is its parent.
        src = ../.;
      } // args);
    in
    {
      ## -----------------------------
      ## Packages
      ## -----------------------------
      packages.${system} = {
        default = mkCyrus { };
        linux = mkCyrus { };

        # Release-profile variant of the stage0 compiler.
        release = mkCyrus { profile = "release"; };

        # Cross-compile stage0 for Windows (requires the windows dev toolchain).
        windows = pkgs.rustPlatform.buildRustPackage {
          pname = "cyrus";
          version = "latest";
          src = ../.;
          cargoRoot = "toolchain/stage0";
          cargoLock.lockFile = ../toolchain/stage0/Cargo.lock;

          nativeBuildInputs = with pkgs; [
            rustup
            pkgsCross.mingwW64.llvm_22.lib
            pkgsCross.mingwW64.llvm_22.dev
            pkgsCross.mingwW64.stdenv.cc
            pkgsCross.mingwW64.buildPackages.binutils
            pkgsCross.mingwW64.zlib
            pkgsCross.mingwW64.libffi
            pkgsCross.mingwW64.libxml2
            pkgsCross.mingwW64.ncurses
          ];

          buildPhase = ''
            export LLVM_SYS_221_PREFIX="${pkgs.llvm_22.dev}"
            cargo build --release --target x86_64-pc-windows-gnu
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp toolchain/stage0/target/x86_64-pc-windows-gnu/release/cyrus.exe $out/bin/
          '';
        };
      };

      defaultPackage.${system} = self.packages.${system}.default;

      ## -----------------------------
      ## Dev shells
      ## -----------------------------
      devShells.${system} = {
        default = (mkCyrus { }).devShell;
        linux = (mkCyrus { }).devShell;

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
          '';
        };
      };
    };
}
