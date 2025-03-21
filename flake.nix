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
        extensions = [ "rust-src" "rust-analyzer" ];
      };
    in
    {
      packages.${system}.default = pkgs.rustPlatform.buildRustPackage {
        pname = "cyrus";
        version = "latest";
        src = ./.;
        cargoLock = {
          lockFile = ./Cargo.lock;
        };
        nativeBuildInputs = with pkgs; [
          rustToolchain
          gcc
          libgccjit
          binutils
          glibc
          gcc_multi
          isl
          libffi
          libffi.dev
          llvmPackages_18.libllvm
        ];

        meta = {
          license = pkgs.lib.licenses.mit;
          description = "Cyrus Programming Language";
          homepage = "https://github.com/cyrus-lang/Cyrus-Lang";
        };
      };

      devShells.${system}.default = pkgs.mkShell {
        name = "cyrus-dev-shell";

        buildInputs = with pkgs; [
          gcc
          libgcc
          glibc
          libgccjit
          gcc_multi
          clang-tools
          libffi
          libffi.dev
          isl
          llvm_18.lib
          llvm_18.dev
          libxml2
        ];
        
        shellHook = ''
          export LIBRARY_PATH="${pkgs.glibc}/lib:${pkgs.gcc_multi}/lib:${pkgs.llvm_18.lib}/lib:${pkgs.libxml2}/lib:$LIBRARY_PATH"
          export LLVM_SYS_180_PREFIX="${pkgs.llvm_18.dev}"
          alias cyrus="cargo run --"
        '';
      };
    };
}

