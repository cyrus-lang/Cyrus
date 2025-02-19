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
      rustToolchain = pkgs.rust-bin.stable.latest.default.override {
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
        nativeBuildInputs = [
          rustToolchain
          pkgs.gcc
          pkgs.libgccjit
          pkgs.binutils
          pkgs.glibc
          pkgs.gcc_multi
          pkgs.isl
          pkgs.libffi
          pkgs.libffi.dev
        ];

        meta = {
          license = pkgs.lib.licenses.mit;
          description = "Cyrus Programming Language";
          homepage = "https://github.com/cyrus-lang/Cyrus-Lang";
        };
      };

      devShells.${system}.default = pkgs.mkShell {
        name = "cyrus-dev-shell";

        buildInputs = [
          pkgs.gcc
          pkgs.libgccjit
          pkgs.binutils
          pkgs.glibc
          pkgs.gcc_multi
          pkgs.clang-tools
          pkgs.isl
          pkgs.libffi
          pkgs.libffi.dev
        ];

        env = {
          LD = "${pkgs.glibc}/lib/ld-linux-x86-64.so.2"; # Use glibc's dynamic linker
          NIX_LDFLAGS = "-L${pkgs.glibc}/lib -L${pkgs.gcc_multi}/lib";
          LD_LIBRARY_PATH = "${pkgs.glibc}/lib:${pkgs.gcc_multi}/lib:$LD_LIBRARY_PATH";
        };

        shellHook = ''
          alias cyrus="cargo run --"
        '';
      };
    };
}

