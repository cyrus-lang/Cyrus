{
  description = "Cyrus language flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      overlays = [ ];
      pkgs = import nixpkgs {
        inherit system overlays;
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
          gcc
          libgcc
          gcc_multi
          clang
          clang-tools
          clangStdenv
          libffi
          libffi.dev
          isl
          llvm_18.lib
          llvm_18.dev
          libxml2
          flex
          bison
        ];

        buildPhase = ''
          export LIBRARY_PATH="${pkgs.gcc_multi}/lib:${pkgs.llvm_18.lib}/lib:${pkgs.libxml2}/lib:${pkgs.flex}/lib:${pkgs.bison}/lib:$LIBRARY_PATH"
          export LLVM_SYS_180_PREFIX="${pkgs.llvm_18.dev}"
          cargo build --release
        '';

        installPhase = ''
          mkdir -p $out/bin
          cp target/release/cyrus $out/bin/
        '';

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
          gcc_multi
          clang-tools
          clang
          libffi
          libffi.dev
          isl
          llvm_18.lib
          llvm_18.dev
          libxml2
          flex
          bison
        ];

        shellHook = ''
          export LIBRARY_PATH="${pkgs.gcc_multi}/lib:${pkgs.llvm_18.lib}/lib:${pkgs.libxml2}/lib:${pkgs.flex}/lib:${pkgs.bison}/lib:$LIBRARY_PATH"
          export LLVM_SYS_180_PREFIX="${pkgs.llvm_18.dev}"
        '';
      };
    };
}
