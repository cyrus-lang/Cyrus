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
