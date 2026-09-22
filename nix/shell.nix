# Convenience entry for `nix-shell nix/shell.nix` (non-flake).
# Delegates to default.nix's development shell.
let
  pkgs = import <nixpkgs> { };
in
(pkgs.callPackage ./default.nix { }).devShell
