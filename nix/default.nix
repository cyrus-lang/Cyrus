{
  # Package set. Defaults to <nixpkgs> so classic Nix works out of the box.
  pkgs ? import <nixpkgs> { },

  # Repository root (the parent directory of nix/).
  src ? ../.,

  # Lock file next to the stage0 cargo project.
  cargoLockFile ? ../toolchain/stage0/Cargo.lock,

  # LLVM used to link llvm-sys / inkwell (project targets LLVM 22).
  llvm ? pkgs.llvm_22,

  # Build profile for cargo.
  profile ? "release",
}:

let
  inherit (pkgs) lib;

  version =
    let
      raw = builtins.readFile (src + "/toolchain/stage0/VERSION");
    in
    lib.removeSuffix "\n" (lib.removeSuffix "\r" raw);

  commonNativeBuildInputs = with pkgs; [
    pkg-config
    clang
    llvm
    llvm.dev
    zlib
    libffi
    libxml2
    ncurses
    openssl
    rustc
    cargo
  ];

  # Environment the llvm-sys / asan / optimizer-wrapper build scripts expect.
  llvmEnv = {
    LLVM_CONFIG = "${llvm}/bin/llvm-config";
    LLVM_SYS_221_PREFIX = "${llvm.dev}";
    CC = "clang";
    CXX = "clang++";
  };

  cyrus = pkgs.rustPlatform.buildRustPackage {
    pname = "cyrus";
    inherit version src;

    # Cargo project lives under toolchain/stage0 after the repo restructure.
    cargoRoot = "toolchain/stage0";
    cargoLock.lockFile = cargoLockFile;

    nativeBuildInputs = commonNativeBuildInputs;

    buildInputs = with pkgs; [
      zlib
      libffi
      libxml2
      ncurses
      openssl
    ];

    # buildRustPackage runs these relative to the unpacked source root.
    preBuild = ''
      export LLVM_CONFIG="${llvm}/bin/llvm-config"
      export LLVM_SYS_221_PREFIX="${llvm.dev}"
      export CC=clang
      export CXX=clang++
      export CPATH="${lib.makeSearchPathOutput "dev" "include" [ pkgs.glibc ]}:$CPATH"
      cd toolchain/stage0
    '';

    buildPhase = ''
      runHook preBuild
      if [ "${profile}" = "release" ]; then
        cargo build --release --offline --frozen
      else
        cargo build --offline --frozen
      fi
      runHook postBuild
      cd ../..
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin $out/share/cyrus
      if [ "${profile}" = "release" ]; then
        cp toolchain/stage0/target/release/cyrus $out/bin/cyrus
      else
        cp toolchain/stage0/target/debug/cyrus $out/bin/cyrus
      fi
      # Ship the standard library next to the binary layout used by packaging.
      cp -r ${src}/lib/std $out/share/cyrus/stdlib
      runHook postInstall
    '';

    doCheck = false;

    meta = with lib; {
      description = "Cyrus programming language compiler";
      homepage = "https://github.com/cyrus-lang/Cyrus";
      license = licenses.mit;
      platforms = platforms.linux;
      mainProgram = "cyrus";
    };
  };

  # Development shell: toolchain + LLVM + helpful aliases.
  devShell = pkgs.mkShell {
    name = "cyrus-dev";

    packages = commonNativeBuildInputs ++ (with pkgs; [
      rustup
      clang-tools
      lld
      python3
      upx
      git
    ]);

    shellHook = ''
      export LLVM_CONFIG="${llvm}/bin/llvm-config"
      export LLVM_SYS_221_PREFIX="${llvm.dev}"
      export CC=clang
      export CXX=clang++
      export CYRUS_STDLIB_PATH="$PWD/lib/std"
      export PATH="$PWD/toolchain/stage0/target/debug:$PATH"

      alias cyrus-build='./x.py build --stage stage0'
      alias cyrus-test='./x.py test --stage all'

      echo "Cyrus dev shell (non-flake). Try: ./x.py --help"
    '';
  };
in
cyrus.overrideAttrs (old: {
  # Expose the shell for `nix-shell nix/default.nix -A devShell` style use
  # is not standard for derivations; instead provide passthru.
  passthru = (old.passthru or { }) // {
    inherit devShell;
    inherit version;
  };
})
