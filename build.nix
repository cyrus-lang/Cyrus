{ lib, rustPlatform, gcc, gccjit12, fetchFromGitHub, stdenv }:

rustPlatform.buildRustPackage rec {
  pname = "cyrus-lang";
  version = "latest"; 

  src = fetchFromGitHub {
    owner = "cyrus-lang"; 
    repo = "Cyrus-Lang";  
    rev = "main";         
    sha256 = lib.fakeSha256;
  };

  nativeBuildInputs = [ gcc gccjit12 ];

  buildInputs = [ gcc ];

  meta = with stdenv.lib; {
    description = "Cyrus Lang Compiler";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
