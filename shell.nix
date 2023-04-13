{ pkgs ? import <nixpkgs> {} }:
let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "0c10ff170461aed0c336f5c21ed0f430c2c3574b";
      sha256 = "LLqaLPJNiap2U8I77K5XVPGJA/Be30Z8lyGOyYXmBlc=";
    }) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.psc-package
    easy-ps.purs-0_15_7
    easy-ps.spago
    pkgs.esbuild
  ];
}
