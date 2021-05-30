{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/e065200fc90175a8f6e50e76ef10a48786126e1c.tar.gz") {} }:
let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "b197e8764680f39416352f2ab3ed8f25e2a6b333";
      sha256 = "1b59dddrkdvh0i26any5g7lxxaxnn9af61dhxbb9bdb5n831dviw";
    }) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.psc-package
    easy-ps.purs-0_14_1
    easy-ps.spago
  ];
}
