{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  shellHook = ''
    export PATH="$PWD/$(dirname $(dune tools which ocamllsp)):$PATH"
    export PATH="$PWD/$(dirname $(dune tools which ocamlformat)):$PATH"
  '';
}
