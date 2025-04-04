{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  shellHook = ''
    ./scripts/init-dune-locks.sh
    source ./scripts/load-dune-dev-tools.sh
  '';
}
