{ pkgs ? import <nixpkgs> {} }:
let
  result = import /home/dmj/miso {};
in pkgs.haskell.packages.ghcjs.callPackage ./hs-flatris.nix {
  miso = result.miso-ghcjs;
}

