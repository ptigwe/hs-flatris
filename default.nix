{ pkgs ? import <nixpkgs> {} }:
let
  result = pkgs.haskell.packages.ghcjs.callCabal2nix "miso" (pkgs.fetchFromGitHub {
   sha256 = "078lhm9aqfmpsk1793ijal7kkpwzlnaa7kk517dannci696k94fd";
   rev = "0cb882b48cc36d031d06696b5a1af84cdb0b5d07";
   owner = "haskell-miso";
   repo = "miso";
}) {};
in pkgs.haskell.packages.ghcjs.callPackage ./hs-flatris.nix {
  miso = result;
}
