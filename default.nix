{ pkgs ? import <nixpkgs> {} }:
let
  result = pkgs.haskell.packages.ghcjs.callCabal2nix "miso" (pkgs.fetchFromGitHub {
   sha256 = "0ylsml2v5fq7z51cvq88yq1h3f4px87n3jfgil8d6xjn11r21sfr";
   rev = "c4e22369551ad6fc6dc567b984cb0c5f68e469a3";
   owner = "haskell-miso";
   repo = "miso";
}) {};
in pkgs.haskell.packages.ghcjs.callPackage ./hs-flatris.nix {
  miso = result;
}
