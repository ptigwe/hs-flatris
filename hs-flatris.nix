{ mkDerivation, aeson, aeson-pretty, base, containers, miso, random
, stdenv
}:
mkDerivation {
  pname = "flatris";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base containers miso random
  ];
  description = "An implementation of Flatris using Haskell and Miso";
  license = stdenv.lib.licenses.unfree;
}
