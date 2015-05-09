{ mkDerivation, aeson, base, haverer, hspec-wai, hspec-wai-json
, http-types, scotty, stdenv, stm, tasty, tasty-hspec
, tasty-quickcheck, text, transformers
}:
mkDerivation {
  pname = "hazard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base haverer http-types scotty stm text transformers
  ];
  testDepends = [
    aeson base hspec-wai hspec-wai-json scotty stm tasty tasty-hspec
    tasty-quickcheck
  ];
  description = "An HTTP API for playing Love Letter";
  license = stdenv.lib.licenses.asl20;
}
