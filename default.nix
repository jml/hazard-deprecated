{ mkDerivation, aeson, base, base64-bytestring, bytestring
, containers, haverer, hspec-wai, hspec-wai-json, http-types
, MonadRandom, random, Spock, stdenv, stm, tasty, tasty-hspec
, tasty-quickcheck, text, transformers, wai, wai-extra
}:
mkDerivation {
  pname = "hazard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring containers haverer http-types MonadRandom
    random Spock stm text transformers wai wai-extra
  ];
  testDepends = [
    aeson base base64-bytestring bytestring haverer hspec-wai
    hspec-wai-json http-types MonadRandom random Spock stm tasty
    tasty-hspec tasty-quickcheck text wai wai-extra
  ];
  description = "An HTTP API for playing Love Letter";
  license = stdenv.lib.licenses.asl20;
}
