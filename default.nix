{ mkDerivation, aeson, base, base64-bytestring, basic-prelude
, blaze-html, blaze-markup, bytestring, containers, either, errors
, haverer, hspec-wai, hspec-wai-json, http-types, lens, MonadRandom
, mtl, path-pieces, random, servant, servant-blaze, servant-server
, Spock, stdenv, stm, tasty, tasty-hspec, tasty-quickcheck, text
, transformers, unordered-containers, vector, wai, wai-extra, warp
}:
mkDerivation {
  pname = "hazard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base basic-prelude blaze-html blaze-markup bytestring
    containers either errors haverer http-types lens MonadRandom mtl
    path-pieces random servant servant-blaze servant-server Spock stm
    tasty-quickcheck text transformers vector wai wai-extra warp
  ];
  testDepends = [
    aeson base base64-bytestring basic-prelude bytestring errors
    haverer hspec-wai hspec-wai-json http-types MonadRandom random
    Spock stm tasty tasty-hspec tasty-quickcheck text
    unordered-containers wai wai-extra
  ];
  description = "An HTTP API for playing a lovely card game";
  license = stdenv.lib.licenses.asl20;
}
