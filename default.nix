{ mkDerivation, aeson, base, base64-bytestring, basic-prelude
, blaze-html, bytestring, containers, errors, haverer, hspec-wai
, hspec-wai-json, http-types, lens, MonadRandom, mtl, path-pieces
, random, servant, Spock, stdenv, stm, tasty, tasty-hspec
, tasty-quickcheck, text, transformers, unordered-containers
, vector, wai, wai-extra
}:
mkDerivation {
  pname = "hazard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base basic-prelude blaze-html bytestring containers errors
    haverer http-types lens MonadRandom mtl path-pieces random servant
    Spock stm tasty-quickcheck text transformers vector wai wai-extra
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
