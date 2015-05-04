with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, base, hspec-wai, hspec-wai-json, http-types
             , scotty, stdenv, tasty, tasty-hspec
             }:
             mkDerivation {
               pname = "hazard";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [ aeson base http-types scotty ];
               testDepends = [
                 aeson base hspec-wai hspec-wai-json scotty tasty tasty-hspec
               ];
               description = "An HTTP API for playing Love Letter";
               license = stdenv.lib.licenses.asl20;
             }) {};
in
  pkg.env
