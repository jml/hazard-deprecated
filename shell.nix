with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellngPackages.override {
      overrides = self: super: {
        haverer = self.callPackage ../haverer {};
        hazard = self.callPackage ./. {};
      };
    };
    pkg = modifiedHaskellPackages.callPackage
            ({ mkDerivation, aeson, base, haverer, hspec-wai, hspec-wai-json
             , http-types, scotty, stdenv, tasty, tasty-hspec
             }:
             mkDerivation {
               pname = "hazard";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [ aeson base haverer http-types scotty ];
               testDepends = [
                 aeson base hspec-wai hspec-wai-json scotty tasty tasty-hspec
               ];
               description = "An HTTP API for playing Love Letter";
               license = stdenv.lib.licenses.asl20;
             }) {};
in
  pkg.env
