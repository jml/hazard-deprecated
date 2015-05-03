with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, scotty, stdenv, tasty }:
             mkDerivation {
               pname = "hazard";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [ base scotty ];
               testDepends = [ base tasty ];
               description = "An HTTP API for playing Love Letter";
               license = stdenv.lib.licenses.asl20;
             }) {};
in
  pkg.env
