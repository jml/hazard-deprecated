with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellngPackages.override {
      overrides = self: super: {
        haverer = self.callPackage ../haverer {};
        hazard = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.hazard.env
