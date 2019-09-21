let
  compiler = "ghc865";
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          tdlib-haskell-bindings =
            haskellPackagesNew.callPackage ../tdlib-haskell-bindings/tdlib-haskell-bindings.nix {};
          gram = pkgs.haskell.packages.${compiler}.callPackage ./gram.nix { inherit tdlib-haskell-bindings; };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  pkgs.haskellPackages.gram
