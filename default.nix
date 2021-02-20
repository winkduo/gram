{ compiler ? "ghc865" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  deps = {
    tdjson = pkgs.tdlib;
  };

  inherit (pkgs.lib.trivial) flip pipe;
  inherit (pkgs.haskell.lib) appendPatch appendConfigureFlags;
        
  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      ormolu = (import sources.ormolu {}).ormolu;
      tdlib-haskell-bindings =
        hself.callCabal2nix
          "tdlib-haskell-bindings"
          sources.tdlib-haskell-bindings
          deps;
      gram =
        hself.callCabal2nix
          "gram"
          (./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.gram
    ];

    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
      myHaskellPackages.ormolu
    ];

    libraryHaskellDepends = [
      myHaskellPackages.tdlib-haskell-bindings
    ];

    shellHook = ''
      set -e
      hpack
      set +e
      export PATH=$PATH:result/bin
    '';
};

in
{
  inherit shell;
  inherit myHaskellPackages;
  gram = myHaskellPackages.gram;
}
