{ compiler ? "ghc865" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  lib-so = import ./lib {};

  inherit (pkgs.lib.trivial) flip pipe;
  inherit (pkgs.haskell.lib) appendPatch appendConfigureFlags;
        
  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      tdlib-yigit =
        hself.callCabal2nix
          "tdlib-yigit"
          sources.tdlib-haskell-bindings
          {};
      tdlib = hself.tdlib-yigit {
        extra-libraries = [tdjson];
      };
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
      ormolu
      hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
      myHaskellPackages.tdlib-yigit
    ];


  # withHoogle = false;
  # libraryHaskellDepends = [
  # 
  # ];
  #
  # executableHaskellDepends = [
  #
  # ];

    shellHook = ''
      export LD_LIBRARY_PATH=lib
      set -e
      hpack
      set +e
    '';
};

in
{
  inherit shell;
  inherit myHaskellPackages;
  tdjson = myHaskellPackages.tdjson;
  zaku = myHaskellPackages.zaku;
}
