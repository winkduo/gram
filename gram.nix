{ mkDerivation, aeson, async, base, containers, curl, directory
, fused-effects, fused-effects-lens, http-client, http-client-tls
, http-types, lens, load-env, mtl, optparse-applicative
, privileged-concurrency, retry, servant-client-core, singletons
, stdenv, tdlib-haskell-bindings, text, time, typed-duration, unix
, vector-sized
}:
mkDerivation {
  pname = "gram";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base containers curl directory fused-effects
    fused-effects-lens http-client http-client-tls http-types lens
    load-env mtl optparse-applicative privileged-concurrency retry
    servant-client-core singletons tdlib-haskell-bindings text time
    typed-duration unix vector-sized
  ];
  license = stdenv.lib.licenses.bsd3;
}
