{ sources ? import ./nix/sources.nix }:
let
  pkgs = import sources.nixpkgs
    { overlays = []; config = {}; };

in
pkgs.mkShell {
  buildInputs = with pkgs; [
    # Haskell development
    cabal-install
    ghc

    # Dependencies
    glibc
    icu67
    zlib
    openssl
    cryptodev
    pkg-config
    brotli
  ];
}
