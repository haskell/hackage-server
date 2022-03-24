let
  nixpkgs = builtins.fetchTarball {
    # master on 2021-08-02
    url = "https://github.com/NixOS/nixpkgs/archive/70e001f35cc363eb789ea0a04eff11b86c440ba3.tar.gz";
    sha256 = "1mrhbcfa8kkx1qnax8xh41grinqiycl56wlws5vvrli8w0pzgl1r";
  };

  pkgs = import nixpkgs { config = { }; };

in
pkgs.mkShell {
  buildInputs = [
    # Haskell development
    pkgs.cabal-install
    pkgs.ghc

    # Dependencies
    pkgs.icu
    pkgs.zlib
    pkgs.brotli
  ];
}
