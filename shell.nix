let
  nixpkgs = builtins.fetchTarball {
    # master on 2022-05-14
    url = "https://github.com/NixOS/nixpkgs/archive/1d370bd07399fb52cea6badfbffbc90ac9b0f8f0.tar.gz";
    sha256 = "1ln4vwy185gwhbf4f8vanrlj4w4bhwrcsb2m8fnm99f4zqzvp7fs";
  };

  pkgs = import nixpkgs { config = { }; };

in 
with pkgs; pkgs.mkShell rec {
  buildInputs = [
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

    gd
    libpng
    libjpeg
    fontconfig
    freetype
    expat
  ];

  # LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
