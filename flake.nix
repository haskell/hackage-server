{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-root.url = "github:srid/flake-root";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        # The "main" project. You can have multiple projects, but this template
        # has only one.
        packages.default = config.packages.hackage-server;
        haskellProjects.default = {
          settings = {
            hackage-server.check = false;
            heist.check = false;
          };
          packages = {
            Cabal.source = "3.10.1.0";
            Cabal-syntax.source = "3.10.1.0";
            attoparsec-aeson.source = "2.1.0.0";
            hedgehog.source = "1.3";
          };
          devShell = {
            tools = hp: {
              # Setting to null should remove this tool from defaults.
              ghcid = null;
              haskell-language-server = null;

              inherit (pkgs)
                cabal-install
                ghc

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
              ;
            };
            hlsCheck.enable = false;
          };
        };
      };
    };

  nixConfig = {
    extra-substituters = ["https://hackage-server.cachix.org/"];
    extra-trusted-public-keys = [
      "hackage-server.cachix.org-1:iw0iRh6+gsFIrxROFaAt5gKNgIHejKjIfyRdbpPYevY="
    ];
    allow-import-from-derivation = "true";
  };
}
