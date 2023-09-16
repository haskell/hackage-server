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
            fourmolu.check = false;
            threads.check = false;
            hls-cabal-plugin.check = false;
            hls-fourmolu-plugin.check = false;
            hw-prim.jailbreak = true;
            hw-hspec-hedgehog.jailbreak = true;
            hw-fingertree.jailbreak = true;
          };
          packages = {
            Cabal.source = "3.10.1.0";
            Cabal-syntax.source = "3.10.1.0";
            attoparsec-aeson.source = "2.1.0.0";
            hedgehog.source = "1.4";
            ormolu.source = "0.7.2.0";
            fourmolu.source = "0.13.1.0";
            tasty-hedgehog.source = "1.4.0.2";
            ghc-lib-parser.source = "9.6.2.20230523";
            ghc-lib-parser-ex.source = "9.6.0.2";
            hlint.source = "3.6.1";
            stylish-haskell.source = "0.14.5.0";
          };
          devShell = {
            tools = hp: {
              inherit (pkgs)
                cabal-install
                ghc

                # https://github.com/haskell/hackage-server/pull/1219#issuecomment-1597140858
                # glibc
                icu67
                zlib
                openssl
                # cryptodev
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
