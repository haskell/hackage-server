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
        apps.default.program = pkgs.writeShellApplication {
          name = "run-hackage-server";
          runtimeInputs = [ config.packages.default ];
          text = ''
            if [ ! -d "state" ]; then
              hackage-server init --static-dir=datafiles --state-dir=state
            else
              echo "'state' state-dir already exists"
            fi
            hackage-server run \
              --static-dir=datafiles \
              --state-dir=state \
              --base-uri=http://127.0.0.1:8080
          '';
        };
        apps.mirror-hackage-server.program = pkgs.writeShellApplication {
          name = "mirror-hackage-server";
          runtimeInputs = [ config.packages.default ];
          text = ''
            echo 'Copying packages from real Hackage Server into local Hackage Server.'
            echo 'This assumes the local Hackage Server uses default credentials;'
            echo 'otherwise, override in nix-default-servers.cfg'
            hackage-mirror nix-default-servers.cfg "$@"
          '';
        };
        packages.default = config.packages.hackage-server;
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc912;
          settings = {
            hackage-server.check = false;

            cabal-add.jailbreak = true;
            cabal-install-parsers.jailbreak = true;
            # https://community.flake.parts/haskell-flake/dependency#nixpkgs
            Cabal-syntax = { super, ... }:
              { custom = _: super.Cabal-syntax_3_14_2_0; };
            Cabal = { super, ... }:
              { custom = _: super.Cabal_3_14_2_0; };
            fourmolu.check = false;
            doctest.check = false;
            network.check = false;
            system-filepath.check = false;
            hls-plugin-api.jailbreak = true;
            ghcide.jailbreak = true;
            # stylish-haskell.jailbreak = true;
            haskell-language-server.jailbreak = true;

            Diff = { super, ... }:
              { custom = _: super.Diff_1_0_2; };

            threads.check = false;
            unicode-data.check = false;
            tree-diff.check = false;

            ormolu = { super, ... }:
              { custom = _: super.ormolu_0_8_0_0;
                check = false;
              };
            extensions = { super, ... }:
              { custom = _: super.extensions_0_1_0_3;
                jailbreak = true;
              };

            brick = { super, ... }:
              { custom = _: super.brick_2_9; };

            hlint = { super, ... }:
              { custom = _: super.hlint_3_10; };
            ghc-lib-parser-ex = { super, ... }:
              { custom = _: super.ghc-lib-parser-ex_9_12_0_0; };
            ghc-lib-parser = { super, ... }:
              {
                custom = _: super.ghc-lib-parser_9_12_2_20250421;
                # custom = _: super.ghc-lib-parser_9_12_2_20250320;
              };
          };
          packages = {
            # https://community.flake.parts/haskell-flake/dependency#path
            # tls.source = "1.9.0";
            fourmolu.source = "0.18.0.0";
            stylish-haskell.source = "0.15.1.0";
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
