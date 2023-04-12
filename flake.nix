{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";

    linear-generics = {
      url = "github:linear-generics/linear-generics";
      flake = false;
    };

    stylish-haskell = {
      url = "github:haskell/stylish-haskell";
      flake = false;
    };

    nothunks = {
      url = "github:input-output-hk/nothunks";
      flake = false;
    };

    hls-hlint-plugin = {
      url = "github:haskell/haskell-language-server?dir=plugins/hls-hlint-plugin";
      flake = false;
    };
    hls-floskell-plugin = {
      url = "github:haskell/haskell-language-server?dir=plugins/hls-floskell-plugin";
      flake = false;
    };
    hls-ormolu-plugin = {
      url = "github:haskell/haskell-language-server?dir=plugins/hls-ormolu-plugin";
      flake = false;
    };
    hls-graph = {
      url = "github:haskell/haskell-language-server?dir=hls-graph";
      flake = false;
    };
    nixpkgs-140774-workaround.url = "github:srid/nixpkgs-140774-workaround";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        # The "main" project. You can have multiple projects, but this template
        # has only one.
        haskellProjects.main = {
          # basePackages = pkgs.haskell.packages.ghc927;
          # basePackages = pkgs.haskell.packages.ghc944;
          imports = [
            inputs.nixpkgs-140774-workaround.haskellFlakeProjectModules.default
          ];
          packages.hackage-server.root = ./.;  # Auto-discovered by haskell-flake
          overrides = self: super: {
            Cabal = super.Cabal_3_10_1_0;
            Cabal-syntax = super.Cabal-syntax_3_10_1_0;
            doctest-parallel = super.doctest-parallel_0_3_0_1;

            ghc-lib-parser = super.ghc-lib-parser_9_4_4_20221225;
            ghc-lib-parser-ex = super.ghc-lib-parser-ex_9_4_0_0;
            text = super.text_2_0_2;
            parsec = self.callHackage "parsec" "3.1.16.1" {};

            chell = pkgs.haskell.lib.doJailbreak (self.callHackage "chell" "0.5.0.1" {});
            ghc-boot-th = self.callHackage "ghc-boot-th" "9.2.1" {};
            hedgehog = self.callHackage "hedgehog" "1.2" {};
            tasty-hedgehog = self.callHackage "tasty-hedgehog" "1.4.0.0" {};
            optparse-applicative = pkgs.haskell.lib.doJailbreak (super.optparse-applicative_0_15_1_0);
            haddock-library = pkgs.haskell.lib.doJailbreak (self.callHackage "haddock-library" "1.11.0" {});

            th-abstraction = self.callHackage "th-abstraction" "0.4.5.0" {};
            stylish-haskell = super.callCabal2nix "stylish-haskell" inputs.stylish-haskell {};

            nothunks = super.callCabal2nix "nothunks" inputs.nothunks {};

            # requirements of HLS
            # TODO: fix HLS https://github.com/haskell/haskell-language-server/issues/3518
            ormolu = self.callHackage "ormolu" "0.5.3.0" {};
            fourmolu = pkgs.haskell.lib.dontCheck (self.callHackage "fourmolu" "0.10.1.0" {});
            # hls-floskell-plugin = pkgs.haskell.lib.dontCheck (self.callHackage "hls-floskell-plugin" "1.0.2.0" {});
            # hls-floskell-plugin = self.callCabal2nix "hls-floskell-plugin" inputs.hls-floskell-plugin {};
            # hls-graph = self.callCabal2nix "hls-graph" inputs.hls-graph {};
            # hls-graph = self.callHackage "hls-graph" "1.9.0.0" {};
            # hls-hlint-plugin = self.callCabal2nix "hls-hlint-plugin" inputs.hls-hlint-plugin {};
            # hls-hlint-plugin = self.callHackage "hls-hlint-plugin" "1.1.2.0" {};
            # hls-ormolu-plugin = self.callCabal2nix "hls-ormolu-plugin" inputs.hls-ormolu-plugin {};
            # hls-ormolu-plugin = self.callHackage "hls-ormolu-plugin" "1.0.2.2" {};
            # hls-ormolu-plugin = self.callHackage "hls-ormolu-plugin" "1.0.3.0" {};
            # hls-plugin-api = self.callHackage "hls-plugin-api" "1.6.0.0" {};
            # hls-test-utils = self.callHackage "hls-test-utils" "1.5.0.0" {};

            hlint = self.callHackage "hlint" "3.5" {};

            ghcide = pkgs.haskell.lib.dontCheck (self.callHackage "ghcide" "1.9.0.0" {});
          };
          devShell = {
            hlsCheck.enable = false;
          };
        };

        packages.default = pkgs.haskell.lib.dontCheck (self'.packages.main-hackage-server);

        # TODO: fix HLS https://github.com/haskell/haskell-language-server/issues/3518
        # Default shell.
        # devShells.default =
        #   config.mission-control.installToDevShell self'.devShells.main;
        devShells.default = pkgs.mkShell {
          buildInputs =
            with pkgs;
            [ cabal-install
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

            ];
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
