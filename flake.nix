{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";

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
