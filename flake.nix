{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-root.url = "github:srid/flake-root";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    hoogle-input.url = "github:ndmitchell/hoogle";
    hoogle-input.flake = false;
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
            warp-tls.jailbreak = true;
            tls-session-manager.jailbreak = true;
            # http-client-tls.jailbreak = true;
            # crypton-connection.jailbreak = true;
            heist.check = false;
            ap-normalize.check = false;
            extensions.jailbreak = true;
            # https://community.flake.parts/haskell-flake/dependency#nixpkgs
            # tar = { super, ... }:
            #   { custom = _: super.tar_0_6_0_0; };
            # tasty = { super, ... }:
            #   { custom = _: super.tasty_1_5; };
          };
          packages = {
            # https://community.flake.parts/haskell-flake/dependency#path
            hoogle.source = inputs.hoogle-input;
            heist.source = "1.1.1.2";
            # tls-session-manager.source = "0.0.4";
            # warp-tls.source = "3.4.3";
            # http-io-streams.source = "0.1.6.3";
            tls.source = "1.9.0";
            # tasty.source = "1.5";
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
