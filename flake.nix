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
            # https://github.com/srid/haskell-flake/discussions/196
            # hackage-server = { super, ... }: {
            #   custom = pkg:  pkg.overrideAttrs (oa: { postCheck = ''..''; });
            # };
            # hackage-server.check = false;
            # hackage-server.cabalFlags.write-ghc-environment-files = true;
            # hackage-server.cabalFlags.write-ghc-environment = "always";

            # https://community.flake.parts/haskell-flake/dependency#nixpkgs
            tar = { super, ... }:
              { custom = _: super.tar_0_6_1_0; };
            tls = { super, ... }:
              { custom = _: super.tls_1_9_0; };
            tasty = { super, ... }:
              { custom = _: super.tasty_1_5; };
            logict.jailbreak = true;
            integer-logarithms.jailbreak = true;
            time-compat.jailbreak = true;
            indexed-traversable.jailbreak = true;
            indexed-traversable-instances.jailbreak = true;
            bitvec.jailbreak = true;
            snap.check = false;
          };
          packages = {
            # https://community.flake.parts/haskell-flake/dependency#path
            # tls.source = "1.9.0";
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
