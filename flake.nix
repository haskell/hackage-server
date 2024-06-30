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
            hackage-mirror nix-default-servers.cfg
          '';
        };
        packages.default = config.packages.hackage-server;
        haskellProjects.default = {
          # basePackages = pkgs.haskell.packages.ghc98;
          settings = {
            hackage-server.check = true;
            # https://community.flake.parts/haskell-flake/dependency#nixpkgs
            tar = { super, ... }:
              { custom = _: super.tar_0_6_3_0; };
            # tls = { super, ... }:
            #   { custom = _: super.tls_2_0_2; };
            # tls-session-manager = { super, ... }:
            #   { custom = _: super.tls-session-manager_0_0_5; };
            tasty = { super, ... }:
              { custom = _: super.tasty_1_5_1; };
            logict.jailbreak = true;
            integer-logarithms.jailbreak = true;
            time-compat.jailbreak = true;
            indexed-traversable-instances.jailbreak = true;
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
