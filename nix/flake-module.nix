{ withSystem, ... }:

{
  flake.nixosModules.default = { config, lib, pkgs, ... }:
    let
      pkg = withSystem pkgs.stdenv.hostPlatform.system ({ config, ... }: config.packages.hackage-server);
    in {
    imports = [ ./nixos-module.nix ];
    services.hackage-server.package = lib.mkDefault pkg;
    services.hackage-server.datafilesDir = lib.mkDefault (
      # The Cabal data-files are installed under an ABI-specific path
      # like share/ghc-X.Y.Z/<abi-hash>/hackage-server-0.6/
      # We use a derivation to resolve the glob at build time.
      pkgs.runCommand "hackage-server-datafiles" {} ''
        datadir=$(dirname $(find ${pkg.data}/share -name templates -type d | head -1))
        if [ -z "$datadir" ]; then
          echo "Could not find hackage-server data files in ${pkg.data}" >&2
          exit 1
        fi
        ln -s "$datadir" $out
      ''
    );
  };

  perSystem = { system, lib, config, pkgs, ... }:
  {
    checks = lib.optionalAttrs pkgs.stdenv.isLinux {
      nixos-test = import ./test.nix {
        hackage-server = config.packages.hackage-server;
        inherit pkgs;
      };
    };

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
          --base-uri=http://127.0.0.1:8080 \
          --required-base-host-header=localhost:8080 \
          --user-content-uri=http://127.0.0.1:8080
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
      # Only include files relevant to the Haskell build so that
      # changes to nix/, flake.nix, etc. don't trigger a rebuild.
      projectRoot = lib.fileset.toSource {
        root = ../.;
        fileset = let
          haskell = f: lib.hasSuffix ".hs" f.name;
        in lib.fileset.unions [
          ../cabal.project
          ../hackage-server.cabal
          ../LICENSE
          (lib.fileset.fileFilter haskell ../src)
          (lib.fileset.fileFilter haskell ../exes)
          (lib.fileset.fileFilter haskell ../benchmarks)
          ../tests # includes .hs, golden files, test tarballs, etc.
          ../datafiles
          ../libstemmer_c
          ../src/Distribution/Server/Util/NLP/LICENSE
        ];
      };
      settings = {
        hackage-server.check = false;

        Cabal-syntax = { super, ... }:
          { custom = _: super.Cabal-syntax_3_16_1_0; };
        Cabal = { super, ... }:
          { custom = _: super.Cabal_3_16_1_0; };

        sandwich.check = false;

        threads.check = false;

        unicode-data.check = false;
      };
      packages = {
        # https://community.flake.parts/haskell-flake/dependency#path
        # tls.source = "1.9.0";
        tar.source = "0.7.0.0";
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
}
