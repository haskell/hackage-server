{ config, lib, pkgs, ... }:

let
  cfg = config.services.hackage-server;
  pkg = cfg.package;
in
{
  options.services.hackage-server = {
    enable = lib.mkEnableOption "hackage-server, a Haskell package repository";

    package = lib.mkPackageOption pkgs "hackage-server" { };

    baseUri = lib.mkOption {
      type = lib.types.str;
      example = "https://hackage.example.org";
      description = "The server's public base URI.";
    };

    userContentUri = lib.mkOption {
      type = lib.types.str;
      example = "https://hackage-content.example.org";
      description = ''
        The server's public user content base URI, used for untrusted
        content to defeat XSS-style attacks.
      '';
    };

    requiredBaseHostHeader = lib.mkOption {
      type = lib.types.str;
      example = "hackage-origin.example.org";
      description = ''
        Required Host header value for incoming requests. This may be
        an internal hostname if the server is behind a reverse proxy.
      '';
    };

    stateDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/hackage-server";
      description = "Directory for the server's persistent state.";
    };

    datafilesDir = lib.mkOption {
      type = lib.types.path;
      # The Cabal data-files are installed under an ABI-specific path
      # like share/ghc-X.Y.Z/<abi-hash>/hackage-server-0.6/
      # We use a derivation to resolve the glob at build time.
      default = pkgs.runCommand "hackage-server-datafiles" {} ''
        templatesDir=$(find ${pkg.data or pkg}/share -name templates -type d | head -1)
        if [ -z "$templatesDir" ]; then
          echo "Could not find hackage-server data files in ${pkg.data or pkg}" >&2
          exit 1
        fi
        datadir=$(dirname "$templatesDir")
        ln -s "$datadir" $out
      '';
      defaultText = lib.literalMD "the data files of {option}`package`";
      description = ''
        Directory containing HTML templates, static files, and TUF keys.
        Defaults to the data-files directory shipped with the package.
        In prod, this is the `datafiles/` directory.
      '';
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = "TCP port for the listening socket.";
    };

    ip = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
      description = "IPv4 address to bind.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "hackage";
      description = "User account under which hackage-server runs.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "hackage";
      description = "Group under which hackage-server runs.";
    };
  };

  config = lib.mkIf cfg.enable {

    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
      home = cfg.stateDir;
      description = "Hackage Server service user";
    };

    users.groups.${cfg.group} = { };

    systemd.tmpfiles.rules = [
      "d ${cfg.stateDir}           0750 ${cfg.user} ${cfg.group} -"
      "d ${cfg.stateDir}/state     0750 ${cfg.user} ${cfg.group} -"
      "d ${cfg.stateDir}/state/tmp 0750 ${cfg.user} ${cfg.group} -"
    ];

    systemd.sockets.hackage-server = {
      description = "Hackage Server listening socket";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "${cfg.ip}:${toString cfg.port}";
        Accept = false;
      };
    };

    systemd.services.hackage-server = {
      description = "Hackage Server";
      requires = [ "hackage-server.socket" ];
      # No wantedBy — service is started on-demand by socket activation

      preStart = ''
        if [ ! -d "${cfg.stateDir}/state/db" ]; then
          ${lib.getExe pkg} init \
            --state-dir="${cfg.stateDir}/state" \
            --static-dir="${cfg.datafilesDir}"
        fi
      '';

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        Restart = "on-failure";
        RestartSec = 3;
        TimeoutStopSec = 120;
        LimitNOFILE = 1073741824;
        WorkingDirectory = cfg.stateDir;

        ExecStart = lib.concatStringsSep " " [
          (lib.getExe pkg)
          "run"
          "--socket-activation-only"
          "--base-uri=${cfg.baseUri}"
          "--user-content-uri=${cfg.userContentUri}"
          "--required-base-host-header=${cfg.requiredBaseHostHeader}"
          "--state-dir=${cfg.stateDir}/state"
          "--static-dir=${cfg.datafilesDir}"
          "--tmp-dir=${cfg.stateDir}/state/tmp"
        ];
      };
    };
  };
}
