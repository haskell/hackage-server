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
      description = ''
        Directory containing HTML templates, static files, and TUF keys.
      '';
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = "TCP port to listen on.";
    };

    ip = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
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
      "d ${cfg.stateDir}       0750 ${cfg.user} ${cfg.group} -"
      "d ${cfg.stateDir}/state 0750 ${cfg.user} ${cfg.group} -"
    ];

    systemd.services.hackage-server = {
      description = "Hackage Server";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

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
          "--ip=${cfg.ip}"
          "--port=${toString cfg.port}"
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
