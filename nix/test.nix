{ hackage-server, pkgs, ... }:

pkgs.testers.runNixOSTest {
  name = "hackage-server";

  containers.machine = { pkgs, ... }: {
    imports = [ ./nixos-module.nix ];

    services.hackage-server = {
      enable = true;
      package = hackage-server;
      datafilesDir = pkgs.runCommand "hackage-server-datafiles" {} ''
        datadir=$(dirname $(find ${hackage-server.data}/share -name templates -type d | head -1))
        ln -s "$datadir" $out
      '';
      baseUri = "http://localhost:8080";
      userContentUri = "http://localhost:8080";
      requiredBaseHostHeader = "localhost:8080";
      port = 8080;
    };

    environment.systemPackages = [ pkgs.curl ];
  };

  testScript = ''
    machine.start()
    machine.wait_for_unit("hackage-server.service")
    machine.wait_for_open_port(8080)

    # Smoke test
    machine.succeed("curl -fsS --max-time 10 http://localhost:8080/")
    machine.succeed("curl -fsS --max-time 10 http://localhost:8080/users/.json")
  '';
}
