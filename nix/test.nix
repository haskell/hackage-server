{ hackage-server, pkgs, ... }:

pkgs.testers.runNixOSTest {
  name = "hackage-server";

  nodes.machine = { pkgs, ... }: {
    imports = [ ./nixos-module.nix ];

    services.hackage-server = {
      enable = true;
      package = hackage-server;
      baseUri = "http://localhost:8080";
      userContentUri = "http://localhost:8080";
      requiredBaseHostHeader = "localhost:8080";
      port = 8080;
      ip = "127.0.0.1";
    };

    environment.systemPackages = [ pkgs.curl ];
  };

  testScript = ''
    machine.start()
    machine.wait_for_unit("hackage-server.socket")

    # Socket activation should defer starting the service until the first request.
    machine.succeed("! systemctl is-active --quiet hackage-server.service")
    machine.succeed("ss -ltn | grep -q '127.0.0.1:8080'")

    # Trigger socket activation and wait for the service to come up
    machine.wait_for_open_port(8080)
    machine.wait_for_unit("hackage-server.service")
    machine.succeed("test \"$(systemctl show -p NRestarts --value hackage-server.service)\" = 0")

    # Smoke test
    machine.succeed("curl -fsS --max-time 30 http://localhost:8080/")
    machine.succeed("curl -fsS --max-time 10 http://localhost:8080/users/.json")

    # Exercise concurrent requests through the activated listener.
    machine.succeed("for i in $(seq 1 10); do curl -fsS --max-time 10 http://localhost:8080/ >/dev/null & done; wait")

    # Restarting the service must leave the socket unit available.
    machine.succeed("systemctl restart hackage-server.service")
    machine.wait_for_unit("hackage-server.service")
    machine.succeed("curl -fsS --max-time 30 http://localhost:8080/")
  '';
}
