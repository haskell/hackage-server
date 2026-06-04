{ hackage-server, pkgs, ... }:

pkgs.testers.runNixOSTest {
  name = "hackage-server";

  containers.machine = { pkgs, ... }: {
    imports = [ ./nixos-module.nix ];

    services.hackage-server = {
      enable = true;
      package = hackage-server;
      baseUri = "http://localhost:8080";
      userContentUri = "http://localhost:8080";
      requiredBaseHostHeader = "localhost:8080";
      port = 8080;
    };

    environment.systemPackages = [ pkgs.curl ];
  };

  testScript = ''
    machine.start()
    machine.wait_for_unit("hackage-server.socket")

    # Trigger socket activation and wait for the service to come up
    machine.wait_for_open_port(8080)

    # Smoke test
    machine.succeed("curl -fsS --max-time 30 http://localhost:8080/")
    machine.succeed("curl -fsS --max-time 10 http://localhost:8080/users/.json")
  '';
}
