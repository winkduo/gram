{
  network.description = "Web server";
  gram = import ../default.nix;
  gramserver = { config, pkgs, ... }: {
    networking.hostName = "gram";
    networking.firewall.allowedTCPPorts = [ 22 80 ];
    environment.systemPackages = [ gram ];
    systemd.services.gram = {
      description = "gram Webserver";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = "${gram}/bin/gram";
        EnvironmentFile = "${builtins.path { name = "dotenv"; path = ../.env; }}";
      };
    };
  };
}
