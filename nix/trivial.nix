rec {
  network.description = "Web server";
  gramserver = { config, pkgs, ... }:
  let gram = import ../default.nix; in
  {
    networking.hostName = "gram";
    networking.firewall.allowedTCPPorts = [ 22 80 ];
    environment.systemPackages = [ gram ];
    systemd.services.gram = {
      description = "gram Webserver";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = ''
          ${gram}/bin/gram \
            --allowed_chat_ids "160758532" \
            --test_chat_id 804952120 \
            --message_spam_window 60 \
            --spam_check_interval 10 \
            --spam_max_messages 10
        '';
        EnvironmentFile = "${builtins.path { name = "dotenv"; path = ../.env; }}";
      };
    };
  };
}
