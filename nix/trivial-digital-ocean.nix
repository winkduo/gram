{
  resources.sshKeyPairs.ssh-key = {};

  gramserver = { config, pkgs, ... }: {
    services.nginx.enable = true;
    services.openssh.enable = true;

    deployment.targetEnv = "digitalOcean";
    deployment.digitalOcean.enableIpv6 = true;
    deployment.digitalOcean.region = "LON1";
    deployment.digitalOcean.size = "512mb";
  };
}
