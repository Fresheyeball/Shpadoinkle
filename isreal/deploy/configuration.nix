{ ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix # generated at runtime by nixos-infect
  ];

  boot.cleanTmpDir = true;
  networking.hostName = "isreal-swan";
  networking.firewall.allowPing = true;
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDeppey98/6oV5sXi/fy8fqxlc+tyYQlxiuVOhx8IcvZ chloe@daria"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILMaYwUX4MK8kWv0EW0ALvNMDAgIMvaLehvTSN28gf4R chloe@chloe-nixos01"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKZzKmSxSVqol+YNsGr5Cts5Cr/eIHqCm/KISHsluVtJ isaac@dunlap"
  ];
}
