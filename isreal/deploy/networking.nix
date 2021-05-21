{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [ "8.8.8.8"
 ];
    defaultGateway = "104.131.0.1";
    defaultGateway6 = "";
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address="104.131.28.246"; prefixLength=18; }
          { address="10.17.0.5"; prefixLength=16; }
        ];
        ipv6.addresses = [
          { address="fe80::e8a3:8cff:fe62:6d3b"; prefixLength=64; }
        ];
        ipv4.routes = [ { address = "104.131.0.1"; prefixLength = 32; } ];
        ipv6.routes = [ { address = ""; prefixLength = 32; } ];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="ea:a3:8c:62:6d:3b", NAME="eth0"
    ATTR{address}=="a6:bc:4f:25:08:08", NAME="eth1"
  '';
}
