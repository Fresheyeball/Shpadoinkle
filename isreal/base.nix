

#  This file is part of Shpadoinkle Isreal.
#
#  Shpadoinkle Isreal is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  Shpadoinkle Isreal is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with Shpadoinkle Isreal.  If not, see <https://www.gnu.org/licenses/>.


{ pkgs, ...}: {

  boot.cleanTmpDir = true;

  time.timeZone = "Etc/UTC";

  programs.bash = {
    enableCompletion = true;
    promptInit = ''
      export TERM=xterm
    '';
  };

  environment.systemPackages = with pkgs; [ ripgrep curl vim git ];

  services = {
    openssh = {
      enable = true;
      passwordAuthentication = false;
      # Priority 0 (top) in order to override configuration.nix
      authorizedKeysFiles = pkgs.lib.mkOverride 0 [ "/etc/ssh/authorized_keys.d/%u" ];
    };
    fail2ban.enable = true;
    locate.enable   = true;
    timesyncd = {
      enable  = true;
      servers = [
        "time.nist.gov"
        "time-a-b.nist.gov"
        "time-b-b.nist.gov"
        "time-c-b.nist.gov"
        "time-d-b.nist.gov"
        "utcnist.colorado.edu"
        "utcnist2.colorado.edu"
      ];
    };
  };

  security = {
    audit.enable  = true;
    auditd.enable = true;
    acme = {
      email       = "isaac.shapira@platonic.systems";
      acceptTerms = true;
    };
  };

  users = {
    mutableUsers = false;
    users.isaac = {
      extraGroups = [ "wheel" ];
      uid         = 1234;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKZzKmSxSVqol+YNsGr5Cts5Cr/eIHqCm/KISHsluVtJ isaac@dunlap"
      ];
      shell = pkgs.bash;
    };
    extraUsers   = {
      root.openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDHd4oMVC3OZpzi2QGB6Qp82YPx6eRo7Da8JdrYfo0jK isaac@dunlap"
      ];
    };
    motd = builtins.readFile ./../etc/motd;
  };

  networking.firewall = {
    enable          = true;
    rejectPackets   = true;
    allowedTCPPorts = [ 80 443 ];
  };

}
