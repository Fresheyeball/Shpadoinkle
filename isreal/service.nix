

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


let ports = { isreal = 8080; hoogle = 8090; };
in {
services = {

  nginx = {
    enable = true;
    recommendedGzipSettings  = true;
    recommendedOptimisation  = true;
    recommendedTlsSettings   = true;
    recommendedProxySettings = true;
    virtualHosts = let
      mkLocation = pp: {
        forceSSL   = true;
        enableACME = true;
        locations."/error_page.html" = {
          root = "/usr/www";
        };
        locations."/notfound_page.html" = {
          root = "/usr/www";
        };
        locations."/" = {
          proxyPass   = "http://localhost:${toString pp}";
          extraConfig = builtins.readFile ./headers.nginx;
        };
      };
    in {
      "isreal.shpadoinkle.org" = mkLocation ports.isreal;
      "hoogle.shpadoinkle.org" = mkLocation ports.hoogle;
    };
  };

  shpadoinkle-isreal = {
    enable = true;
    port   = ports.isreal;
    hoogle = ports.hoogle;
  };

};
}
