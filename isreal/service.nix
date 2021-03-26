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
