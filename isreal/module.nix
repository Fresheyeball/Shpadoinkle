{ lib, config, ... }:
with lib;
let
  cfg       = config.services.shpadoinkle-isreal;
  chan      = "5272327b81ed355bbed5659b8d303cf2979b6953";
  pkgs      = import ../nix/pkgs.nix { inherit chan; isJS = false; compiler = "ghc865"; };
  isreal    = pkgs.haskell.packages.ghc865.Shpadoinkle-isreal;
  cabal     = pkgs.haskell.packages.ghc865.cabal-install;
  swan      = import ./swan-shell.nix {};
  transmute = y: xs: map (x: "${x}=${y.${x}}") xs;
  buildPath = y: xs: pkgs.lib.strings.concatStringsSep ":"
   (map (i: "${i.outPath}/bin") (builtins.filter (x: x != null) (xs ++ y.nativeBuildInputs ++ y.buildInputs)));
in {

  options.services.shpadoinkle-isreal = {
    enable = mkEnableOption "Snowman as a service";

    port   = mkOption {
      default = 8080;
    };

    workspace = mkOption {
      default     = "/var/colorado";
      description = "Working folder for snowmen. There will be much disk IO here.";
    };

  };

  config = mkIf cfg.enable {
    systemd.services.shpadoinkle-isreal = {
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Environment = [
          "TERRITORY=${cfg.workspace}"
          "HOME=${cfg.workspace}/hovel"
          "SWAN=${./swan}"
          "PORT=${toString cfg.port}"
          "PATH=${buildPath swan [ pkgs.coreutils ]}"
        ] ++ transmute swan [
          "LANG"
          "LOCALE_ARCHIVE"
          "NIX_GHCJS"
          "NIX_GHCJSPKG"
          "NIX_GHCJS_DOCDIR"
          "NIX_GHCJS_LIBDIR"
        ];
      };

      script = ''
        rm -rf $HOME
        mkdir -p $HOME
        ${isreal}/bin/isreal
      '';

    };
  };
}
