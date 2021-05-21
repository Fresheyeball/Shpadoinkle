

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


{ lib, config, ... }:
with lib;
let
  cfg       = config.services.shpadoinkle-isreal;
  chan      = "5272327b81ed355bbed5659b8d303cf2979b6953";
  pkgs      = import ../nix/pkgs.nix { inherit chan; isJS = false; compiler = "ghc865"; };
  inherit (pkgs.haskell.packages.ghc865) Shpadoinkle-isreal cabal-install;
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

    hoogle   = mkOption {
      default = 8998;
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

        echo "serving hoogle on port ${toString cfg.hoogle}"
        hoogle server --local --port=${toString cfg.hoogle} &

        echo "starting Isreal Swan"
        cabal update
        ${Shpadoinkle-isreal}/bin/isreal +RTS -N8 -H1G -A32M -RTS &> ${cfg.workspace}/isreal-log
      '';

    };
  };
}
