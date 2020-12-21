{ compiler ? ""
, isJS ? false
, system ? "x86_64-linux"
, chan ? "5272327b81ed355bbed5659b8d303cf2979b6953"
}:
import ./base-pkgs.nix { inherit chan; } {
  inherit system;
  overlays = [
    (import ./overlay.nix { inherit chan compiler isJS; })
  ];
}
