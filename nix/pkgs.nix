{ compiler ? ""
, isJS ? false
, system ? "x86_64-linux"
, chan
}:
import ./base-pkgs.nix { inherit chan; } {
  inherit system;
  overlays = [
    (import ./overlay.nix { inherit chan compiler isJS; })
  ];
}
