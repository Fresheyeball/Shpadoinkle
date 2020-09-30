{ compiler ? ""
, isJS ? false
, system ? "x86_64-linux"
, chan
}:
import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
}) {
  inherit system;
  overlays = [
    (import ./overlay.nix { inherit compiler isJS; })
  ];
}
