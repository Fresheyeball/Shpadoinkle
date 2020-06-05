{ compiler ? "", isJS ? false, chan }:
import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
}) {
  overlays = [
    (import ./overlay.nix { inherit compiler isJS; })
  ];
}
