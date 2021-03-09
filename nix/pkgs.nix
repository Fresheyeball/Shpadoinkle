{ compiler ? ""
, isJS ? false
, system ? "x86_64-linux"
, chan ? (import ./chan.nix)
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
}:
import ./base-pkgs.nix { inherit chan; } {
  inherit system;
  overlays = [
    (import ./overlay.nix { inherit chan compiler isJS enableLibraryProfiling enableExecutableProfiling; })
  ];
}
