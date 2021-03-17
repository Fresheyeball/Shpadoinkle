{ compiler ? ""
, isJS ? false
, system ? builtins.currentSystem
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
