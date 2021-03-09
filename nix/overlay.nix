{ chan, compiler, isJS, enableLibraryProfiling, enableExecutableProfiling }:
with (import ./base-pkgs.nix { inherit chan; } {}).lib;
foldl' composeExtensions (_: _: {})
[
  (import ./overlay-reflex.nix { inherit compiler isJS enableLibraryProfiling; })
  (import ./overlay-shpadoinkle.nix { inherit compiler isJS; })
]
