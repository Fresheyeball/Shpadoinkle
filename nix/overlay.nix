{ chan, compiler, isJS }:
with (import ./base-pkgs.nix { inherit chan; } {}).lib;
foldl' composeExtensions (_: _: {})
[
  (import ./overlay-reflex.nix { inherit compiler isJS; })
  (import ./overlay-shpadoinkle.nix { inherit compiler isJS; })
]
