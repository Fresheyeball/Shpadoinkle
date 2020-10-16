{ compiler, isJS }: with (import <nixpkgs> {}).lib; foldl' composeExtensions (_: _: {})
[
  (import ./overlay-reflex.nix { inherit compiler isJS; })
  (import ./overlay-shpadoinkle.nix { inherit compiler isJS; })
]
