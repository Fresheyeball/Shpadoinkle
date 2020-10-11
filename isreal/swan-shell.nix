{ chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
}:
let
  overlay = self: super: {
    haskell = super.haskell // { packages = super.haskell.packages // {
      ghcjs86 = super.haskell.packages.ghcjs86.override (old:
        { overrides = super.lib.composeExtensions (old.overrides or (_:_: {})) (hself: hsuper: {
            swan = hself.callCabal2nix "swan" ./swan {};
          });
        });
      };
    };
  };
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
  }) {
    overlays = [
      (import ../nix/overlay.nix { compiler = "ghc864"; isJS = true; })
      overlay
    ];
  };
in
with pkgs.haskell.packages; ghcjs86.shellFor {
  packages = _: [ ghcjs86.swan ];
  buildInputs = [ ghc864.cabal-install ];
}
