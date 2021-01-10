{ chan ? (import ../nix/chan.nix)
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
      (import ../nix/overlay.nix { inherit chan; compiler = "ghc865"; isJS = true; })
      overlay
    ];
  };
in
with pkgs.haskell.packages; ghcjs86.shellFor {
  packages    = _: [ ghcjs86.swan ];
  buildInputs = [ ghc865.cabal-install ];
  withHoogle  = true;
}
