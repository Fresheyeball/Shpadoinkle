

#  This file is part of Shpadoinkle Isreal.
#
#  Shpadoinkle Isreal is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  Shpadoinkle Isreal is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with Shpadoinkle Isreal.  If not, see <https://www.gnu.org/licenses/>.


{ chan ? (import ../nix/chan.nix)
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
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
      (import ../nix/overlay.nix
        { inherit chan enableLibraryProfiling enableExecutableProfiling;
          compiler = "ghc865";
          isJS = true;
        }
      )
      overlay
    ];
  };
in
with pkgs.haskell.packages; ghcjs86.shellFor {
  packages    = _: [ ghcjs86.swan ];
  buildInputs = [ ghc865.cabal-install ];
  withHoogle  = true;
}
