{ compiler ? "ghc865"
, ci ? false
, system ? builtins.currentSystem
, chan ? (import ../nix/chan.nix)
, optimize ? true
}:


#  This file is part of Shpadoinkle Website.
#
#  Shpadoinkle Website is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  Shpadoinkle Website is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with Shpadoinkle Website.  If not, see <https://www.gnu.org/licenses/>.


let
  pkgs   = import ../nix/pkgs.nix { inherit compiler system chan; isJS = false;  };
  pkgsJS = import ../nix/pkgs.nix { inherit compiler system chan; isJS = true;   };
  util   = import ../nix/util.nix { inherit pkgs compiler; isJS = true; };
  brand  = import ./brand.nix     { inherit chan; };
  opti   = if optimize then util.doCannibalize else (x: x);
  file   = if optimize then "all.min.js" else "all.js";
  assets = util.gitignore [
    "landing_logo.svg"
  ] ./assets;
  setTarget = x: y: with pkgs.haskell.lib; dontHaddock (overrideCabal x (_: { buildTarget = y; installPhase = ''
    runHook preInstall
    ./Setup copy ${y}
    runHook postInstall
  '';} ));
  setMay = if ci then (x: _: x) else setTarget;
in
  pkgs.runCommand "website" {
    LANG = "C.UTF-8";
  } ''
    mkdir $out
    ${brand.icons.mkIcons brand.logo.thumbnail "$out"}
    mkdir $out/assets
    for asset in ${assets}/*
    do
      ln -s $asset $out/assets/$(basename $asset)
    done
    ln -s ${brand.logo.full.svg { color = "bright_yellow"; }} $out/assets/landing_logo.svg
    ln -s ${opti (setMay pkgsJS.haskell.packages.${util.compilerjs}.Shpadoinkle-website "run")}/bin/run.jsexe/${file} $out/all.min.js
    ${setMay pkgs.haskell.packages.${compiler}.Shpadoinkle-website "disembodied"}/bin/disembodied -o $out
    cp $out/404/index.html $out/404.html
    echo ${pkgs.lib.commitIdFromGitRepo ../.git} > $out/version
  ''
