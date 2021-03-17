{ compiler ? "ghc865"
, system ? builtins.currentSystem
, chan ? (import ../nix/chan.nix)
, optimize ? true
}:

let
  pkgs   = import ../nix/pkgs.nix { inherit compiler system chan; isJS = false;  };
  pkgsJS = import ../nix/pkgs.nix { inherit compiler system chan; isJS = true;   };
  util   = import ../nix/util.nix { inherit pkgs compiler; isJS = true; };
  opti = if optimize then util.doCannibalize else (x: x);
  file = if optimize then "all.min.js" else "all.js";
in
  pkgs.runCommand "marketing" {} ''
    mkdir $out
    ln -s ${./static} $out/static
    ln -s ${opti pkgsJS.haskell.packages.${util.compilerjs}.Shpadoinkle-marketing}/bin/run.jsexe/${file} $out/all.min.js
    ${pkgs.haskell.packages.${compiler}.Shpadoinkle-marketing}/bin/disembodied -o $out
  ''
