{ compiler ? "ghc865"
, system ? "x86_64-linux"
, chan ? "5272327b81ed355bbed5659b8d303cf2979b6953"
}:

let
  pkgs   = import ../nix/pkgs.nix { inherit compiler system chan; isJS = false;  };
  pkgsJS = import ../nix/pkgs.nix { inherit compiler system chan; isJS = true;   };
  util   = import ../nix/util.nix { inherit compiler; isJS = true; };
in
  pkgs.runCommand "marketing" {} ''
    mkdir $out
    ln -s ${./static} $out/static
    ln -s ${pkgsJS.haskell.packages.${util.compilerjs}.Shpadoinkle-marketing}/bin/run.jsexe/all.min.js $out/all.min.js
    ${pkgs.haskell.packages.${compiler}.Shpadoinkle-marketing}/bin/disembodied -o $out
  ''
