{ compiler ? "ghc865"
, chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
}:
let

  pkgs = import ../nix/pkgs.nix {
    inherit compiler chan; isJS = true; };

  util = import ../nix/util.nix { inherit pkgs compiler; isJS = true; };

  dev  = pkgs.haskell.packages.${util.compilerjs};

in pkgs.runCommand "Shpadoinkle-developer-tools" {}
  ''
    mkdir $out
    cp ${./manifest.json} $out/manifest.json
    cp ${./main.html}     $out/main.html
    cp ${./main.js}       $out/main.js
    cp ${./panel.html}    $out/panel.html
    cp ${./icon.png}      $out/icon.png
    cp ${./inject.js}     $out/inject.js
    cp ${dev.Shpadoinkle-developer-tools}/bin/devtools.jsexe/all.js $out/all.js
  ''
