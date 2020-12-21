{ compiler ? "ghc865"
, chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
}:
let

  pkgs = import ../nix/pkgs.nix {
    inherit compiler chan; isJS = true; };

  util = import ../nix/util.nix { inherit pkgs compiler; isJS = true; };

  dev  = pkgs.haskell.packages.${util.compilerjs};

in pkgs.runCommand "Shpadoinkle-developer-tools.zip" {}
  ''
    mkdir temp
    cp ${./manifest.json} temp/manifest.json
    cp ${./main.html}     temp/main.html
    cp ${./main.js}       temp/main.js
    cp ${./panel.html}    temp/panel.html
    cp ${./icon.png}      temp/icon.png
    cp ${./inject.js}     temp/inject.js
    cp ${./style.css}     temp/style.css
    cp ${util.doCannibalize dev.Shpadoinkle-developer-tools}/bin/devtools.jsexe/all.js temp/all.js
    ${pkgs.zip}/bin/zip -r $out temp
  ''
