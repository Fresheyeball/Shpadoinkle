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
    cp ${./manifest.json} ./manifest.json
    cp ${./main.html}     ./main.html
    cp ${./main.js}       ./main.js
    cp ${./panel.html}    ./panel.html
    cp ${./icon.png}      ./icon.png
    cp ${./inject.js}     ./inject.js
    cp ${./style.css}     ./style.css
    cp ${util.doCannibalize dev.Shpadoinkle-developer-tools}/bin/devtools.jsexe/all.js ./all.js
    ${pkgs.zip}/bin/zip -r $out *
  ''
