{ compiler ? "ghc865"
, chan ? (import ../nix/chan.nix)
}:
let

  chrome-webstore =
    { name        = "Shpadoinkle Developer Tools";
      description = "Time travel debugging for Shpadoinkle";
      author      = "Isaac Shapira";
      version     = "0.0.1";
    };

  pkgs = import ../nix/pkgs.nix {
    inherit compiler chan; isJS = true; };

  util = import ../nix/util.nix { inherit pkgs compiler; isJS = true; };

  dev  = pkgs.haskell.packages.${util.compilerjs};

  patch-manifest = with chrome-webstore; ''
    ${pkgs.jq}/bin/jq '.name = "${name}" | .description = "${description}" | .version = "${version}" | .author = "${author}"' < manifest.json > manifest-tmp.json
    mv manifest-tmp.json manifest.json
  '';

in pkgs.runCommand "Shpadoinkle-developer-tools.zip" {}
  ''
    cp ${./manifest.json} ./manifest.json
    cp ${./main.html}     ./main.html
    cp ${./main.js}       ./main.js
    cp ${./panel.html}    ./panel.html
    cp ${./icon.png}      ./icon.png
    cp ${./inject.js}     ./inject.js
    cp ${./style.css}     ./style.css
    ${patch-manifest}
    cp ${util.doCannibalize dev.Shpadoinkle-developer-tools}/bin/devtools.jsexe/all.js ./all.js
    ${pkgs.zip}/bin/zip -r $out *
  ''
