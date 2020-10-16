{ isJS ? false
, compiler ? "ghc865"
, pack ? "all"
, chan ? "5272327b81ed355bbed5659b8d303cf2979b6953"
, withHoogle ? false
, extra ? (_: b: b)
, optimize ? true
, system ? "x86_64-linux"
}:

let
  pkgs = import ./nix/pkgs.nix { inherit compiler isJS system chan; };
in
  with pkgs; with lib;

  let
    optimizeJS = optimize && isJS && !inNixShell;

    util = import ./nix/util.nix { inherit compiler isJS; };
    docker = import ./examples/servant-crud/docker.nix { inherit compiler chan; };

    hlsrev = "875e9b94d0358a88bd6f4cd2a35161ee1ebce896";

    hls = (import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${hlsrev}.tar.gz";
    }) {}).haskell.packages.${compiler}.haskell-language-server;

    ghcTools = with haskell.packages.${compiler}; [ cabal-install ghcid hpack ];

    packages = {

      inherit (haskell.packages.${util.compilerjs})
        Shpadoinkle
        Shpadoinkle-backend-snabbdom
        Shpadoinkle-backend-static
        Shpadoinkle-backend-pardiff
        Shpadoinkle-console
        Shpadoinkle-lens
        Shpadoinkle-html
        Shpadoinkle-router
        Shpadoinkle-widgets
        Shpadoinkle-tests;
        Shpadoinkle-examples = (if optimizeJS then util.doCannibalize else id)
          haskell.packages.${util.compilerjs}.Shpadoinkle-examples;

    } // (
      if !isJS then { inherit (haskell.packages.${util.compilerjs})
        Shpadoinkle-isreal;
      } else {}
    );


    shellBase = {
      inherit withHoogle;
      packages = _: if pack == "all" then attrValues packages else [ packages.${pack} ];
      COMPILER = util.compilerjs;
      buildInputs = ghcTools ++ [ ack util.cannibalize ];
      shellHook = ''
        cat ${./etc/figlet}
        ./nix/hpackall.sh | grep generated
      '';
    };


  in


    if inNixShell
    then haskell.packages.${util.compilerjs}.shellFor (extra pkgs shellBase)
    else (if builtins.currentSystem == "x86_64-linux" && isJS then { inherit docker; } else {}) // packages
