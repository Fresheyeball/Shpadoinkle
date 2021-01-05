{ isJS ? false
, compiler ? "ghc865"
, pack ? "all"
, chan ? (import ./nix/chan.nix)
, withHoogle ? false
, extra ? (_: b: b)
, optimize ? true
, system ? "x86_64-linux"
}:

let
  pkgs = import ./nix/pkgs.nix { inherit compiler isJS system chan; };
  util = import ./nix/util.nix { inherit pkgs compiler isJS; };
in
  with pkgs; with lib;

  let
    optimizeJS = optimize && isJS && !inNixShell;

    docker = import ./examples/servant-crud/docker.nix { inherit compiler chan; };

    ghcTools = with haskell.packages.${compiler};
      [ cabal-install ghcid hpack pkgs.stylish-haskell pkgs.hlint ];

    cannibal = if optimizeJS then util.doCannibalize else id;

    packages = {

      inherit (haskell.packages.${util.compilerjs})
        Shpadoinkle

        Shpadoinkle-backend-snabbdom
        Shpadoinkle-backend-static
        Shpadoinkle-backend-pardiff
        Shpadoinkle-console
        Shpadoinkle-developer-tools
        Shpadoinkle-lens
        Shpadoinkle-html
        Shpadoinkle-router
        Shpadoinkle-widgets
        Shpadoinkle-isreal

        Shpadoinkle-tests;
        Shpadoinkle-examples  = cannibal haskell.packages.${util.compilerjs}.Shpadoinkle-examples;
        Shpadoinkle-marketing = cannibal haskell.packages.${util.compilerjs}.Shpadoinkle-marketing;

    } // (
      if !isJS then { inherit (haskell.packages.${util.compilerjs})
        Shpadoinkle-disembodied;
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
