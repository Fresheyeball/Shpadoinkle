{ isJS ? false
, compiler ? "ghc864"
, pack ? "all"
, chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
, withHoogle ? false
, extra ? (_: b: b)
, optimize ? true
, system ? "x86_64-linux"
}:

let pkgs = import ./nix/pkgs.nix { inherit compiler isJS system chan; }; in with pkgs; with lib;

let
  optimizeJS = optimize && isJS && !inNixShell;

  util   = import ./nix/util.nix { inherit compiler isJS; };
  docker = import ./examples/servant-crud/docker.nix { inherit compiler chan; };

  ghcTools = with haskell.packages.${compiler}; [ stylish-haskell cabal-install ghcid hpack ];

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

    } // (if !isJS then {

    inherit (haskell.packages.${util.compilerjs})
    Shpadoinkle-isreal;

    } else {});


  shellBase = {
    inherit withHoogle;
    packages    = _: if pack == "all" then attrValues packages else [ packages.${pack} ];
    COMPILER    = util.compilerjs;
    buildInputs = ghcTools ++ [ ack util.cannibalize ];
    shellHook   = ''
      cat ${./etc/figlet}
      ./hpackall.sh | grep generated
      ${if pack == "all" then ''
        echo ""
        echo " | ⚠ WARNING, a bug in the shellFor Nix function, prevents some things from working."
        echo " | Please run nix-shell with a specific package target to get around this."
        echo " | IE: nix-shell examples"
        '' else ""}
    '';
  };


in


  if inNixShell
  then haskell.packages.${util.compilerjs}.shellFor (extra pkgs shellBase)
  else (if builtins.currentSystem == "x86_64-linux" then { inherit docker; } else {}) // packages
