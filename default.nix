{ isJS ? false
, compiler ? "ghc864"
, pack ? "all"
, chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
, withHoogle ? false
, extra ? (_: b: b)
}:

let pkgs = import ./nix/pkgs.nix { inherit compiler isJS chan; }; in with pkgs; with lib;
let


  util = import ./nix/util.nix { inherit compiler isJS; };
  docker = import ./examples/servant-crud/docker.nix { inherit compiler chan; };


  ghcTools = with haskell.packages.${compiler}; [ stylish-haskell cabal-install ghcid hpack ];


  packages = {
    inherit (haskell.packages.${util.compilerjs})
    Shpadoinkle
    Shpadoinkle-backend-snabbdom
    Shpadoinkle-backend-static
    Shpadoinkle-backend-pardiff
    Shpadoinkle-debug
    Shpadoinkle-lens
    Shpadoinkle-html
    Shpadoinkle-router
    Shpadoinkle-widgets
    Shpadoinkle-examples
    Shpadoinkle-experiments
    Shpadoinkle-tests;
  };


  shellBase = {
    inherit withHoogle;
    packages    = _: if pack == "all" then attrValues packages else [ packages.${pack} ];
    COMPILER    = util.compilerjs;
    buildInputs = ghcTools ++ [ ack ];
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
  else { inherit docker; } // packages
