build-or-shell:
{ isJS ? false
, compiler ? "ghc865"
, pack ? "all"
, chan ? (import ./chan.nix)
, withHoogle ? false
, extra ? (_: b: b)
, optimize ? true
, system ? builtins.currentSystem
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
}:

let
  pkgs = import ./pkgs.nix { inherit compiler isJS system chan enableLibraryProfiling enableExecutableProfiling; };
  util = import ./util.nix { inherit pkgs compiler isJS; };
in
  with pkgs; with lib;

  let
    optimizeJS = optimize && isJS && !inNixShell;

    docker = import ../examples/servant-crud/docker.nix { inherit compiler chan; };

    ghcTools = with haskell.packages.${compiler};
      [ easy-hls cabal-install ghcid hpack pkgs.stylish-haskell pkgs.hlint ];

    cannibal = if optimizeJS then util.doCannibalize else id;

    easy-hls = pkgs.callPackage (pkgs.fetchFromGitHub {
      owner  = "jkachmar";
      repo   = "easy-hls-nix";
      rev    = "b0ceb9277963eb39a8bb279f187e38b36d7d63db";
      sha256 = "1UD7GIHLZyJueRMPpUiV1SoeBEwFyz6tgCRijDvfWkU=";
    }) {
      ghcVersions = [ "8.6.5" ];
    };

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
        Shpadoinkle-streaming
        Shpadoinkle-widgets
        Shpadoinkle-isreal
        Shpadoinkle-template

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
        cat ${../etc/figlet}
        ./nix/hpackall.sh | grep generated
      '';
    };


  in
    if build-or-shell == "build" then
      (if builtins.currentSystem == "x86_64-linux" && isJS then
        { inherit docker; }
      else
        {}
      )
        // packages
    else if build-or-shell == "shell" then
      haskell.packages.${util.compilerjs}.shellFor (extra pkgs shellBase)
    else
        builtins.throw ''The first argument to this function must be either "build" or "shell"''
