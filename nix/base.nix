let
  f =
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
    in with pkgs; with lib;


      let
        optimizeJS = optimize && isJS && !inNixShell;


        docker = import ../examples/servant-crud/docker.nix { inherit compiler chan; };


        ghcTools = with haskell.packages.${compiler};
          [ easy-hls cabal-install ghcid hpack pkgs.stylish-haskell pkgs.hlint ];


        cannibal = if optimizeJS then util.doCannibalize else id;


        easy-hls = pkgs.callPackage (pkgs.fetchFromGitHub {
          owner  = "jkachmar";
          repo   = "easy-hls-nix";
          rev    = "cf0cb016e1c57934592fd4c9d07d6b7a67d3f6ce";
          sha256 = "1whs5xckd1p4r8xskyfh5h098ks0fw1ki3ccjgb1fpmc4hbdx7sb";
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
          if isJS then {} else { inherit (haskell.packages.${util.compilerjs})

            Shpadoinkle-disembodied;

          }
        ) // (
          if build-or-shell == "shell" then {} else {


          snowman = import ../snowman/template {
            inherit chan isJS system enableLibraryProfiling enableExecutableProfiling;
            shpadoinkle-path = ../.;
          };

          swan    = import ../snowman { inherit chan; };


        });




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
          builtins.throw ''The first argument to this function must be either "build" or "shell"'';


in
  { build = f "build";
    shell = f "shell";
  }
