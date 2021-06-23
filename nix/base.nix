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


        eventlog2html = (import (pkgs.fetchFromGitHub {
          owner  = "Fresheyeball";
          repo   = "eventlog2html";
          rev    = "cad38a207470e3f32530ff1545a5eff7c9b42b01";
          sha256 = "1a3d244n76ii423vrxigclnkbd89m1sab9zw5ydn6fs2x263lwp2";
        }) {}).eventlog2html;

        # eventlog2html = (import ../../eventlog2html {}).eventlog2html;


        ghcTools = with haskell.packages.${compiler}; with haskell.lib;
          [ easy-hls pkgs.stylish-haskell pkgs.hlint]
          ++ (if enableLibraryProfiling then [ eventlog2html ] else [])
          ++ map disableLibraryProfiling
            ([ cabal-install ghcid ]
            ++ (if enableLibraryProfiling then [ hp2pretty ghc-prof-flamegraph ] else []));


        cannibal = if optimizeJS then util.doCannibalize else id;


        easy-hls = pkgs.callPackage (pkgs.fetchFromGitHub {
          owner  = "jkachmar";
          repo   = "easy-hls-nix";
          rev    = "9d64543a015563942c954b89addc1108800ed134";
          sha256 = "1szq3g34dv22fqzil549mvpdd1865s64vqnfxj0l2aw9ha32jxyz";
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
            Shpadoinkle-template;

            Shpadoinkle-examples = cannibal haskell.packages.${util.compilerjs}.Shpadoinkle-examples;
            Shpadoinkle-website  = cannibal haskell.packages.${util.compilerjs}.Shpadoinkle-website;


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

        });


        shellBase = {
          inherit withHoogle;
          packages = _: if pack == "all" then attrValues packages else [ packages.${pack} ];
          COMPILER = util.compilerjs;
          buildInputs = ghcTools ++ [ asciidoctor ack util.cannibalize nixops ];
          shellHook = ''
            cat ${../etc/figlet}
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
