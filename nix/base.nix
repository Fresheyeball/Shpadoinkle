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
          owner  = "mpickering";
          repo   = "eventlog2html";
          rev    = "a18ec810328c71122ccc630fccfcea5b48c0e937";
          sha256 = "064x0y3ix5h22ibl9sn3znhkan6g1prkniv2hgrrssr0c4sgjvhb";
        }) {}).eventlog2html;

        # eventlog2html = (import ../../eventlog2html {}).eventlog2html;


        ghcTools = with haskell.packages.${compiler}; with haskell.lib;
          [ easy-hls pkgs.stylish-haskell pkgs.hlint]
          # ++ (if enableLibraryProfiling then [ eventlog2html ] else [])
          ++ map disableLibraryProfiling
            ([ cabal-install ghcid ]
            ++ (if enableLibraryProfiling then [ hp2pretty ghc-prof-flamegraph ] else []));


        cannibal = if optimizeJS then util.doCannibalize else id;


        easy-hls = pkgs.callPackage (pkgs.fetchFromGitHub {
          owner  = "jkachmar";
          repo   = "easy-hls-nix";
          rev    = "291cf77f512a7121bb6801cde35ee1e8b7287f91";
          sha256 = "1bvbcp9zwmh53sm16ycp8phhc6vzc72a71sf0bvyjgfbn6zp68bc";
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

          swan    = import ../snowman { inherit chan; };


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
