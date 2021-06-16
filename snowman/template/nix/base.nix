let
  f =
    build-or-shell:
    { chan ? "CHAN"
    , compiler ? "ghc865"
    , withHoogle ? false
    , doHoogle ? false
    , doHaddock ? false
    , enableLibraryProfiling ? false
    , enableExecutableProfiling ? false
    , strictDeps ? false
    , isJS ? false
    , system ? builtins.currentSystem
    , optimize ? true
    , shpadoinkle-path ? null
    }:
    let


      # It's a shpadoinkle day
      shpadoinkle = if shpadoinkle-path != null then shpadoinkle-path else builtins.fetchGit {
        url    = https://gitlab.com/platonic/shpadoinkle.git;
        ref    = "master";
        rev    = "REV";
      };


      # Additional ignore patterns to keep the Nix src clean
      ignorance = [
        "*.md"
        "figlet"
        "*.nix"
        "*.sh"
        "*.yml"
        "result"
      ];


      # Get some utilities
      inherit (import (shpadoinkle + "/nix/util.nix") { inherit compiler isJS pkgs; }) compilerjs gitignore doCannibalize;


      # Build faster by doing less
      chill = p: (pkgs.haskell.lib.overrideCabal p {
        inherit enableLibraryProfiling enableExecutableProfiling;
      }).overrideAttrs (_: {
        inherit doHoogle doHaddock strictDeps;
      });


      # Overlay containing Shpadoinkle packages, and needed alterations for those packages
      # as well as optimizations from Reflex Platform
      shpadoinkle-overlay =
        import (shpadoinkle + "/nix/overlay.nix") { inherit compiler chan isJS enableLibraryProfiling enableExecutableProfiling; };


      # Haskell specific overlay (for you to extend)
      haskell-overlay = hself: hsuper: {
        "happy" = pkgs.haskell.lib.dontCheck hsuper.happy;
      };


      # Top level overlay (for you to extend)
      snowman-overlay = self: super: {
        haskell = super.haskell //
          { packages = super.haskell.packages //
            { ${compilerjs} = super.haskell.packages.${compilerjs}.override (old: {
                overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) haskell-overlay;
              });
            };
          };
        };


      # Complete package set with overlays applied
      pkgs = import
        (builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
        }) {
        inherit system;
        overlays = [
          shpadoinkle-overlay
          snowman-overlay
        ];
      };


      ghcTools = with pkgs.haskell.packages.${compiler};
        [ cabal-install
          ghcid
        ] ++ (if isJS then [] else [ stylish-haskell ]);


      # We can name him George
      snowman = pkgs.haskell.packages.${compilerjs}.callCabal2nix "snowman" (gitignore ignorance ../.) {};


    in with pkgs; with lib;

      { build =
          (if isJS && optimize then doCannibalize else x: x) (chill snowman);

        shell =
          pkgs.haskell.packages.${compilerjs}.shellFor {
            inherit withHoogle;
            packages    = _: [ snowman ];
            COMPILER    = compilerjs;
            buildInputs = ghcTools;
            shellHook   = ''
              ${lolcat}/bin/lolcat ${../figlet}
              cat ${../intro}
            '';
          };
      }.${build-or-shell};
in
  { build = f "build";
    shell = f "shell";
  }
