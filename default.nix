{ isJS ? false, compiler ? "ghc864", pack ? "all" }: let pkgs = import <nixpkgs> {}; in with pkgs; with lib;
let


  jsaddle-src = fetchFromGitHub
    { owner = "ghcjs";
      repo = "jsaddle";
      rev = "9b37e9972108f77e773ad0aa65ac2dd394d5f61e";
      sha256 = "0j1kbmck88drdgjj50si20n7iiyhbddgpwd3siclgpkpqa2gq1j0";
    };


  servant-src = fetchFromGitHub
    { owner = "haskell-servant";
      repo = "servant";
      rev = "b4e5aa0deff238e117137be68a4345bb02b7a80b";
      sha256 = "0x27bgrasbxzp045rqj4ldrfnm2k832ch7vfkl9s7xj0afrcy6pg";
    };


  gitignore = (callPackage (fetchFromGitHub
    { owner = "siers";
      repo = "nix-gitignore";
      rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
      sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
    }) {}).gitignoreSource
      [ ".git"
        "*.ghc*"
        "*.cabal"
        "*result*"
        "*dist*"
      ];


  chrome-rev = "9619debe3d8b99bc56342ec4e0ee818aaa5eb985";
  chrome = (import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${chrome-rev}.tar.gz";
    }) {}).google-chrome;


  targets = {
    Shpadoinkle                  = gitignore ./core;
    Shpadoinkle-backend-snabbdom = gitignore ./backends/snabbdom;
    Shpadoinkle-backend-static   = gitignore ./backends/static;
    Shpadoinkle-backend-pardiff  = gitignore ./backends/pardiff;
    Shpadoinkle-html             = gitignore ./html;
    Shpadoinkle-router           = gitignore ./router;
    Shpadoinkle-widgets          = gitignore ./widgets;
    Shpadoinkle-examples         = gitignore ./examples;
  };


  compilerjs = if isJS then "ghcjs${builtins.substring 3 2 compiler}" else compiler;


  haskellPackages = with haskell.lib; haskell.packages.${compilerjs}.extend (composeExtensions
      (packageSourceOverrides targets)
      (self: super: let
         dontCheckJs = if isJS then dontCheck else id;
      in {
          ghcWithPackages = p: super.ghcWithPackages (
            f: p f ++ (if inNixShell then [ f.cabal-install f.ghcid ] else [])
          );
          jsaddle              =              self.callCabal2nix "jsaddle"      "${jsaddle-src}/jsaddle" {};
          jsaddle-warp         = dontCheck   (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
          comonad              = dontCheckJs super.comonad;
          extra                = dontCheckJs super.extra;
          SHA                  = dontCheckJs super.SHA;
          pureMD5              = dontCheckJs super.pureMD5;
          unliftio             = dontCheckJs super.unliftio;
          semigroupoids        = dontCheckJs super.semigroupoids;
          megaparsec           = dontCheckJs super.megaparsec;
          lens                 = dontCheckJs super.lens;
          hpack                = haskell.packages.${compiler}.hpack;
          http-types           = dontCheckJs super.http-types;
          silently             = dontCheckJs super.silently;
          QuickCheck           = dontCheckJs super.QuickCheck;
          temporary            = dontCheckJs super.temporary;
          Diff                 = dontCheckJs super.Diff;
          hspec                = dontCheckJs super.hspec;
          servant              = dontCheckJs (self.callCabal2nix "servant"              "${servant-src}/servant" {});
          servant-server       = dontCheck   (self.callCabal2nix "servant-server"       "${servant-src}/servant-server" {});
          servant-client       = dontCheck   (self.callCabal2nix "servant-client"       "${servant-src}/servant-client" {});
          servant-client-ghcjs = dontCheck   (self.callCabal2nix "servant-client-ghcjs" "${servant-src}/servant-client-ghcjs" {});

          Shpadoinkle-tests    = haskell.packages.${compiler}.callCabal2nix "tests" (gitignore ./tests) {};
      })
  );

  ghcTools = with haskell.packages.${compiler}; [ stylish-haskell cabal-install ghcid ];

  packages = map (t: haskellPackages.${t}) (attrNames targets ++ [ "Shpadoinkle-tests" ]);

in

  if inNixShell
  then haskellPackages.shellFor {
    packages = _: if pack == "all" then packages else [ haskellPackages.${pack} ];
    COMPILER = compilerjs;
    EXAMPLES = "../result";
    CHROME   = "${chrome}/bin/google-chrome-stable";
    HEADLESS = false;
    buildInputs = [ selenium-server-standalone chromedriver chrome ] ++ ghcTools;
    withHoogle = true;
  } else foldl (ps: p: ps // { ${p.pname} = p; }) {} packages
