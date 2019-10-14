{ compiler ? "ghc843", pack ? "all" }: let pkgs = import ./pkgs.nix; in with pkgs; with lib;
let

  jsaddle-src = fetchFromGitHub
    { owner = "ghcjs";
      repo = "jsaddle";
      rev = "68208be806c49a2a0c9f037dfac85feae10a8c80";
      sha256 = "0acj0x716ikfb08ndib36jmwxkwq399lvkip46sfkh1ynn0pvc1c";
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
    Shpadoinkle-backend-pardiff  = gitignore ./backends/pardiff;
    Shpadoinkle-html             = gitignore ./html;
    Shpadoinkle-widgets          = gitignore ./widgets;
    Shpadoinkle-examples         = gitignore ./examples;
  };


  haskellPackages = with haskell.lib; haskell.packages.${compiler}.extend (pkgs.lib.composeExtensions
      (packageSourceOverrides targets)
      (self: super: {
          ghcWithPackages = p: super.ghcWithPackages (
            f: p f ++ (if inNixShell then [ f.cabal-install f.ghcid ] else [])
          );
          jsaddle           = self.callCabal2nix            "jsaddle"      "${jsaddle-src}/jsaddle" {};
          jsaddle-warp      = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
          comonad           = dontCheck super.comonad;
          extra             = dontCheck super.extra;
          SHA               = dontCheck super.SHA;
          pureMD5           = dontCheck super.pureMD5;
          unliftio          = dontCheck super.unliftio;
          semigroupoids     = dontCheck super.semigroupoids;
          megaparsec        = dontCheck super.megaparsec;
          lens              = dontCheck super.lens;
          hpack             = haskell.packages.ghc843.hpack;
          http-types        = dontCheck super.http-types;
          silently          = dontCheck super.silently;
          Shpadoinkle-tests = haskell.packages.ghc843.callCabal2nix "tests" (gitignore ./tests) {};
      })
  );

  ghc843Tools = with haskell.packages.ghc843; [ stylish-haskell cabal-install ghcid ];

  packages = map (t: haskellPackages.${t}) (attrNames targets ++ [ "Shpadoinkle-tests" ]);

in

  if inNixShell
  then haskellPackages.shellFor {
    packages = _: if pack == "all" then packages else [ haskellPackages.${pack} ];
    COMPILER = "ghcjs84";
    EXAMPLES = "../result";
    CHROME   = "${chrome}/bin/google-chrome-stable";
    HEADLESS = false;
    buildInputs = [ selenium-server-standalone chromedriver chrome ] ++ ghc843Tools;
    withHoogle = true;
  } else foldl (ps: p: ps // { ${p.pname} = p; }) {} packages
