{ isJS ? false, compiler ? "ghc864", pack ? "all", chan ? "e1843646b04fb564abf6330a9432a76df3269d2f", withHoogle ? false }:

let pkgs = import ./pkgs.nix compiler isJS chan; in with pkgs; with lib;
let


  util = import ./util.nix { inherit compiler isJS; };


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
        "*.nix"
        "*.md"
      ];


  targets = {
    Shpadoinkle                  = gitignore ./core;
    Shpadoinkle-backend-snabbdom = gitignore ./backends/snabbdom;
    Shpadoinkle-backend-static   = gitignore ./backends/static;
    Shpadoinkle-backend-pardiff  = gitignore ./backends/pardiff;
    Shpadoinkle-lens             = gitignore ./lens;
    Shpadoinkle-html             = gitignore ./html;
    Shpadoinkle-router           = gitignore ./router;
    Shpadoinkle-widgets          = gitignore ./widgets;
    Shpadoinkle-examples         = gitignore ./examples;
  };


  haskellPackages = with haskell.lib; haskell.packages.${util.compilerjs}.extend
    (composeExtensions (packageSourceOverrides targets) (self: super: {
      hpack                = haskell.packages.${compiler}.hpack;
      Shpadoinkle-tests    = haskell.packages.${compiler}.callCabal2nix "tests" (gitignore ./tests) {};
    }));


  ghcTools = with haskell.packages.${compiler}; [ stylish-haskell cabal-install ghcid hpack ];
  packages = map (t: haskellPackages.${t}) (attrNames targets ++ [ "Shpadoinkle-tests" ]);


in


  if inNixShell
  then haskellPackages.shellFor {
    packages = _: if pack == "all" then packages else [ haskellPackages.${pack} ];
    COMPILER = util.compilerjs;
    EXAMPLES = "../result";
    CHROME   = "${google-chrome}/bin/google-chrome-stable";
    HEADLESS = false;
    buildInputs = [ selenium-server-standalone chromedriver google-chrome ] ++ ghcTools;
    inherit withHoogle;
    shellHook = ''
      hpack
    '';
  } else foldl (ps: p: ps // { ${p.pname} = p; }) {} packages
