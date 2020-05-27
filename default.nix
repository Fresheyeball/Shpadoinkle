{ isJS ? false
, compiler ? "ghc864"
, pack ? "all"
, chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
, withHoogle ? false
, extra ? (_: b: b)
}:

let pkgs = import ./pkgs.nix compiler isJS chan; in with pkgs; with lib;
let


  util = import ./util.nix { inherit compiler isJS; };


  gitignore = pkgs.gitignore
      [ ".git"
        "*.ghc*"
        "*.cabal"
        "*result*"
        "*dist*"
        "*.nix"
        "*.md"
        ".*.yml"
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
    Shpadoinkle-experiments      = gitignore ./experiments;
  };


  haskellPackages = with haskell.lib; haskell.packages.${util.compilerjs}.extend
    (composeExtensions (packageSourceOverrides targets) (self: super: {
      hpack                = haskell.packages.${compiler}.hpack;
      Shpadoinkle-tests    = haskell.packages.${compiler}.callCabal2nix "tests" (gitignore ./tests) {};
    }));


  ghcTools = with haskell.packages.${compiler}; [ stylish-haskell cabal-install ghcid hpack ] ++
    []; # (if isJS then [ haskell.compiler.ghcjs86 ] else []);
  packages = map (t: haskellPackages.${t}) (attrNames targets ++ [ "Shpadoinkle-tests" ]);


  shellBase = {
    inherit withHoogle;
    packages    = _: if pack == "all" then packages else [ haskellPackages.${pack} ];
    COMPILER    = util.compilerjs;
    buildInputs = ghcTools;
    shellHook   = ''
      cat ${./etc/figlet}
    '';
  };


in


  if inNixShell
  then haskellPackages.shellFor (extra pkgs shellBase)
  else foldl (ps: p: ps // { ${p.pname} = p; }) {} packages
