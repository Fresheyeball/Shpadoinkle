{forShell ? false}:

let


  rev = "24596d7a4a4b07bd9ce3ad748b8dabf5957003c5";

  pkgs = import (builtins.fetchTarball {
        url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    }) {};

  jsaddle-src = pkgs.fetchFromGitHub
    { owner = "ghcjs";
      repo = "jsaddle";
      rev = "68208be806c49a2a0c9f037dfac85feae10a8c80";
      sha256 = "0acj0x716ikfb08ndib36jmwxkwq399lvkip46sfkh1ynn0pvc1c";
    };

  ghc = pkgs.haskell.packages.ghc843;

  targets = {
    Shpadoinkle = ./core;
    Shpadoinkle-backend-snabbdom = ./backends/snabbdom;
  };

  haskellPackages = ghc.extend (pkgs.lib.composeExtensions
      (pkgs.haskell.lib.packageSourceOverrides (targets // {
        # any version overrides or submodule package dependencies here
        # some-package = "0.2.0.0";           # fetch specific hackage version
        # some-package = ./dir/subproject;    # git submodule packages
      }))
      (self: super: {
          ghcWithPackages = p: super.ghcWithPackages (
              # I like to have ghci-pretty and ghcid available in my nix-shell
              f: p f ++ (if forShell then [ f.cabal-install f.ghcid ] else [])
          );
          jsaddle = self.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
          # any fetch/overrides go here
          # some-package = pkgs.haskell.lib.dontCheck (self.callCabal2nix "some-package" (pkgs.fetchFromGitHub { ... }) {});
          # some-package = pkgs.haskell.lib.overrideCabal super.some-package (drv: { patches = (drv.patches or [ ... ]); });
      })
  );

  buildSet = pkgs.lib.foldl (ps: p: ps // { ${p.pname} = p; }) {} packages;
  packages = map (t: haskellPackages.${t} ) (builtins.attrNames targets);
  tools = [ pkgs.pkgconfig ];

in

  if forShell

  then haskellPackages.shellFor { packages = _: packages; buildInputs = tools; }

  else buildSet
