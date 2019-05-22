{ compiler ? "ghcjs84" }: let


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


  gitignore = (pkgs.callPackage (pkgs.fetchFromGitHub
    { owner = "siers";
      repo = "nix-gitignore";
      rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
      sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
    }) {}).gitignoreSource
      [ ".git"
        ".ghc.environment.x86_64-linux-8.4.3"
        "*.cabal"
      ];


  targets = {
    Shpadoinkle                  = gitignore ./core;
    # Shpadoinkle-backend-snabbdom = gitignore ./backends/snabbdom;
    Shpadoinkle-backend-pardiff  = gitignore ./backends/pardiff;
    Shpadoinkle-html             = gitignore ./html;
    Shpadoinkle-examples         = gitignore ./examples;
  };


  haskellPackages = with pkgs.haskell.lib; pkgs.haskell.packages.${compiler}.extend (pkgs.lib.composeExtensions
      (packageSourceOverrides targets)
      (self: super: {
          ghcWithPackages = p: super.ghcWithPackages (
            f: p f ++ (if false && pkgs.lib.inNixShell then [ f.cabal-install f.ghcid ] else [])
          );
          jsaddle       = self.callCabal2nix "jsaddle"      "${jsaddle-src}/jsaddle" {};
          jsaddle-warp  = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
          comonad       = dontCheck super.comonad;
          extra         = dontCheck super.extra;
          unliftio      = dontCheck super.unliftio;
          semigroupoids = dontCheck super.semigroupoids;
          lens          = dontCheck super.lens;
          hpack         = dontCheck super.hpack; # why?
          http-types    = dontCheck super.http-types;
          silently      = dontCheck super.silently;
      })
  );


  packages = map (t: haskellPackages.${t}) (builtins.attrNames targets);
  buildSet = pkgs.lib.foldl (ps: p: ps // { ${p.pname} = p; }) {} packages;
  tools = [ pkgs.pkgconfig ];

in

  if pkgs.lib.inNixShell
  then haskellPackages.shellFor { packages = _: packages; buildInputs = tools; }
  else buildSet
