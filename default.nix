let
    rev = "24596d7a4a4b07bd9ce3ad748b8dabf5957003c5";


    jsaddle-src = pkgs.fetchFromGitHub
      { owner = "ghcjs";
        repo = "jsaddle";
        rev = "68208be806c49a2a0c9f037dfac85feae10a8c80";
        sha256 = "0acj0x716ikfb08ndib36jmwxkwq399lvkip46sfkh1ynn0pvc1c";
      };

    overlay = self: super: {
      haskellPackages = super.haskell.packages.ghc843.override {
        overrides = hself: hsuper: {
          jsaddle = hself.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
        };
      };
    };

    pkgs = import (builtins.fetchTarball {
        url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    }) {
      config.allowBroken = false;
      config.allowUnfree = true;
      overlays = [ overlay ];
    };

    src = gitignore.gitignoreSource [".git"] ./.;

    compiler = "ghc843";

    gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
      owner = "siers";
      repo = "nix-gitignore";
      rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
      sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
    }) {};


in with pkgs; haskellPackages.callCabal2nix "Shpadoinkle" src {}
