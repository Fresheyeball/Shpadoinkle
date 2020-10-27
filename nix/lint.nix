{ pkgs ? import ./pkgs.nix { chan = "5272327b81ed355bbed5659b8d303cf2979b6953"; }
}:
with pkgs;
mkShell {
  buildInputs = [ stylish-haskell hlint ];
}
