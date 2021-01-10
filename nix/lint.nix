{ pkgs ? import ./pkgs.nix {}
}:
with pkgs;
mkShell {
  buildInputs = [ stylish-haskell hlint ];
}
