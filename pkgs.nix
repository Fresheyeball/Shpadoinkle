let rev = "ad85b9a9f8d81d6f3fe8d5006c917ab123d2f62f";
 in import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    }) {}
