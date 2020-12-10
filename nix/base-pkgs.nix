{ chan }:
import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
})
