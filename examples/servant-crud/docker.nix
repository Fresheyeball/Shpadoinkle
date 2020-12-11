{ compiler ? "ghc864"
, chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
}:
let
  pkgs = import ../../nix/pkgs.nix { inherit compiler chan; };
  inherit (import ../../nix/util.nix { inherit pkgs compiler; isJS = true; }) compilerjs;
  pkgsJS  = import ../../nix/pkgs.nix { inherit compiler chan; isJS = true; };
  pkgsGHC = import ../../nix/pkgs.nix { inherit compiler chan; isJS = false; };
  client  = pkgsJS.haskell.packages.${compilerjs}.Shpadoinkle-examples + "/bin/servant-crud-client.jsexe/";
  server  = pkgsGHC.haskell.packages.${compiler}.Shpadoinkle-examples  + "/bin/servant-crud-server";
in import ../../nix/docker.nix {
  inherit client server;
  pkgs      = pkgsGHC;
  imgName   = "servant-client-crud-docker";
  extraArgs = "--assets ${client}";
}
