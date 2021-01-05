{ compiler ? "ghc864"
, chan ? (import ../../nix/chan.nix)
}:
let
  inherit (import ../../nix/util.nix { inherit compiler; isJS = true; pkgs = pkgsJS; }) compilerjs;
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
