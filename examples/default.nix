{ isJS ? false
, optimize ? true }:
import ../default.nix {
  pack = "Shpadoinkle-examples";
  inherit isJS optimize;
}
