{ isJS ? false
, optimize ? null }:
import ../default.nix {
  pack = "Shpadoinkle-examples";
  inherit isJS optimize;
}
