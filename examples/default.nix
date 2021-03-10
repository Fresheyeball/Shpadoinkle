{ isJS ? false
, optimize ? true }:
import ../. {
  pack = "Shpadoinkle-examples";
  inherit isJS optimize;
}
