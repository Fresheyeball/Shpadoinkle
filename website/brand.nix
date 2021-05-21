{ chan ? (import ../nix/chan.nix)
}:
let
  pkgs        = import ../nix/pkgs.nix { inherit chan; };
  mkLogo      = import ./logo.nix;
  mkThumbnail = import ./thumbnail.nix;
  colors      = import ./colors.nix;
in import (pkgs.fetchgit {
    url    = https://gitlab.com/fresheyeball/nix-branding.git;
    rev    = "bf9543ff076d1cc921421bfd14c475b53d11806f";
    sha256 = "01j25xnq11x8q4gk7yfwrslk6c0754b8wbyh72havjs9l91a6hdr";
  })
  {
    inherit pkgs mkLogo mkThumbnail colors;
    primary-color    = "dark";
    background-color = "dim_yellow";
    name             = "Shpadoinkle";
    twitterHandle    = "@shpadoinkleUI";
  }
